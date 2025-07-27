package com.example.academic_collab_platform_backend.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.example.academic_collab_platform_backend.mapper.SearchHistoryMapper;
import com.example.academic_collab_platform_backend.model.SearchHistory;
import com.example.academic_collab_platform_backend.service.SearchHistoryService;
import com.example.academic_collab_platform_backend.util.RedisUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.task.AsyncTaskExecutor;
import org.springframework.stereotype.Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;


import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;
import java.util.UUID;

/**
 * 搜索历史相关业务实现类，负责搜索历史的增删查、缓存等具体业务逻辑。
 * 优化：使用异步处理，提升性能
 */
@Service
@Slf4j
public class SearchHistoryServiceImpl implements SearchHistoryService {

    @Autowired
    private SearchHistoryMapper searchHistoryMapper;

    @Autowired
    private RedisUtil redisUtil;

    @Autowired
    @Qualifier("searchHistoryTaskExecutor")
    private AsyncTaskExecutor taskExecutor;
    
    // 用户消息队列前缀
    private static final String USER_QUEUE_PREFIX = "user_search_queue:";

    private static final String RECENT_SEARCH_PREFIX = "recent_search:";
    private static final String SEARCH_LOCK_PREFIX = "search_lock:";
    private static final int RECENT_SEARCH_LIMIT = 5;
    private static final int LOCK_TIMEOUT = 10; // 锁超时时间（秒）
    private static final int CACHE_EXPIRE_TIME = 3600; // 缓存过期时间（秒）
    
    private final ObjectMapper objectMapper = new ObjectMapper();
    
    // 本地锁，用于防止同一用户并发操作
    private final Lock localLock = new ReentrantLock();

    /**
     * 分页获取指定用户的搜索历史。
     */
    @Override
    public IPage<SearchHistory> getHistory(String userId,int page,int size){
        QueryWrapper<SearchHistory> wrapper =new QueryWrapper<>();
        wrapper.lambda().eq(SearchHistory::getUserId,userId).orderByDesc(SearchHistory::getTimestamp);
        return searchHistoryMapper.selectPage(new Page<>(page,size),wrapper);
    }

    /**
     * 添加一条搜索历史。
     */
    @Override
    public void addHistory(SearchHistory item) {
        searchHistoryMapper.insert(item);
    }

    /**
     * 清空指定用户的搜索历史。
     * 使用延迟双删策略保证缓存一致性
     */
    @Override
    public void clearHistory(String userId) {
        String cacheKey = RECENT_SEARCH_PREFIX + userId;
        String queueKey = USER_QUEUE_PREFIX + userId;
        
        try {
            // 1. 第一次删除缓存（立即删除）
            redisUtil.delete(cacheKey);
            redisUtil.delete(queueKey);
            log.debug("用户{}搜索历史第一次删除缓存完成", userId);
            
            // 2. 删除数据库
            QueryWrapper<SearchHistory> wrapper = new QueryWrapper<>();
            wrapper.lambda().eq(SearchHistory::getUserId, userId);
            int deletedCount = searchHistoryMapper.delete(wrapper);
            log.info("用户{}搜索历史数据库删除完成，删除{}条记录", userId, deletedCount);
            
            // 3. 延迟删除缓存（防止并发问题）
            scheduleDelayedDelete(userId, cacheKey, queueKey);
            
        } catch (Exception e) {
            log.error("用户{}搜索历史清除失败", userId, e);
            throw new RuntimeException("清空搜索历史失败", e);
        }
    }
    
    /**
     * 调度延迟删除
     * 延迟500ms后再次删除缓存，防止并发读取问题
     */
    private void scheduleDelayedDelete(String userId, String cacheKey, String queueKey) {
        taskExecutor.execute(() -> {
            try {
                // 延迟500ms，等待可能的并发读取完成
                Thread.sleep(500);
                
                // 再次删除缓存
                redisUtil.delete(cacheKey);
                redisUtil.delete(queueKey);
                
                log.debug("用户{}搜索历史延迟双删完成", userId);
                
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.warn("用户{}延迟删除被中断", userId);
            } catch (Exception e) {
                log.error("用户{}延迟删除失败", userId, e);
            }
        });
    }

    /**
     * 添加一条搜索历史（写入MySQL和消息队列）。
     * 优化：使用消息队列保证同一用户消息的顺序处理
     */
    @Override
    public void addSearchHistory(SearchHistory history) {
        // 1. 立即写入 MySQL（同步）
        searchHistoryMapper.insert(history);
        
        // 2. 将消息加入用户专属队列（保证顺序）
        String queueKey = USER_QUEUE_PREFIX + history.getUserId();
        try {
            String messageJson = objectMapper.writeValueAsString(history);
            redisUtil.lpush(queueKey, messageJson);
            
            // 3. 异步处理队列（不阻塞用户请求）
            taskExecutor.execute(() -> {
                try {
                    processUserSearchQueue(history.getUserId());
                } catch (Exception e) {
                    log.error("处理用户{}搜索队列失败", history.getUserId(), e);
                }
            });
        } catch (Exception e) {
            log.error("添加搜索历史到队列失败，userId: {}, type: {}", 
                history.getUserId(), history.getType(), e);
        }
    }

    /**
     * 处理用户搜索队列
     * 保证同一用户消息的顺序处理
     */
    private void processUserSearchQueue(String userId) {
        String queueKey = USER_QUEUE_PREFIX + userId;
        String lockKey = SEARCH_LOCK_PREFIX + userId;
        String lockValue = UUID.randomUUID().toString();
        
        try {
            // 获取分布式锁
            boolean lockAcquired = redisUtil.tryLock(lockKey, lockValue, LOCK_TIMEOUT, TimeUnit.SECONDS);
            
            if (lockAcquired) {
                try {
                    // 处理队列中的所有消息
                    List<SearchHistory> messages = new ArrayList<>();
                    for (int i = 0; i < 10; i++) { // 每次处理最多10条
                        String messageJson = redisUtil.rpop(queueKey);
                        if (messageJson != null) {
                            try {
                                SearchHistory history = objectMapper.readValue(messageJson, SearchHistory.class);
                                messages.add(history);
                            } catch (Exception e) {
                                log.warn("解析队列消息失败: {}", messageJson, e);
                            }
                        } else {
                            break;
                        }
                    }
                    
                    // 按时间戳排序并更新缓存
                    if (!messages.isEmpty()) {
                        updateCacheWithBatch(userId, messages);
                        log.info("批量处理用户{}搜索历史成功，处理{}条消息", userId, messages.size());
                    }
                } finally {
                    redisUtil.releaseLock(lockKey, lockValue);
                }
            } else {
                log.warn("获取用户{}锁失败，跳过队列处理", userId);
            }
        } catch (Exception e) {
            log.error("处理用户{}搜索队列失败", userId, e);
        }
    }
    
    /**
     * 批量更新缓存
     */
    private void updateCacheWithBatch(String userId, List<SearchHistory> messages) {
        String cacheKey = RECENT_SEARCH_PREFIX + userId;
        
        try {
            // 获取当前缓存
            List<SearchHistory> currentList = getCurrentCache(userId);
            
            // 按时间戳排序
            messages.sort((a, b) -> Long.compare(a.getTimestamp(), b.getTimestamp()));
            
            // 合并历史记录
            List<SearchHistory> mergedList = new ArrayList<>();
            mergedList.addAll(messages);
            mergedList.addAll(currentList);
            
            // 去重（基于ID）
            mergedList = mergedList.stream()
                .collect(Collectors.toMap(
                    SearchHistory::getId,
                    history -> history,
                    (existing, replacement) -> existing
                ))
                .values()
                .stream()
                .sorted((a, b) -> Long.compare(b.getTimestamp(), a.getTimestamp()))
                .collect(Collectors.toList());
            
            // 保持限制数量
            if (mergedList.size() > RECENT_SEARCH_LIMIT) {
                mergedList = mergedList.subList(0, RECENT_SEARCH_LIMIT);
            }
            
            // 更新缓存
            redisUtil.setObject(cacheKey, mergedList, CACHE_EXPIRE_TIME, TimeUnit.SECONDS);
            
            log.debug("用户{}缓存更新成功，最终记录数: {}", userId, mergedList.size());
            
        } catch (Exception e) {
            log.error("更新用户{}缓存失败", userId, e);
        }
    }
    
    /**
     * 获取当前缓存
     */
    private List<SearchHistory> getCurrentCache(String userId) {
        String cacheKey = RECENT_SEARCH_PREFIX + userId;
        String cachedData = redisUtil.get(cacheKey);
        
        if (cachedData != null && !cachedData.isEmpty()) {
            try {
                return objectMapper.readValue(
                    cachedData,
                    objectMapper.getTypeFactory().constructCollectionType(List.class, SearchHistory.class)
                );
            } catch (Exception e) {
                log.warn("解析用户{}缓存数据失败", userId, e);
                redisUtil.delete(cacheKey);
            }
        }
        
        return new ArrayList<>();
    }

    /**
     * 异步更新最近搜索缓存（保留原有方法，用于兼容）
     */
    private void updateRecentSearchCacheAsync(SearchHistory history) {
        String cacheKey = RECENT_SEARCH_PREFIX + history.getUserId();
        String lockKey = SEARCH_LOCK_PREFIX + history.getUserId();
        String lockValue = UUID.randomUUID().toString();
        
        try {
            // 使用RedisUtil的分布式锁
            boolean lockAcquired = redisUtil.tryLock(lockKey, lockValue, LOCK_TIMEOUT, TimeUnit.SECONDS);
            
            if (lockAcquired) {
                try {
                    // 获取当前缓存数据
                    String currentData = redisUtil.get(cacheKey);
                    List<SearchHistory> historyList = new ArrayList<>();
                    
                    if (currentData != null && !currentData.isEmpty()) {
                        try {
                            historyList = parseSearchHistoryList(currentData);
                        } catch (Exception e) {
                            // 数据损坏，重新开始
                            historyList = new ArrayList<>();
                            log.warn("缓存数据损坏，重新初始化，userId: {}", history.getUserId());
                        }
                    }
                    
                    // 添加新记录到开头
                    historyList.add(0, history);
                    
                    // 保持限制数量
                    if (historyList.size() > RECENT_SEARCH_LIMIT) {
                        historyList = historyList.subList(0, RECENT_SEARCH_LIMIT);
                    }
                    
                    // 使用RedisUtil存储
                    redisUtil.setObject(cacheKey, historyList, CACHE_EXPIRE_TIME, TimeUnit.SECONDS);
                    
                    log.debug("异步更新搜索历史缓存成功，userId: {}, 缓存大小: {}", 
                        history.getUserId(), historyList.size());
                    
                } finally {
                    // 释放锁
                    redisUtil.releaseLock(lockKey, lockValue);
                }
            } else {
                // 获取锁失败，使用本地锁作为备选方案
                updateCacheWithLocalLock(history, cacheKey, lockKey);
            }
        } catch (Exception e) {
            log.error("异步更新搜索历史缓存异常，userId: {}", history.getUserId(), e);
            // 异常处理，记录日志但不影响主流程
        }
    }

    /**
     * 获取指定用户的最近搜索历史（优先查Redis，无则查MySQL并回填Redis）。
     * 优化：使用异步处理，提升性能
     */
    @Override
    public List<SearchHistory> getRecentSearchHistory(String userId, int limit) {
        String cacheKey = RECENT_SEARCH_PREFIX + userId;
        
        // 1. 尝试从Redis获取
        String cachedData = redisUtil.get(cacheKey);
        if (cachedData != null && !cachedData.isEmpty()) {
            try {
                List<SearchHistory> result = parseSearchHistoryList(cachedData);
                log.debug("从缓存获取搜索历史成功，userId: {}, 数量: {}", userId, result.size());
                return result;
            } catch (Exception e) {
                // 缓存数据损坏，删除缓存
                redisUtil.delete(cacheKey);
                log.warn("缓存数据损坏，已删除，userId: {}", userId);
            }
        }
        
        // 2. Redis没有数据，从MySQL获取
        List<SearchHistory> dbResult = getFromDatabase(userId, limit);
        
        // 3. 异步回填缓存（不阻塞用户请求）
        if (!dbResult.isEmpty()) {
            taskExecutor.execute(() -> {
                try {
                    updateCacheAsync(userId, dbResult);
                } catch (Exception e) {
                    log.error("异步回填搜索历史缓存失败，userId: {}", userId, e);
                }
            });
        }
        
        return dbResult;
    }

    /**
     * 从数据库获取搜索历史
     */
    private List<SearchHistory> getFromDatabase(String userId, int limit) {
        return searchHistoryMapper.selectList(
            new QueryWrapper<SearchHistory>()
                .eq("user_id", userId)
                .orderByDesc("timestamp")
                .last("LIMIT " + limit)
        );
    }

    /**
     * 异步更新缓存
     */
    private void updateCacheAsync(String userId, List<SearchHistory> historyList) {
        String cacheKey = RECENT_SEARCH_PREFIX + userId;
        String lockKey = SEARCH_LOCK_PREFIX + userId;
        String lockValue = UUID.randomUUID().toString();
        
        try {
            boolean lockAcquired = redisUtil.tryLock(lockKey, lockValue, LOCK_TIMEOUT, TimeUnit.SECONDS);
            
            if (lockAcquired) {
                try {
                    redisUtil.setObject(cacheKey, historyList, CACHE_EXPIRE_TIME, TimeUnit.SECONDS);
                    log.debug("异步回填搜索历史缓存成功，userId: {}, 数量: {}", userId, historyList.size());
                } finally {
                    redisUtil.releaseLock(lockKey, lockValue);
                }
            }
        } catch (Exception e) {
            log.error("异步回填缓存异常，userId: {}", userId, e);
        }
    }

    /**
     * 使用本地锁更新缓存（备选方案）
     */
    private void updateCacheWithLocalLock(SearchHistory history, String cacheKey, String lockKey) {
        localLock.lock();
        try {
            // 重新尝试获取分布式锁
            String lockValue = UUID.randomUUID().toString();
            boolean lockAcquired = redisUtil.tryLock(lockKey, lockValue, LOCK_TIMEOUT, TimeUnit.SECONDS);
            
            if (lockAcquired) {
                try {
                    updateRecentSearchCacheAsync(history);
                } finally {
                    redisUtil.releaseLock(lockKey, lockValue);
                }
            }
        } finally {
            localLock.unlock();
        }
    }

    /**
     * 解析搜索历史列表
     */
    private List<SearchHistory> parseSearchHistoryList(String jsonData) throws JsonProcessingException {
        return objectMapper.readValue(jsonData, 
            objectMapper.getTypeFactory().constructCollectionType(List.class, SearchHistory.class));
    }

    /**
     * 批量更新搜索历史缓存（用于数据迁移或批量操作）
     */
    public void batchUpdateCache(List<SearchHistory> histories) {
        if (histories == null || histories.isEmpty()) {
            return;
        }
        
        // 按用户ID分组
        Map<String, List<SearchHistory>> userGroups = histories.stream()
            .collect(Collectors.groupingBy(SearchHistory::getUserId));
        
        // 批量更新每个用户的缓存
        userGroups.forEach((userId, userHistories) -> {
            String cacheKey = RECENT_SEARCH_PREFIX + userId;
            try {
                redisUtil.setObject(cacheKey, userHistories, CACHE_EXPIRE_TIME, TimeUnit.SECONDS);
            } catch (Exception e) {
                log.error("批量更新用户{}缓存失败", userId, e);
            }
        });
    }
} 