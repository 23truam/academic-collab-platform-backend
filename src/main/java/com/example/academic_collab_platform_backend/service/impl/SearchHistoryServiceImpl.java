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
import org.springframework.stereotype.Service;
import com.fasterxml.jackson.databind.ObjectMapper;
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
 * 优化：使用String存储，支持高并发
 */
@Service
public class SearchHistoryServiceImpl implements SearchHistoryService {

    @Autowired
    private SearchHistoryMapper searchHistoryMapper;

    @Autowired
    private RedisUtil redisUtil;

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
     */
    @Override
    public void clearHistory(String userId) {
        QueryWrapper<SearchHistory> wrapper = new QueryWrapper<>();
        wrapper.lambda().eq(SearchHistory::getUserId, userId);
        searchHistoryMapper.delete(wrapper);
        
        // 清除Redis缓存
        String cacheKey = RECENT_SEARCH_PREFIX + userId;
        redisUtil.delete(cacheKey);
    }

    /**
     * 添加一条搜索历史（写入MySQL和Redis缓存）。
     * 优化：使用String存储，支持高并发
     */
    @Override
    public void addSearchHistory(SearchHistory history) {
        // 1. 写入 MySQL（异步或同步，根据业务需求）
        searchHistoryMapper.insert(history);
        
        // 2. 更新 Redis 缓存
        updateRecentSearchCache(history);
    }

    /**
     * 获取指定用户的最近搜索历史（优先查Redis，无则查MySQL并回填Redis）。
     * 优化：使用String存储，支持高并发
     */
    @Override
    public List<SearchHistory> getRecentSearchHistory(String userId, int limit) {
        String cacheKey = RECENT_SEARCH_PREFIX + userId;
        
        // 1. 尝试从Redis获取
        String cachedData = redisUtil.get(cacheKey);
        if (cachedData != null && !cachedData.isEmpty()) {
            try {
                return parseSearchHistoryList(cachedData);
            } catch (Exception e) {
                // 缓存数据损坏，删除缓存
                redisUtil.delete(cacheKey);
            }
        }
        
        // 2. Redis没有数据，从MySQL获取并回填缓存
        return getFromDatabaseAndCache(userId, limit, cacheKey);
    }



    /**
     * 更新最近搜索缓存（使用String存储）
     */
    private void updateRecentSearchCache(SearchHistory history) {
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
                    
                } finally {
                    // 释放锁
                    redisUtil.releaseLock(lockKey, lockValue);
                }
            } else {
                // 获取锁失败，使用本地锁作为备选方案
                updateCacheWithLocalLock(history, cacheKey, lockKey);
            }
        } catch (Exception e) {
            // 异常处理，记录日志但不影响主流程
            e.printStackTrace();
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
                    updateRecentSearchCache(history);
                } finally {
                    redisUtil.releaseLock(lockKey, lockValue);
                }
            }
        } finally {
            localLock.unlock();
        }
    }

    /**
     * 从数据库获取数据并缓存
     */
    private List<SearchHistory> getFromDatabaseAndCache(String userId, int limit, String cacheKey) {
        String lockKey = SEARCH_LOCK_PREFIX + userId;
        String lockValue = UUID.randomUUID().toString();
        
        try {
            // 使用RedisUtil的分布式锁
            boolean lockAcquired = redisUtil.tryLock(lockKey, lockValue, LOCK_TIMEOUT, TimeUnit.SECONDS);
            
            if (lockAcquired) {
                try {
                    // 双重检查，防止其他线程已经更新了缓存
                    String cachedData = redisUtil.get(cacheKey);
                    if (cachedData != null && !cachedData.isEmpty()) {
                        return parseSearchHistoryList(cachedData);
                    }
                    
                    // 从数据库查询
                    List<SearchHistory> dbList = searchHistoryMapper.selectList(
                        new QueryWrapper<SearchHistory>()
                            .eq("user_id", userId)
                            .orderByDesc("timestamp")
                            .last("LIMIT " + limit)
                    );
                    
                    // 缓存数据
                    if (!dbList.isEmpty()) {
                        redisUtil.setObject(cacheKey, dbList, CACHE_EXPIRE_TIME, TimeUnit.SECONDS);
                    }
                    
                    return dbList;
                } finally {
                    redisUtil.releaseLock(lockKey, lockValue);
                }
            } else {
                // 获取锁失败，直接查询数据库
                return searchHistoryMapper.selectList(
                    new QueryWrapper<SearchHistory>()
                        .eq("user_id", userId)
                        .orderByDesc("timestamp")
                        .last("LIMIT " + limit)
                );
            }
        } catch (Exception e) {
            // 异常处理，直接查询数据库
            e.printStackTrace();
            return searchHistoryMapper.selectList(
                new QueryWrapper<SearchHistory>()
                    .eq("user_id", userId)
                    .orderByDesc("timestamp")
                    .last("LIMIT " + limit)
            );
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
                e.printStackTrace();
            }
        });
    }
} 