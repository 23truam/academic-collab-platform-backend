package com.example.academic_collab_platform_backend.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import com.example.academic_collab_platform_backend.dto.ChatMessageResponse;
import com.example.academic_collab_platform_backend.dto.UserListDTO;
import com.example.academic_collab_platform_backend.mapper.ChatMessageMapper;
import com.example.academic_collab_platform_backend.mapper.UserMapper;
import com.example.academic_collab_platform_backend.mapper.UserOnlineStatusMapper;
import com.example.academic_collab_platform_backend.model.ChatMessage;
import com.example.academic_collab_platform_backend.model.User;
import com.example.academic_collab_platform_backend.model.UserOnlineStatus;
import com.example.academic_collab_platform_backend.service.ChatService;
import com.example.academic_collab_platform_backend.util.RedisUtil;
import com.example.academic_collab_platform_backend.mq.ChatMessageProducer;
import com.example.academic_collab_platform_backend.event.ChatMessagePushEvent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.dao.DuplicateKeyException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Collections;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.concurrent.ConcurrentHashMap;

@Service
public class ChatServiceImpl implements ChatService {

    private static final Logger log = LoggerFactory.getLogger(ChatServiceImpl.class);

    @Autowired
    private ChatMessageMapper chatMessageMapper;

    @Autowired
    private UserMapper userMapper;

    @Autowired
    private UserOnlineStatusMapper userOnlineStatusMapper;

    // 注意：此处无需注入 UserService，保留最小依赖

    @Autowired
    private RedisUtil redisUtil;

    @Autowired
    private ChatMessageProducer chatMessageProducer;

    @Autowired
    private ApplicationEventPublisher eventPublisher;

    @Value("${chat.messaging.mode:direct}")
    private String messagingMode;

    // 缓存过期时间：1小时
    private static final long CACHE_EXPIRE_HOURS = 1;

    // 添加用户信息缓存
    private final Map<Long, User> userCache = new ConcurrentHashMap<>();

    private User getUserFromCache(Long userId) {
        return userCache.computeIfAbsent(userId, id -> userMapper.selectById(id));
    }

    @Override
    public ChatMessageResponse sendMessage(Long senderId, ChatMessageRequest request) {
        // 如果是rabbit模式，则发送到MQ队列
        if ("rabbit".equalsIgnoreCase(messagingMode)) {
            log.info("🐰 [ChatService] Using RabbitMQ mode - SenderId: {}, ReceiverId: {}, ClientMsgId: {}", 
                    senderId, request.getReceiverId(), request.getClientMsgId());
            
            // 设置发送者ID到请求中
            request.setSenderId(senderId);
            chatMessageProducer.publish(request);
            
            // 返回已入队响应
            ChatMessageResponse response = new ChatMessageResponse();
            response.setSenderId(senderId);
            response.setReceiverId(request.getReceiverId());
            response.setContent(request.getContent());
            response.setMessageType(request.getMessageType() != null ? request.getMessageType() : "TEXT");
            response.setClientMsgId(request.getClientMsgId());
            response.setCreateTime(LocalDateTime.now());
            
            log.info("✅ [ChatService] Message queued successfully (RabbitMQ mode) - ClientMsgId: {}", request.getClientMsgId());
            return response;
        }
        
        // direct模式：原有的直发逻辑
        log.info("🔄 [ChatService] Using Direct mode - SenderId: {}, ReceiverId: {}, ClientMsgId: {}", 
                senderId, request.getReceiverId(), request.getClientMsgId());
        return processAndDispatchInternal(senderId, request);
    }

    private ChatMessageResponse findByClientMsgId(Long senderId, String clientMsgId) {
        if (clientMsgId == null) return null;
        QueryWrapper<ChatMessage> wrapper = new QueryWrapper<>();
        wrapper.eq("sender_id", senderId).eq("client_msg_id", clientMsgId).last("LIMIT 1");
        ChatMessage found = chatMessageMapper.selectOne(wrapper);
        return found != null ? convertToResponse(found) : null;
    }

    @Override
    public List<ChatMessageResponse> getChatHistory(Long user1Id, Long user2Id, Integer limit) {
        List<ChatMessage> messages = chatMessageMapper.getChatHistory(user1Id, user2Id, limit != null ? limit : 200);
        return messages.stream()
                .map(this::convertToResponse)
                .collect(Collectors.toList());
    }

    @Override
    public Map<String, Object> getChatHistoryWithCache(Long user1Id, Long user2Id, Integer limit, Long loginTime) {
        log.info("🔍 [ChatService] getChatHistoryWithCache called - user1Id: {}, user2Id: {}, limit: {}, loginTime: {}", 
                user1Id, user2Id, limit, loginTime);
        
        // 🔧 修复缓存键生成 - 包含登录时间以避免错误缓存共享
        String cacheKey = generateChatCacheKeyWithTime(user1Id, user2Id, limit, loginTime);

        // 只缓存"登录时刻之前"的历史段。recent 段始终直查数据库，避免缓存频繁更新
        List<ChatMessageResponse> historyMessages = null;
        boolean cacheHit = false;
        
        if (loginTime != null) {
            LocalDateTime loginDateTime = java.time.Instant.ofEpochMilli(loginTime)
                    .atZone(java.time.ZoneId.systemDefault())
                    .toLocalDateTime();
            log.info("📅 [ChatService] Using loginTime for history split: {}", loginDateTime);
            
            // 🔧 临时禁用缓存：为了避免离线消息丢失问题，含loginTime的查询直接走数据库
            // 确保每次都能获取到最新的数据，不受缓存影响
            historyMessages = getMessagesBefore(user1Id, user2Id, limit, loginTime);
            if (historyMessages == null) historyMessages = Collections.emptyList();
            cacheHit = false;
            log.info("📊 [Cache] DISABLED for loginTime query - loaded {} history messages from database", historyMessages.size());
        } else {
            // 无登录时间，查询全部消息（可以使用缓存）
            String simpleCacheKey = generateChatCacheKey(user1Id, user2Id, limit);
            List<ChatMessageResponse> cached = getCachedHistoryMessages(simpleCacheKey);
            if (cached == null) {
                historyMessages = getChatHistory(user1Id, user2Id, limit);
                cacheHistoryMessages(simpleCacheKey, historyMessages);
                cacheHit = false;
                log.info("📊 [Cache] MISS - cached {} total messages", historyMessages.size());
            } else {
                historyMessages = cached;
                cacheHit = true;
                log.info("📊 [Cache] HIT - loaded {} total messages from cache", historyMessages.size());
            }
        }

        // recent 段：> loginTime 的消息，始终走数据库
        List<ChatMessageResponse> recentMessages = getMessagesAfter(user1Id, user2Id, limit, loginTime);

        // 🔍 添加详细的调试信息
        log.info("🔍 [Debug] History messages detail ({} total):", historyMessages.size());
        for (int i = 0; i < Math.min(historyMessages.size(), 3); i++) {
            ChatMessageResponse msg = historyMessages.get(i);
            log.info("  - History[{}]: id={}, content='{}', createTime={}, isRead={}", 
                    i, msg.getId(), msg.getContent(), msg.getCreateTime(), msg.getIsRead());
        }
        
        log.info("🔍 [Debug] Recent messages detail ({} total):", recentMessages.size());
        for (int i = 0; i < Math.min(recentMessages.size(), 3); i++) {
            ChatMessageResponse msg = recentMessages.get(i);
            log.info("  - Recent[{}]: id={}, content='{}', createTime={}, isRead={}", 
                    i, msg.getId(), msg.getContent(), msg.getCreateTime(), msg.getIsRead());
        }

        // 组装结果
        Map<String, Object> result = new java.util.HashMap<>();
        result.put("historyMessages", historyMessages);
        result.put("recentMessages", recentMessages);
        result.put("hasHistoryDivider", !historyMessages.isEmpty() && !recentMessages.isEmpty());
        result.put("cacheHit", cacheHit);
        
        log.info("📋 [ChatService] Result summary - historyMessages: {}, recentMessages: {}, hasHistoryDivider: {}, cacheHit: {}", 
                historyMessages.size(), recentMessages.size(), !historyMessages.isEmpty() && !recentMessages.isEmpty(), cacheHit);
        
        return result;
    }

    // 读取缓存中的最近 limit 条消息
    private List<ChatMessageResponse> getCachedHistoryMessages(String cacheKey) {
        String cachedJson = redisUtil.get(cacheKey);
        if (cachedJson != null) {
            try {
                return redisUtil.getObjectMapper().readValue(
                        cachedJson,
                        redisUtil.getObjectMapper().getTypeFactory().constructCollectionType(List.class, ChatMessageResponse.class)
                );
            } catch (Exception e) {
                redisUtil.delete(cacheKey);
            }
        }
        return null;
    }

    // 查询数据库并转换为响应对象
    private List<ChatMessageResponse> getAllChatMessages(Long user1Id, Long user2Id, Integer limit) {
        List<ChatMessage> allMessages = chatMessageMapper.getChatHistory(user1Id, user2Id, limit != null ? limit : 200);
        return allMessages.stream().map(this::convertToResponse).collect(Collectors.toList());
    }

    private List<ChatMessageResponse> getMessagesBefore(Long user1Id, Long user2Id, Integer limit, Long loginTime) {
        if (loginTime == null) return Collections.emptyList();
        java.time.LocalDateTime loginLocalDateTime = java.time.Instant.ofEpochMilli(loginTime)
                .atZone(java.time.ZoneId.systemDefault())
                .toLocalDateTime();
        
        // 🔧 历史消息查询：使用原始登录时间作为边界（<= loginTime）
        // 不需要调整时间，保持精确的时间边界
        log.info("📊 [ChatService] Querying history messages BEFORE or EQUAL: {}", loginLocalDateTime);
        List<ChatMessage> msgs = chatMessageMapper.getChatHistoryBeforeTime(user1Id, user2Id, loginLocalDateTime, limit != null ? limit : 200);
        log.info("📊 [ChatService] Found {} history messages before/equal loginTime", msgs.size());
        
        return msgs.stream().map(this::convertToResponse).collect(Collectors.toList());
    }

    private List<ChatMessageResponse> getMessagesAfter(Long user1Id, Long user2Id, Integer limit, Long loginTime) {
        if (loginTime == null) return Collections.emptyList();
        java.time.LocalDateTime loginLocalDateTime = java.time.Instant.ofEpochMilli(loginTime)
                .atZone(java.time.ZoneId.systemDefault())
                .toLocalDateTime();
        
        // 🔧 时间精度修复：不再需要时间缓冲，使用精确的时间边界
        // 由于loginTime现在使用与消息创建时间相同的时间源，边界判断准确
        log.info("📊 [ChatService] Querying recent messages AFTER: {} (精确时间边界)", loginLocalDateTime);
        List<ChatMessage> msgs = chatMessageMapper.getChatHistoryAfterTime(user1Id, user2Id, loginLocalDateTime, limit != null ? limit : 200);
        log.info("📊 [ChatService] Found {} recent messages after loginTime", msgs.size());
        
        return msgs.stream().map(this::convertToResponse).collect(Collectors.toList());
    }

    // 按登录时间分为历史和新消息
    private Map<String, List<ChatMessageResponse>> splitHistoryAndRecentMessages(List<ChatMessageResponse> allResponses, Long loginTime) {
        List<ChatMessageResponse> historyMessages = new ArrayList<>();
        List<ChatMessageResponse> recentMessages = new ArrayList<>();
        if (loginTime != null) {
            java.time.LocalDateTime loginLocalDateTime = java.time.Instant.ofEpochMilli(loginTime)
                    .atZone(java.time.ZoneId.systemDefault())
                    .toLocalDateTime();
            for (ChatMessageResponse msg : allResponses) {
                if (msg.getCreateTime() == null || msg.getCreateTime().isBefore(loginLocalDateTime)) {
                    historyMessages.add(msg);
                } else {
                    recentMessages.add(msg);
                }
            }
        } else {
            historyMessages.addAll(allResponses);
        }
        Map<String, List<ChatMessageResponse>> result = new java.util.HashMap<>();
        result.put("history", historyMessages);
        result.put("recent", recentMessages);
        return result;
    }

    // 写入缓存（最近 limit 条消息）
    private void cacheHistoryMessages(String cacheKey, List<ChatMessageResponse> historyMessages) {
        redisUtil.setObject(cacheKey, historyMessages, CACHE_EXPIRE_HOURS, TimeUnit.HOURS);
    }


    @Override
    public void clearChatCache(Long user1Id, Long user2Id) {
        // 清除不同limit的缓存
        String[] limits = {"20", "50", "100", "200"};
        for (String limit : limits) {
            // 兼容历史（未排序）与新规则（排序）两种缓存键
            String oldKey = "chat_history:" + user1Id + ":" + user2Id + ":" + limit;
            String oldKeyReverse = "chat_history:" + user2Id + ":" + user1Id + ":" + limit;
            String newKey = generateChatCacheKey(user1Id, user2Id, Integer.parseInt(limit));
            redisUtil.delete(oldKey);
            redisUtil.delete(oldKeyReverse);
            redisUtil.delete(newKey);
        }
    }

    @Override
    public List<UserListDTO> getUserList(Long currentUserId) {
        // 获取所有用户（除了当前用户）
        QueryWrapper<User> wrapper = new QueryWrapper<>();
        wrapper.ne("id", currentUserId);
        List<User> users = userMapper.selectList(wrapper);

        return users.stream()
                .map(user -> {
                    UserOnlineStatus status = userOnlineStatusMapper.selectById(user.getId());
                    boolean isOnline = status != null && Boolean.TRUE.equals(status.getIsOnline());
                    return new UserListDTO(user.getId(), user.getUsername(), isOnline);
                })
                .collect(Collectors.toList());
    }


    @Override
    public void markMessagesAsRead(Long senderId, Long receiverId) {
        chatMessageMapper.markMessagesAsRead(senderId, receiverId);
    }

    @Override
    public Integer getUnreadMessageCount(Long userId) {
        return chatMessageMapper.getUnreadMessageCount(userId);
    }

    @Override
    public Map<Long,Integer> getUnreadCountMap(Long currentUserId){
        List<Map<String,Object>> list=chatMessageMapper.getUnreadCountMap(currentUserId);
        Map<Long,Integer> result=new HashMap<>();
        for(Map<String,Object> row:list){
            Long senderId=((Number) row.get("sender_id")).longValue();
            Integer count=((Number)row.get("cnt")).intValue();
            result.put(senderId,count);
        }
        return result;
    }

    @Override
    public void updateUserOnlineStatus(Long userId, Boolean isOnline, String sessionId) {
        // 🆕 添加null检查
        if (isOnline == null) {
            System.out.println("⚠️ [ChatService] isOnline参数为null，默认设置为false: userId=" + userId);
            isOnline = false;  // 默认为离线
        }
        
        // 先查询是否已存在记录；注意：不能根据是否设置了 userId 来判断是否为新记录
        UserOnlineStatus existing = userOnlineStatusMapper.selectById(userId);

        UserOnlineStatus status = existing != null ? existing : new UserOnlineStatus();
        status.setUserId(userId);
        status.setIsOnline(isOnline);
        if (Boolean.TRUE.equals(isOnline)) {
            status.setLastLoginTime(LocalDateTime.now());
        }
        status.setSessionId(sessionId);

        if (existing != null) {
            userOnlineStatusMapper.updateById(status);
        } else {
            userOnlineStatusMapper.insert(status);
        }
        // 如果用户下线，记录退出时间
        if (Boolean.FALSE.equals(isOnline)) {
            userOnlineStatusMapper.updateLastLogoutTime(userId);
        }
    }


    private ChatMessageResponse  convertToResponse(ChatMessage message) {
        ChatMessageResponse response = new ChatMessageResponse();
        response.setId(message.getId());
        response.setSenderId(message.getSenderId());
        response.setReceiverId(message.getReceiverId());
        response.setContent(message.getContent());
        response.setMessageType(message.getMessageType());
        response.setIsRead(message.getIsRead());
        response.setCreateTime(message.getCreateTime());
        response.setClientMsgId(message.getClientMsgId());

        // 使用缓存获取用户信息，避免重复查询
        User sender = getUserFromCache(message.getSenderId());
        User receiver = getUserFromCache(message.getReceiverId());
        
        if (sender != null) {
            response.setSenderName(sender.getUsername());
        }
        if (receiver != null) {
            response.setReceiverName(receiver.getUsername());
        }

        return response;
    }

    private String generateChatCacheKey(Long user1Id, Long user2Id, Integer limit) {
        long a = Math.min(user1Id, user2Id);
        long b = Math.max(user1Id, user2Id);
        return "chat_history:" + a + ":" + b + ":" + limit;
    }
    
    // 🆕 新增：包含登录时间的缓存键生成方法
    private String generateChatCacheKeyWithTime(Long user1Id, Long user2Id, Integer limit, Long loginTime) {
        long a = Math.min(user1Id, user2Id);
        long b = Math.max(user1Id, user2Id);
        // 如果有登录时间，包含在缓存键中；否则使用默认键
        if (loginTime != null) {
            // 🔧 修复：使用精确到小时的时间戳，避免同一天多次登录的缓存冲突
            // 按小时划分缓存，既能复用缓存又能避免离线消息丢失
            String loginHour = java.time.Instant.ofEpochMilli(loginTime)
                    .atZone(java.time.ZoneId.systemDefault())
                    .format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd-HH"));
            return "chat_history_time:" + a + ":" + b + ":" + limit + ":" + loginHour;
        } else {
            return generateChatCacheKey(user1Id, user2Id, limit);
        }
    }



    /**
     * 处理并分发消息的内部方法（复用原有逻辑）
     * @param senderId 发送者ID
     * @param request 消息请求
     * @return 消息响应
     */
    private ChatMessageResponse processAndDispatchInternal(Long senderId, ChatMessageRequest request) {
        // 🔧 统一时间处理：使用精确的当前时间，与前端loginTime保持一致
        LocalDateTime currentTime = LocalDateTime.now();
        
        ChatMessage message = ChatMessage.builder()
                .senderId(senderId)
                .receiverId(request.getReceiverId())
                .content(request.getContent())
                .messageType(request.getMessageType() != null ? request.getMessageType() : "TEXT")
                .isRead(false)
                .clientMsgId(request.getClientMsgId())
                .createTime(currentTime)
                .updateTime(currentTime).build();

        // 幂等插入（基于 senderId + clientMsgId 唯一索引）
        try {
            log.info("💾 [Database] 准备插入消息到数据库 - SenderId: {}, ReceiverId: {}, ClientMsgId: {}, Content: {}", 
                    senderId, request.getReceiverId(), request.getClientMsgId(), request.getContent());
            
            int insertResult = chatMessageMapper.insert(message);
            
            log.info("✅ [Database] 消息插入成功 - MessageId: {}, ClientMsgId: {}, InsertResult: {}", 
                    message.getId(), request.getClientMsgId(), insertResult);
            
            // 🔍 验证消息是否真的被存储到数据库中
            try {
                ChatMessageResponse verification = findByClientMsgId(senderId, request.getClientMsgId());
                if (verification != null) {
                    log.info("🔍 [Database] 消息存储验证成功 - MessageId: {}, ClientMsgId: {}, isRead: {}", 
                            verification.getId(), verification.getClientMsgId(), verification.getIsRead());
                } else {
                    log.error("❌ [Database] 消息存储验证失败 - 插入后立即查询不到记录: ClientMsgId: {}", 
                            request.getClientMsgId());
                }
            } catch (Exception e) {
                log.error("❌ [Database] 消息存储验证异常: ClientMsgId: {}, Error: {}", 
                        request.getClientMsgId(), e.getMessage());
            }
                    
        } catch (DuplicateKeyException e) {
            // 如果违反唯一约束，说明是重复发送，忽略插入并查询已存在记录用于返回
            log.warn("🔄 [Database] 检测到重复消息，查询已存在记录 - SenderId: {}, ClientMsgId: {}", 
                    senderId, request.getClientMsgId());
            
            ChatMessageResponse existed = findByClientMsgId(senderId, request.getClientMsgId());
            if (existed != null) {
                log.info("✅ [Database] 找到已存在消息 - MessageId: {}, ClientMsgId: {}", 
                        existed.getId(), request.getClientMsgId());
                return existed;
            }
            
            log.error("❌ [Database] 重复消息但查询不到已存在记录 - SenderId: {}, ClientMsgId: {}", 
                    senderId, request.getClientMsgId());
            throw e;
        } catch (Exception e) {
            log.error("❌ [Database] 消息插入失败 - SenderId: {}, ClientMsgId: {}, Error: {}", 
                    senderId, request.getClientMsgId(), e.getMessage(), e);
            throw e;
        }

        ChatMessageResponse response = convertToResponse(message);
        
        // 通过事件发布推送消息给接收者
        try {
            eventPublisher.publishEvent(new ChatMessagePushEvent(this, request.getReceiverId(), response));
            log.info("📨 [ChatService] Published message push event - ReceiverId: {}, MessageId: {}", 
                    request.getReceiverId(), response.getId());
        } catch (Exception e) {
            log.error("❌ [ChatService] Failed to publish message push event for user {}: {}", 
                    request.getReceiverId(), e.getMessage());
        }

        return response;
    }
    
    @Override
    public UserOnlineStatus getUserOnlineStatus(Long userId) {
        return userOnlineStatusMapper.selectById(userId);
    }
    
    // 🆕 第二阶段：离线消息处理实现
    @Override
    public List<ChatMessageResponse> getOfflineMessages(Long userId, LocalDateTime lastLogoutTime, Integer limit) {
        if (lastLogoutTime == null) {
            log.info("📭 [Offline] 无下线时间记录，跳过查询: userId={}", userId);
            return Collections.emptyList();
        }
        
        // 🆕 第三阶段：性能优化 - 限制查询范围
        if (limit == null || limit <= 0) {
            limit = 100;  // 默认限制
        }
        if (limit > 500) {
            limit = 500;  // 最大限制，防止一次查询过多数据
        }
        
        log.info("🔍 [Offline] 查询离线消息: userId={}, lastLogoutTime={}, limit={}", userId, lastLogoutTime, limit);
        
        try {
            long startTime = System.currentTimeMillis();
            
            // 查询离线期间收到的未读消息
            List<ChatMessage> offlineMessages = chatMessageMapper.getOfflineMessages(userId, lastLogoutTime, limit);
            
            long queryTime = System.currentTimeMillis() - startTime;
            
            log.info("📊 [Offline] 查询完成: userId={}, count={}, queryTime={}ms", userId, offlineMessages.size(), queryTime);
            
            return offlineMessages.stream()
                    .map(this::convertToResponse)
                    .collect(Collectors.toList());
                    
        } catch (Exception e) {
            log.error("❌ [Offline] 查询离线消息失败: userId={}, error={}", userId, e.getMessage(), e);
            return Collections.emptyList();
        }
    }
    
    @Override
    public void processOfflineMessagePull(ChatMessageRequest request) {
        Long userId = request.getSenderId();
        String clientMsgId = request.getClientMsgId();
        long startTime = System.currentTimeMillis();
        
        try {
            log.info("📨 [Offline] 处理离线消息拉取: userId={}, clientMsgId={}, sessionId={}", 
                userId, clientMsgId, request.getSessionId());
            
            // 1. 幂等性检查：防止重复处理
            String dedupKey = "offline_pull:" + clientMsgId;
            Boolean firstTime = redisUtil.setIfAbsent(dedupKey, "1", 1, TimeUnit.HOURS);
            if (Boolean.FALSE.equals(firstTime)) {
                log.info("🔄 [Offline] 离线消息拉取已处理过，跳过: userId={}, clientMsgId={}", userId, clientMsgId);
                return;
            }
            
            // 2. 解析下线时间
            LocalDateTime lastLogoutTime = LocalDateTime.parse(request.getContent());
            
            // 3. 查询离线消息（使用请求中的批量大小）
            List<ChatMessageResponse> offlineMessages = getOfflineMessages(userId, lastLogoutTime, request.getBatchSize());
            
            if (offlineMessages.isEmpty()) {
                log.info("📭 [Offline] 无离线消息: userId={}", userId);
                return;
            }
            
            log.info("📤 [Offline] 开始推送离线消息: userId={}, count={}", userId, offlineMessages.size());
            
            // 4. 🆕 第三阶段：批量推送优化
            int successCount = 0;
            int failCount = 0;
            
            for (ChatMessageResponse message : offlineMessages) {
                try {
                    // 🆕 保持原始消息类型，通过特殊字段标识离线消息
                    // 不修改messageType，前端根据推送队列区分离线消息
                    
                    // 🎯 复用现有的推送事件机制
                    eventPublisher.publishEvent(new ChatMessagePushEvent(this, userId, message));
                    
                    successCount++;
                    log.debug("📬 [Offline] 推送离线消息: userId={}, messageId={}, originalType={}", 
                            userId, message.getId(), message.getMessageType());
                    
                } catch (Exception e) {
                    failCount++;
                    log.error("❌ [Offline] 离线消息推送失败: userId={}, messageId={}, error={}", 
                        userId, message.getId(), e.getMessage());
                }
            }
            
            // 5. 🆕 第三阶段：记录处理统计
            long processingTime = System.currentTimeMillis() - startTime;
            String statsKey = "offline_stats:" + userId;
            Map<String, Object> stats = new HashMap<>();
            stats.put("lastPullTime", System.currentTimeMillis());
            stats.put("messageCount", offlineMessages.size());
            stats.put("successCount", successCount);
            stats.put("failCount", failCount);
            stats.put("processingTimeMs", processingTime);
            
            redisUtil.setObject(statsKey, stats, 24, TimeUnit.HOURS);
            
            log.info("✅ [Offline] 离线消息拉取处理完成: userId={}, total={}, success={}, failed={}, time={}ms", 
                userId, offlineMessages.size(), successCount, failCount, processingTime);
                
        } catch (Exception e) {
            long errorTime = System.currentTimeMillis() - startTime;
            log.error("❌ [Offline] 离线消息拉取处理失败: userId={}, clientMsgId={}, time={}ms, error={}", 
                userId, clientMsgId, errorTime, e.getMessage(), e);
        }
    }
    
    // 🆕 修改现有的processAndDispatch方法，支持离线消息处理
    @Override
    public void processAndDispatch(ChatMessageRequest request) {
        if (request.isOfflinePullRequest()) {
            // 🆕 处理离线消息拉取
            log.info("📮 [ChatService] Processing offline pull request - UserId: {}, ClientMsgId: {}", 
                request.getSenderId(), request.getClientMsgId());
            processOfflineMessagePull(request);
        } else {
            // 原有的普通消息处理逻辑
            log.info("📮 [ChatService] Processing normal message from MQ - SenderId: {}, ReceiverId: {}, ClientMsgId: {}", 
                request.getSenderId(), request.getReceiverId(), request.getClientMsgId());
            
            ChatMessageResponse response = processAndDispatchInternal(request.getSenderId(), request);
            
            log.info("✅ [ChatService] Message processed and dispatched successfully from MQ - MessageId: {}, ClientMsgId: {}", 
                response.getId(), request.getClientMsgId());
        }
    }
    
    // 🆕 第三阶段：监控和性能优化实现
    @Override
    public Map<String, Object> getOfflineMessageStats(Long userId) {
        try {
            String statsKey = "offline_stats:" + userId;
            String cachedStats = redisUtil.get(statsKey);
            
            if (cachedStats != null) {
                return redisUtil.getObjectMapper().readValue(cachedStats, Map.class);
            }
            
            // 如果没有缓存统计，返回默认值
            Map<String, Object> defaultStats = new HashMap<>();
            defaultStats.put("lastPullTime", 0L);
            defaultStats.put("messageCount", 0);
            defaultStats.put("successCount", 0);
            defaultStats.put("failCount", 0);
            defaultStats.put("processingTimeMs", 0L);
            
            return defaultStats;
            
        } catch (Exception e) {
            log.error("❌ [Stats] 获取离线消息统计失败: userId={}, error={}", userId, e.getMessage());
            return Collections.emptyMap();
        }
    }
    
    @Override
    public void batchMarkMessagesAsRead(Long userId, List<Long> messageIds) {
        if (messageIds == null || messageIds.isEmpty()) {
            return;
        }
        
        try {
            // 🆕 批量更新消息为已读状态
            int batchSize = 50;  // 每批次处理50条
            for (int i = 0; i < messageIds.size(); i += batchSize) {
                int endIndex = Math.min(i + batchSize, messageIds.size());
                List<Long> batchIds = messageIds.subList(i, endIndex);
                
                chatMessageMapper.batchMarkAsRead(userId, batchIds);
                log.debug("📖 [BatchRead] 批量标记已读: userId={}, count={}", userId, batchIds.size());
            }
            
            log.info("✅ [BatchRead] 批量标记完成: userId={}, totalCount={}", userId, messageIds.size());
            
        } catch (Exception e) {
            log.error("❌ [BatchRead] 批量标记失败: userId={}, count={}, error={}", 
                userId, messageIds.size(), e.getMessage(), e);
        }
    }
} 