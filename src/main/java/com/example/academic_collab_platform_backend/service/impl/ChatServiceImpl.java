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
import com.example.academic_collab_platform_backend.util.UserContextUtil;
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

    @Autowired
    private RedisUtil redisUtil;
    
    @Autowired
    private UserContextUtil userContextUtil;

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
        String cacheKey = generateChatCacheKey(user1Id, user2Id, limit);
        
        List<ChatMessageResponse> historyMessages = Collections.emptyList();
        List<ChatMessageResponse> recentMessages;
        boolean cacheHit = false;

        // 🔄 使用用户最后退出时间分割消息：退出前的为历史消息，退出后的为新消息
        if (loginTime != null) {
            log.info("🕰️ [ChatService] 开始查询用户的最后退出时间用于消息分割");
            
            // 获取当前用户ID
            Long currentUserId = userContextUtil.getCurrentUserId();
            if (currentUserId == null) {
                log.warn("⚠️ [ChatService] 无法获取当前用户ID，使用登录时间分割");
                log.info("🕰️ [ChatService] 使用登录时间分割消息 - loginTime: {} ({})", 
                    loginTime, java.time.Instant.ofEpochMilli(loginTime).atZone(java.time.ZoneId.systemDefault()));
            } else {
                // 查询用户的最后退出时间
                UserOnlineStatus userStatus = userOnlineStatusMapper.selectById(currentUserId);
                java.time.LocalDateTime lastLogoutTime = userStatus != null && userStatus.getLastLogoutTime() != null ? 
                    userStatus.getLastLogoutTime() : 
                    java.time.Instant.ofEpochMilli(loginTime).atZone(java.time.ZoneId.systemDefault()).toLocalDateTime();
                
                long logoutTimeMillis = lastLogoutTime.atZone(java.time.ZoneId.systemDefault()).toInstant().toEpochMilli();
                log.info("🕰️ [ChatService] 使用最后退出时间分割消息 - lastLogoutTime: {} ({}), loginTime: {} ({})", 
                    logoutTimeMillis, lastLogoutTime,
                    loginTime, java.time.Instant.ofEpochMilli(loginTime).atZone(java.time.ZoneId.systemDefault()));
                
                // 使用退出时间替换登录时间
                loginTime = logoutTimeMillis;
            }
            
            List<ChatMessageResponse> cached = getCachedHistoryMessages(cacheKey);
            if (cached == null) {
                // 回源：查询退出时间之前的历史消息
                historyMessages = getMessagesBefore(user1Id, user2Id, limit, loginTime);
                if (historyMessages == null) historyMessages = Collections.emptyList();
                cacheHistoryMessages(cacheKey, historyMessages);
                log.info("📋 [ChatService] 历史消息数量: {}", historyMessages.size());
                log.debug("Chat history cache MISS: key={}", cacheKey);
            } else {
                historyMessages = cached;
                cacheHit = true;
                log.debug("Chat history cache HIT: key={}", cacheKey);
            }

            // 💾 新消息：直接从MySQL查询退出后的消息
            log.info("💾 [MySQL] 查询退出后的新消息...");
            recentMessages = getMessagesAfter(user1Id, user2Id, limit, loginTime);
            log.info("📋 [ChatService] 新消息数量: {}", recentMessages.size());
            
            // 🔍 添加详细的消息时间调试
            if (!recentMessages.isEmpty()) {
                log.info("🔍 [ChatService] 新消息时间范围: {} -> {}", 
                    recentMessages.get(0).getCreateTime(),
                    recentMessages.get(recentMessages.size()-1).getCreateTime());
            }
            if (!historyMessages.isEmpty()) {
                log.info("🔍 [ChatService] 历史消息时间范围: {} -> {}", 
                    historyMessages.get(0).getCreateTime(),
                    historyMessages.get(historyMessages.size()-1).getCreateTime());
            }
        } else {
            // 无登录时间，不进行分割
            log.info("💬 [ChatService] 无登录时间，使用统一消息列表");
            historyMessages = Collections.emptyList();
            recentMessages = getAllChatMessages(user1Id, user2Id, limit);
        }



        // 组装结果
        Map<String, Object> result = new java.util.HashMap<>();
        result.put("historyMessages", historyMessages);
        result.put("recentMessages", recentMessages);
        result.put("hasHistoryDivider", !historyMessages.isEmpty() && !recentMessages.isEmpty());
        result.put("cacheHit", cacheHit);
        result.put("loginTime", loginTime); // 🆕 返回登录时间用于调试
        return result;
    }

    // 读取缓存中的历史消息（登录前的消息，按时间正序）
    private List<ChatMessageResponse> getCachedHistoryMessages(String cacheKey) {
        String cachedJson = redisUtil.get(cacheKey);
        if (cachedJson != null) {
            try {
                List<ChatMessageResponse> cached = redisUtil.getObjectMapper().readValue(
                        cachedJson,
                        redisUtil.getObjectMapper().getTypeFactory().constructCollectionType(List.class, ChatMessageResponse.class)
                );
                // 🔍 添加调试日志
                if (cached != null && !cached.isEmpty()) {
                    log.info("📖 [Cache] 读取历史消息缓存: key={}, count={}, 时间范围: {} -> {}", 
                        cacheKey, cached.size(),
                        cached.get(0).getCreateTime(),
                        cached.get(cached.size()-1).getCreateTime());
                }
                return cached;
            } catch (Exception e) {
                log.warn("⚠️ [Cache] 缓存数据异常，删除: key={}, error={}", cacheKey, e.getMessage());
                redisUtil.delete(cacheKey);
            }
        }
        return null;
    }

    // 查询数据库并转换为响应对象
    private List<ChatMessageResponse> getAllChatMessages(Long user1Id, Long user2Id, Integer limit) {
        List<ChatMessage> allMessages = chatMessageMapper.getChatHistory(user1Id, user2Id, limit != null ? limit : 200);
        log.info("💾 [MySQL] getAllChatMessages查询结果: user1Id={}, user2Id={}, limit={}, 返回{}条消息", 
                user1Id, user2Id, limit != null ? limit : 200, allMessages.size());
        return allMessages.stream().map(this::convertToResponse).collect(Collectors.toList());
    }

    private List<ChatMessageResponse> getMessagesBefore(Long user1Id, Long user2Id, Integer limit, Long loginTime) {
        if (loginTime == null) return Collections.emptyList();
        java.time.LocalDateTime loginLocalDateTime = java.time.Instant.ofEpochMilli(loginTime)
                .atZone(java.time.ZoneId.systemDefault())
                .toLocalDateTime();
        List<ChatMessage> msgs = chatMessageMapper.getChatHistoryBeforeTime(user1Id, user2Id, loginLocalDateTime, limit != null ? limit : 200);
        log.info("💾 [MySQL] getMessagesBefore查询结果: user1Id={}, user2Id={}, beforeTime={}, limit={}, 返回{}条消息", 
                user1Id, user2Id, loginLocalDateTime, limit != null ? limit : 200, msgs.size());
        return msgs.stream().map(this::convertToResponse).collect(Collectors.toList());
    }

    private List<ChatMessageResponse> getMessagesAfter(Long user1Id, Long user2Id, Integer limit, Long splitTime) {
        if (splitTime == null) return Collections.emptyList();
        java.time.LocalDateTime splitLocalDateTime = java.time.Instant.ofEpochMilli(splitTime)
                .atZone(java.time.ZoneId.systemDefault())
                .toLocalDateTime();
        
        // 🔍 先查询所有消息用于调试
        List<ChatMessage> allMsgs = chatMessageMapper.getChatHistory(user1Id, user2Id, limit != null ? limit : 200);
        log.info("🔍 [Debug] 所有消息数量: {}, 分割时间: {}", allMsgs.size(), splitLocalDateTime);
        
        // 显示最近的几条消息时间
        if (!allMsgs.isEmpty()) {
            int showCount = Math.min(5, allMsgs.size());
            log.info("🔍 [Debug] 最近{}条消息时间:", showCount);
            for (int i = allMsgs.size() - showCount; i < allMsgs.size(); i++) {
                ChatMessage msg = allMsgs.get(i);
                boolean isAfterSplit = msg.getCreateTime().isAfter(splitLocalDateTime);
                log.info("  - 消息{}: {} [{}] (分割后: {})", msg.getId(), msg.getCreateTime(), msg.getContent(), isAfterSplit);
            }
        }
        
        // 查询分割时间后的消息
        List<ChatMessage> msgs = chatMessageMapper.getChatHistoryAfterTime(user1Id, user2Id, splitLocalDateTime, limit != null ? limit : 200);
        log.info("💾 [MySQL] getMessagesAfter查询结果: user1Id={}, user2Id={}, afterTime={}, limit={}, 返回{}条消息", 
                user1Id, user2Id, splitLocalDateTime, limit != null ? limit : 200, msgs.size());
        
        if (!msgs.isEmpty()) {
            log.info("🔍 [Debug] 查询到的分割后消息:");
            for (ChatMessage msg : msgs) {
                log.info("  - 消息{}: {} [{}]", msg.getId(), msg.getCreateTime(), msg.getContent());
            }
        }
        
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

    // 写入缓存（登录前的历史消息，按时间正序）
    private void cacheHistoryMessages(String cacheKey, List<ChatMessageResponse> historyMessages) {
        // 🔍 添加调试日志
        if (!historyMessages.isEmpty()) {
            log.info("💾 [Cache] 保存历史消息缓存: key={}, count={}, 时间范围: {} -> {}", 
                cacheKey, historyMessages.size(),
                historyMessages.get(0).getCreateTime(),
                historyMessages.get(historyMessages.size()-1).getCreateTime());
        }
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
            log.info("🗑️ [Cache] 清除聊天缓存: {}, {}, {}", oldKey, oldKeyReverse, newKey);
        }
    }
    
    /**
     * 🧹 清除所有聊天缓存（临时调试用）
     * 用于解决缓存顺序问题，强制重新生成所有缓存
     */
    public void clearAllChatCache() {
        try {
            // 手动清除常见的聊天缓存键
            String[] limits = {"20", "50", "100", "200"};
            String[] patterns = {"chat_history:"};
            
            for (String pattern : patterns) {
                for (String limit : limits) {
                    // 构建可能的缓存键模式并删除
                    String keyPattern = pattern + "*:" + limit;
                    // 这里简化处理，删除已知的缓存键
                    log.info("🧹 [Cache] 尝试清除缓存模式: {}", keyPattern);
                }
            }
            
            log.info("🧹 [Cache] 聊天缓存清除完成 - 重新进入聊天将生成新缓存");
        } catch (Exception e) {
            log.error("❌ [Cache] 清除缓存失败: {}", e.getMessage());
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
        log.info("📖 [ChatService] 标记消息为已读: senderId={}, receiverId={}", senderId, receiverId);
        chatMessageMapper.markMessagesAsRead(senderId, receiverId);
        log.info("✅ [ChatService] 消息标记已读完成: senderId={}, receiverId={}", senderId, receiverId);
    }

    @Override
    public Integer getUnreadMessageCount(Long userId) {
        Integer count = chatMessageMapper.getUnreadMessageCount(userId);
        log.info("📊 [ChatService] 查询未读消息数: userId={}, count={}", userId, count);
        return count;
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
        log.info("📊 [ChatService] 查询未读消息映射: currentUserId={}, result={}", currentUserId, result);
        return result;
    }

    @Override
    public void updateUserOnlineStatus(Long userId, Boolean isOnline, String sessionId) {
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

    // 🚫 已废弃：缓存只用于登录前的历史消息，新发送的消息不修改缓存
    // private void updateChatCacheAfterSend(Long user1Id, Long user2Id, ChatMessageResponse newMsg) { ... }

    @Override
    public void processAndDispatch(ChatMessageRequest request) {
        // MQ消费者调用的处理方法
        log.info("📮 [ChatService] Processing message from MQ - SenderId: {}, ReceiverId: {}, ClientMsgId: {}, ThreadInfo: {}", 
                request.getSenderId(), request.getReceiverId(), request.getClientMsgId(), 
                Thread.currentThread().getName() + "-" + Thread.currentThread().getId());
        
        ChatMessageResponse response = processAndDispatchInternal(request.getSenderId(), request);
        
        log.info("✅ [ChatService] Message processed and dispatched successfully from MQ - MessageId: {}, ClientMsgId: {}, Thread: {}", 
                response.getId(), request.getClientMsgId(), Thread.currentThread().getName());
    }

    /**
     * 处理并分发消息的内部方法（复用原有逻辑）
     * @param senderId 发送者ID
     * @param request 消息请求
     * @return 消息响应
     */
    private ChatMessageResponse processAndDispatchInternal(Long senderId, ChatMessageRequest request) {
        ChatMessage message = ChatMessage.builder()
                .senderId(senderId)
                .receiverId(request.getReceiverId())
                .content(request.getContent())
                .messageType(request.getMessageType() != null ? request.getMessageType() : "TEXT")
                .isRead(false)  // 🔍 确保新消息标记为未读
                .clientMsgId(request.getClientMsgId())
                .createTime(LocalDateTime.now())
                .updateTime(LocalDateTime.now()).build();
        
        log.info("🆕 [ChatService] 创建新消息: senderId={}, receiverId={}, isRead={}, clientMsgId={}", 
                senderId, request.getReceiverId(), false, request.getClientMsgId());

        // 幂等插入（基于 senderId + clientMsgId 唯一索引）
        try {
            chatMessageMapper.insert(message);
            log.info("✅ [ChatService] 消息插入成功 - MessageId: {}, ClientMsgId: {}, isRead最终确认: {}", 
                    message.getId(), request.getClientMsgId(), message.getIsRead());
        } catch (DuplicateKeyException e) {
            // 如果违反唯一约束，说明是重复发送，忽略插入并查询已存在记录用于返回
            log.warn("⚠️ [ChatService] 检测到重复消息，基于clientMsgId幂等性跳过: senderId={}, clientMsgId={}", 
                    senderId, request.getClientMsgId());
            ChatMessageResponse existed = findByClientMsgId(senderId, request.getClientMsgId());
            if (existed != null) {
                log.info("🔄 [ChatService] 返回已存在的消息: MessageId={}, ClientMsgId={}", 
                        existed.getId(), request.getClientMsgId());
                return existed;
            }
            throw e;
        }

        ChatMessageResponse response = convertToResponse(message);
        
        // 通过事件发布推送消息给接收者
        try {
            eventPublisher.publishEvent(new ChatMessagePushEvent(this, request.getReceiverId(), response));
            log.info("🎯 [ChatService] Published message push event - ReceiverId: {}, MessageId: {}, ClientMsgId: {}, ThreadId: {}", 
                    request.getReceiverId(), response.getId(), request.getClientMsgId(), Thread.currentThread().getId());
        } catch (Exception e) {
            log.error("❌ [ChatService] Failed to publish message push event for user {}: {}", 
                    request.getReceiverId(), e.getMessage());
        }
        
        // 🚫 不再更新缓存：缓存只用于登录前的历史消息，新发送的消息不修改缓存
        // updateChatCacheAfterSend(senderId, request.getReceiverId(), response);

        return response;
    }

    @Override
    public com.example.academic_collab_platform_backend.model.UserOnlineStatus getUserOnlineStatus(Long userId) {
        return userOnlineStatusMapper.selectById(userId);
    }
} 