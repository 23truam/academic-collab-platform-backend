package com.example.academic_collab_platform_backend.service.impl;

import com.baomidou.mybatisplus.core.conditions.Wrapper;
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
import com.example.academic_collab_platform_backend.service.UserService;
import com.example.academic_collab_platform_backend.util.RedisUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.swing.text.StyledEditorKit;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.concurrent.ConcurrentHashMap;

@Service
public class ChatServiceImpl implements ChatService {

    @Autowired
    private ChatMessageMapper chatMessageMapper;

    @Autowired
    private UserMapper userMapper;

    @Autowired
    private UserOnlineStatusMapper userOnlineStatusMapper;

    @Autowired
    private UserService userService;

    @Autowired
    private RedisUtil redisUtil;

    // 缓存过期时间：1小时
    private static final long CACHE_EXPIRE_HOURS = 1;

    // 添加用户信息缓存
    private final Map<Long, User> userCache = new ConcurrentHashMap<>();

    private User getUserFromCache(Long userId) {
        return userCache.computeIfAbsent(userId, id -> userMapper.selectById(id));
    }

    @Override
    public ChatMessageResponse sendMessage(Long senderId, ChatMessageRequest request) {
        ChatMessage message = ChatMessage.builder()
                .senderId(senderId)
                .receiverId(request.getReceiverId())
                .content(request.getContent())
                .messageType(request.getMessageType() != null ? request.getMessageType() : "TEXT")
                .isRead(false)
                .createTime(LocalDateTime.now())
                .updateTime(LocalDateTime.now()).build();

        chatMessageMapper.insert(message);

        // 发送新消息后，清除相关缓存
        clearChatCache(senderId, request.getReceiverId());

        return convertToResponse(message);
    }

    @Override
    public List<ChatMessageResponse> getChatHistory(Long user1Id, Long user2Id, Integer limit) {
        List<ChatMessage> messages = chatMessageMapper.getChatHistory(user1Id, user2Id, limit != null ? limit : 50);
        return messages.stream()
                .map(this::convertToResponse)
                .collect(Collectors.toList());
    }

    @Override
    public Map<String, Object> getChatHistoryWithCache(Long user1Id, Long user2Id, Integer limit, Long loginTime) {
        String cacheKey = generateChatCacheKey(user1Id, user2Id, limit);
        String cachedJson = redisUtil.get(cacheKey);
        List<ChatMessageResponse> cachedMessages = null;
        if (cachedJson != null) {
            try {
                cachedMessages = redisUtil.getObjectMapper().readValue(cachedJson, 
                    redisUtil.getObjectMapper().getTypeFactory().constructCollectionType(List.class, ChatMessageResponse.class));
            } catch (Exception e) {
                redisUtil.delete(cacheKey);
            }
        }

        List<ChatMessage> allMessages = chatMessageMapper.getChatHistory(user1Id, user2Id, limit != null ? limit : 50);
        List<ChatMessageResponse> allResponses = allMessages.stream().map(this::convertToResponse).collect(Collectors.toList());

        // 按登录时间分为历史和新消息
        List<ChatMessageResponse> historyMessages = new java.util.ArrayList<>();
        List<ChatMessageResponse> recentMessages = new java.util.ArrayList<>();
        if (loginTime != null) {
            for (ChatMessageResponse msg : allResponses) {
                if (msg.getCreateTime().toInstant(java.time.ZoneOffset.ofHours(8)).toEpochMilli() < loginTime) {
                    historyMessages.add(msg);
                } else {
                    recentMessages.add(msg);
                }
            }
        } else {
            historyMessages.addAll(allResponses);
        }

        // 只缓存历史消息
        if (cachedMessages == null && !historyMessages.isEmpty()) {
            redisUtil.setObject(cacheKey, historyMessages, CACHE_EXPIRE_HOURS, TimeUnit.HOURS);
        }

        Map<String, Object> result = new java.util.HashMap<>();
        result.put("historyMessages", historyMessages);
        result.put("recentMessages", recentMessages);
        result.put("hasHistoryDivider", !historyMessages.isEmpty() && !recentMessages.isEmpty());
        return result;
    }

    @Override
    public void clearChatCache(Long user1Id, Long user2Id) {
        // 清除不同limit的缓存
        String[] limits = {"20", "50", "100"};
        for (String limit : limits) {
            String cacheKey = generateChatCacheKey(user1Id, user2Id, Integer.parseInt(limit));
            redisUtil.delete(cacheKey);
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
    public Map<Long, Integer> getUnreadCountMap(Long currentUserId) {
        List<Map<String, Object>> list = chatMessageMapper.getUnreadCountMap(currentUserId);
        Map<Long, Integer> result = new java.util.HashMap<>();
        for (Map<String, Object> row : list) {
            Long senderId = ((Number) row.get("sender_id")).longValue();
            Integer count = ((Number) row.get("cnt")).intValue();
            result.put(senderId, count);
        }
        return result;
    }

    @Override
    public void updateUserOnlineStatus(Long userId, Boolean isOnline, String sessionId) {
        UserOnlineStatus status = userOnlineStatusMapper.selectById(userId);
        if (status == null) {
            status = new UserOnlineStatus();
            status.setUserId(userId);
        }
        status.setIsOnline(isOnline);
        if (Boolean.TRUE.equals(isOnline)) {
            status.setLastLoginTime(LocalDateTime.now());
        }
        status.setSessionId(sessionId);

        if (status.getUserId() != null) {
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
        return "chat_history:" + user1Id + ":" + user2Id + ":" + limit;
    }
} 