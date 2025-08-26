package com.example.academic_collab_platform_backend.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.example.academic_collab_platform_backend.dto.ChatMessageResponse;
import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import com.example.academic_collab_platform_backend.dto.UserListDTO;
import com.example.academic_collab_platform_backend.model.UserOnlineStatus;
import com.example.academic_collab_platform_backend.service.ChatService;
import com.example.academic_collab_platform_backend.service.ChatWebSocketService;
import com.example.academic_collab_platform_backend.event.ChatMessagePushEvent;
import com.example.academic_collab_platform_backend.mq.ChatMessageProducer;
import com.example.academic_collab_platform_backend.mq.UserQueueConsumer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.stereotype.Service;
import org.springframework.web.socket.messaging.SessionDisconnectEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Service
public class ChatWebSocketServiceImpl implements ChatWebSocketService {
    @Autowired
    private ChatService chatService;
    @Autowired
    private SimpMessagingTemplate messagingTemplate;
    @Autowired
    private ChatMessageProducer chatMessageProducer;  //  注入消息生产者
    
    @Autowired
    private UserQueueConsumer userQueueConsumer;  //  注入用户队列消费者

    // 维护用户当前活跃会话对端：key=用户ID，value=正在聊天的对端用户ID
    private static final Map<Long, Long> activePeerMap = new ConcurrentHashMap<>();

    // 上线/下线事件由 WebSocket 层调用

    @Override
    public void setActivePeer(Long userId, Long peerUserId) {
        if (userId == null) return;
        if (peerUserId == null) {
            activePeerMap.remove(userId);
            System.out.println("[WebSocket] 清除活跃会话: userId=" + userId);
        } else {
            activePeerMap.put(userId, peerUserId);
            System.out.println("[WebSocket] 设置活跃会话: userId=" + userId + ", peerUserId=" + peerUserId);
        }
    }

    @Override
    public void clearActivePeer(Long userId) {
        if (userId != null) {
            activePeerMap.remove(userId);
        }
    }

    // 推送全局未读消息数
    @Override
    public void pushUnreadCount(Long userId) {
        Integer totalUnread = chatService.getUnreadMessageCount(userId);
        messagingTemplate.convertAndSendToUser(
            userId.toString(),
            "/queue/unread-count",
            totalUnread
        );
    }
    
    // 防重复推送的缓存 - 只基于时间间隔防重复，不基于内容
    private static final Map<Long, Long> lastPushTimeCache = new ConcurrentHashMap<>();
    private static final long PUSH_INTERVAL_MS = 100; // 100ms内同一用户不重复推送
    
    @Override
    public void pushUnreadMap(Long userId) {
        long currentTime = System.currentTimeMillis();
        
        // 🔧 修正防重复逻辑：只基于时间间隔，不基于内容
        // 因为用户可能发送相同内容的消息多次，unreadMap统计应该实时更新
        Long lastTime = lastPushTimeCache.get(userId);
        if (lastTime != null && (currentTime - lastTime) < PUSH_INTERVAL_MS) {
            System.out.println("🚫 [WebSocket] 跳过频繁推送unreadMap: userId=" + userId + 
                              ", 距离上次推送" + (currentTime - lastTime) + "ms < " + PUSH_INTERVAL_MS + "ms");
            return;
        }
        
        Map<Long, Integer> unreadMap = chatService.getUnreadCountMap(userId);
        System.out.println("📊 [WebSocket] 推送unreadMap给userId=" + userId + " 内容: " + unreadMap + 
                          " 调用者: " + Thread.currentThread().getStackTrace()[2].getMethodName() + 
                          " 线程: " + Thread.currentThread().getName());
        
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            String json = objectMapper.writeValueAsString(unreadMap);
            messagingTemplate.convertAndSendToUser(
                userId.toString(),
                "/queue/unread-map",
                json
            );
            
            // 只记录推送时间
            lastPushTimeCache.put(userId, currentTime);
            
        } catch (Exception e) {
            System.err.println("❌ [WebSocket] unreadMap序列化失败: " + e.getMessage());
        }
    }

    @Override
    public void handleUserConnect(Long userId, String sessionId) {
        System.out.println("🟢 [Online] 用户上线: userId=" + userId + ", sessionId=" + sessionId);
        
        try {
            // 🆕 启动用户专属队列消费者
            System.out.println("🔄 [Online] 准备启动用户队列消费者: userId=" + userId);
            userQueueConsumer.startConsumerForUser(userId);
            System.out.println("🚀 [Online] 用户队列消费者启动成功: userId=" + userId);
            

            
            // 原有逻辑保持不变
            chatService.updateUserOnlineStatus(userId, true, sessionId);
            broadcastUserStatus(userId, true);
            
            // 🔧 减少推送频率：用户上线时只推送一次
            System.out.println("🔄 [Online] 推送用户上线未读统计: userId=" + userId);
            pushUnreadCount(userId);
            pushUnreadMap(userId);
            
        } catch (Exception e) {
            System.err.println("❌ [Online] 用户上线处理异常: userId=" + userId + ", error=" + e.getMessage());
            // 即使消费者启动失败，也不应阻止用户上线的其他流程
        }
    }
    




    @Override
    public void handleUserDisconnect(Long userId) {
        System.out.println("🔴 [Offline] 用户下线: " + userId);
        
        try {
            //停止用户专属队列消费者
            userQueueConsumer.stopConsumerForUser(userId);
            System.out.println("🛑 [Offline] 用户队列消费者停止成功: userId=" + userId);
            chatService.updateUserOnlineStatus(userId, false, null);
            // 清理活跃会话映射
            activePeerMap.remove(userId);
            // 推送下线
            broadcastUserStatus(userId, false);
            // 🔧 优化：用户下线时不推送未读统计（因为用户已经离线）
            // pushUnreadCount(userId);
            // pushUnreadMap(userId);
            System.out.println("🔄 [Offline] 用户下线，跳过未读统计推送: userId=" + userId);
            
        } catch (Exception e) {
            System.err.println("❌ [Offline] 用户下线处理异常: userId=" + userId + ", error=" + e.getMessage());
            // 即使消费者停止失败，也要继续其他下线流程
        }
    }



    // 推送用户状态变更
    @Override
    public void broadcastUserStatus(Long userId, Boolean isOnline) {
        System.out.println("[WebSocket] 推送用户状态: userId=" + userId + ", isOnline=" + isOnline);
        // 只推送userId和isOnline，前端可自行更新
        messagingTemplate.convertAndSend("/topic/user-status", new UserListDTO(userId, null, isOnline));
    }

    // 直接推送消息给指定用户（供MQ消费者调用）
    @Override
    public void sendMessageToUser(Long receiverId, ChatMessageResponse message) {
        System.out.println("[WebSocket] 准备推送消息: receiverId=" + receiverId + 
            ", messageId=" + message.getId() + 
            ", messageType=" + message.getMessageType());
        
        // 🔧 修复：始终更新未读统计，不管用户是否在线
        System.out.println("📊 [WebSocket] 发送消息时更新未读统计: receiverId=" + receiverId + ", messageId=" + message.getId());
        pushUnreadCount(receiverId);
        pushUnreadMap(receiverId);
        
        // 检查用户在线状态，决定是否推送WebSocket消息
        boolean userOnline = isUserOnline(receiverId);
        if (!userOnline) {
            System.out.println("📴 [WebSocket] 用户离线，跳过WebSocket推送（未读统计已更新）: receiverId=" + receiverId + ", messageId=" + message.getId());
            return;
        }
        
        // 推送到消息队列
        String queueSuffix = "/queue/messages";
        
        System.out.println("[WebSocket] 推送消息到队列: " + queueSuffix + ", receiverId=" + receiverId + 
            ", messageId=" + message.getId());
        
        // 推送消息到用户个人队列
        try {
            messagingTemplate.convertAndSendToUser(
                    receiverId.toString(),
                    queueSuffix,
                    message);
            System.out.println("✅ [WebSocket] 消息推送完成: receiverId=" + receiverId + 
                ", messageId=" + message.getId() + 
                ", senderId=" + message.getSenderId() + 
                ", content=" + message.getContent() + 
                ", messageType=" + message.getMessageType());
        } catch (Exception e) {
            System.err.println("❌ [WebSocket] 消息推送失败: receiverId=" + receiverId + 
                ", messageId=" + message.getId() + ", error=" + e.getMessage());
        }
    }

    // 🆕 添加在线状态检查方法
    private boolean isUserOnline(Long userId) {
        try {
            UserOnlineStatus status = chatService.getUserOnlineStatus(userId);
            
            // 安全的null检查
            boolean isOnline = status != null && 
                              status.getIsOnline() != null && 
                              Boolean.TRUE.equals(status.getIsOnline());
            
            System.out.println("🔍 [OnlineCheck] 用户在线状态检查: userId=" + userId + 
                              ", status=" + (status != null ? "存在" : "不存在") + 
                              ", isOnline=" + (status != null ? status.getIsOnline() : "null") + 
                              ", 最终判定=" + isOnline);
            
            return isOnline;
        } catch (Exception e) {
            System.err.println("❌ [OnlineCheck] 状态检查异常: userId=" + userId + ", error=" + e.getMessage());
            // 异常时默认认为离线，避免无效推送
            return false;
        }
    }

    // 监听消息推送事件
    @EventListener
    public void handleChatMessagePushEvent(ChatMessagePushEvent event) {
        System.out.println("🎯 [WebSocket] 收到消息推送事件: receiverId=" + event.getReceiverId() + 
                          ", messageId=" + event.getMessage().getId() + 
                          ", senderId=" + event.getMessage().getSenderId() + 
                          ", content=" + event.getMessage().getContent() + 
                          ", threadId=" + Thread.currentThread().getId());
        sendMessageToUser(event.getReceiverId(), event.getMessage());
    }

    // 监听WebSocket断开事件，自动推送下线
    @EventListener
    public void handleWebSocketDisconnectListener(SessionDisconnectEvent event) {
        StompHeaderAccessor headerAccessor = StompHeaderAccessor.wrap(event.getMessage());
        Object userIdObj = headerAccessor.getSessionAttributes() != null ? headerAccessor.getSessionAttributes().get("userId") : null;
        if (userIdObj != null) {
            try {
                Long userId = Long.valueOf(userIdObj.toString());
                System.out.println("[WebSocket] 断开事件触发下线推送: " + userId);
                handleUserDisconnect(userId);
            } catch (Exception e) {
                System.err.println("[WebSocket] 断开事件处理异常: " + e.getMessage());
            }
        }
    }
} 