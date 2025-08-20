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
    private ChatMessageProducer chatMessageProducer;  // 🆕 注入消息生产者

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
    
    @Override
    public void pushUnreadMap(Long userId) {
        Map<Long, Integer> unreadMap = chatService.getUnreadCountMap(userId);
        System.out.println("[WebSocket] 推送unreadMap给userId=" + userId + " 内容: " + unreadMap);
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            String json = objectMapper.writeValueAsString(unreadMap);
            messagingTemplate.convertAndSendToUser(
                userId.toString(),
                "/queue/unread-map",
                json
            );
        } catch (Exception e) {
            System.err.println("[WebSocket] unreadMap序列化失败: " + e.getMessage());
        }
    }

    @Override
    public void handleUserConnect(Long userId, String sessionId) {
        System.out.println("🟢 [Online] 用户上线: userId=" + userId + ", sessionId=" + sessionId);
        
        // 🆕 第二阶段：发送离线消息拉取请求到RabbitMQ
        triggerOfflineMessagePull(userId, sessionId);
        
        // 原有逻辑保持不变
        chatService.updateUserOnlineStatus(userId, true, sessionId);
        broadcastUserStatus(userId, true);
        pushUnreadCount(userId);
        pushUnreadMap(userId);
    }
    
    // 🆕 第二阶段：触发离线消息拉取
    private void triggerOfflineMessagePull(Long userId, String sessionId) {
        try {
            // 1. 获取用户上次下线时间
            UserOnlineStatus status = chatService.getUserOnlineStatus(userId);
            if (status == null || status.getLastLogoutTime() == null) {
                System.out.println("📭 [Offline] 无需拉取离线消息: userId=" + userId + " (无下线时间记录)");
                return;
            }
            
            // 2. 创建离线消息拉取请求
            ChatMessageRequest offlineRequest = ChatMessageRequest.createOfflinePullRequest(
                userId, status.getLastLogoutTime(), sessionId
            );
            
            // 3. 🎯 发送到现有的chat消息队列（复用现有基础设施）
            chatMessageProducer.publish(offlineRequest);
            
            System.out.println("📨 [Offline] 离线消息拉取请求已发送: userId=" + userId + 
                ", lastLogoutTime=" + status.getLastLogoutTime() + 
                ", clientMsgId=" + offlineRequest.getClientMsgId());
                
        } catch (Exception e) {
            System.err.println("❌ [Offline] 离线消息拉取请求发送失败: userId=" + userId + 
                ", error=" + e.getMessage());
        }
    }



    @Override
    public void handleUserDisconnect(Long userId) {
        System.out.println("[WebSocket] 用户下线: " + userId);
        chatService.updateUserOnlineStatus(userId, false, null);
        // 清理活跃会话映射
        activePeerMap.remove(userId);
        // 推送下线
        broadcastUserStatus(userId, false);
        // 用户下线时也可推送（可选）
        pushUnreadCount(userId);
        pushUnreadMap(userId);
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
        
        // 🆕 第二阶段：对于离线消息，不需要检查在线状态（因为是拉取时推送）
        if (!"OFFLINE".equals(message.getMessageType())) {
            // 🆕 普通消息才检查在线状态
            if (!isUserOnline(receiverId)) {
                System.out.println("📴 [Offline] 用户离线，跳过推送: receiverId=" + receiverId + ", messageId=" + message.getId());
                return;
            }
        }
        
        // 🆕 第三阶段：根据消息类型选择不同的推送队列
        String queueSuffix = "OFFLINE".equals(message.getMessageType()) ? 
            "/queue/offline-messages" : "/queue/messages";
        
        System.out.println("[WebSocket] 推送消息到队列: " + queueSuffix + ", receiverId=" + receiverId + 
            ", messageId=" + message.getId());
        
        // 推送消息到用户个人队列
        messagingTemplate.convertAndSendToUser(
                receiverId.toString(),
                queueSuffix,
                message);
        
        // 🆕 对于离线消息，推送后立即标记为已读（用户现在看到了）
        if ("OFFLINE".equals(message.getMessageType()) && message.getId() != null) {
            try {
                // 标记该条离线消息为已读
                chatService.markMessagesAsRead(message.getSenderId(), receiverId);
                System.out.println("📖 [Offline] 离线消息已标记为已读: messageId=" + message.getId());
            } catch (Exception e) {
                System.err.println("❌ [Offline] 标记离线消息为已读失败: " + e.getMessage());
            }
        }
        
        // 推送未读统计更新
        pushUnreadCount(receiverId);
        pushUnreadMap(receiverId);
        
        System.out.println("✅ [WebSocket] 消息推送完成: receiverId=" + receiverId + 
            ", messageType=" + message.getMessageType());
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
        System.out.println("[WebSocket] 收到消息推送事件: receiverId=" + event.getReceiverId() + ", messageId=" + event.getMessage().getId());
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