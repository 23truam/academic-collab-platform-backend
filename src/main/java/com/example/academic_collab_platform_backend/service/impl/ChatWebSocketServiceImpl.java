package com.example.academic_collab_platform_backend.service.impl;

import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import com.example.academic_collab_platform_backend.dto.ChatMessageResponse;
import com.example.academic_collab_platform_backend.dto.UserListDTO;
import com.example.academic_collab_platform_backend.service.ChatService;
import com.example.academic_collab_platform_backend.service.ChatWebSocketService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.stereotype.Service;
import org.springframework.web.socket.messaging.SessionDisconnectEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Map;

@Service
public class ChatWebSocketServiceImpl implements ChatWebSocketService {
    @Autowired
    private ChatService chatService;
    @Autowired
    private SimpMessagingTemplate messagingTemplate;

/*    @Override
    public void handleUserConnect(Long userId, String sessionId) {
        chatService.updateUserOnlineStatus(userId, true, sessionId);
    }

    @Override
    public void handleUserDisconnect(Long userId) {
        chatService.updateUserOnlineStatus(userId, false, null);
    }

    @Override
    public ChatMessageResponse handleSendMessage(Long senderId, ChatMessageRequest request) {
        ChatMessageResponse response = chatService.sendMessage(senderId, request);
        messagingTemplate.convertAndSendToUser(
            request.getReceiverId().toString(),
            "/queue/messages",
            response
        );
        return response;
    }*/

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

    public void handleUserConnect(Long userId,String sessionId){
        System.out.println("[WebSocket] 用户上线: " + userId + ", sessionId: " + sessionId);
        chatService.updateUserOnlineStatus(userId,true,sessionId);
        // 推送上线
        broadcastUserStatus(userId, true);
        // 用户上线时推送未读消息数
        pushUnreadCount(userId);
        pushUnreadMap(userId);
    }



    public void handleUserDisconnect(Long userId) {
        System.out.println("[WebSocket] 用户下线: " + userId);
        chatService.updateUserOnlineStatus(userId, false, null);
        // 推送下线
        broadcastUserStatus(userId, false);
        // 用户下线时也可推送（可选）
        pushUnreadCount(userId);
        pushUnreadMap(userId);
    }

    public ChatMessageResponse handleSendMessage(Long senderId, ChatMessageRequest request) {
        System.out.println("[WebSocket] handleSendMessage 被调用, senderId=" + senderId + ", receiverId=" + request.getReceiverId());
        ChatMessageResponse response=chatService.sendMessage(senderId,request);
        messagingTemplate.convertAndSendToUser(
                request.getReceiverId().toString(),
                "/queue/messages",
                response);
        // 不再推送到/topic/public
        // 新消息后推送未读消息数
        pushUnreadCount(request.getReceiverId());
        pushUnreadMap(request.getReceiverId());
        return response;
    }

    // 推送用户状态变更
    @Override
    public void broadcastUserStatus(Long userId, Boolean isOnline) {
        System.out.println("[WebSocket] 推送用户状态: userId=" + userId + ", isOnline=" + isOnline);
        // 只推送userId和isOnline，前端可自行更新
        messagingTemplate.convertAndSend("/topic/user-status", new UserListDTO(userId, null, isOnline));
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