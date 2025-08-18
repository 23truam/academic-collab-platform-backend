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
import java.util.concurrent.ConcurrentHashMap;

@Service
public class ChatWebSocketServiceImpl implements ChatWebSocketService {
    @Autowired
    private ChatService chatService;
    @Autowired
    private SimpMessagingTemplate messagingTemplate;

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
    public void handleUserConnect(Long userId,String sessionId){
        System.out.println("[WebSocket] 用户上线: " + userId + ", sessionId: " + sessionId);
        chatService.updateUserOnlineStatus(userId,true,sessionId);
        // 推送上线
        broadcastUserStatus(userId, true);
        // 用户上线时推送未读消息数
        pushUnreadCount(userId);
        pushUnreadMap(userId);
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

    @Override
    public ChatMessageResponse handleSendMessage(Long senderId, ChatMessageRequest request) {
        System.out.println("[WebSocket] handleSendMessage 被调用, senderId=" + senderId + ", receiverId=" + request.getReceiverId());
        ChatMessageResponse response=chatService.sendMessage(senderId,request);
        // 只推送给接收方个人队列
        messagingTemplate.convertAndSendToUser(
                request.getReceiverId().toString(),
                "/queue/messages",
                response);
        // 新消息后：
        // - 若接收方未处于与发送方的同一活跃会话，推送未读统计（保持红点）
        // - 若接收方正处于与发送方的活跃会话，不再由后端自动标记已读，改由前端在确认为当前会话时主动调用标记接口，避免误判
        Long activePeer = activePeerMap.get(request.getReceiverId());
        if (activePeer == null || !activePeer.equals(senderId)) {
            System.out.println("[WebSocket] 推送未读统计: receiverId=" + request.getReceiverId() + ", activePeer=" + activePeer + ", senderId=" + senderId);
            pushUnreadCount(request.getReceiverId());
            pushUnreadMap(request.getReceiverId());
        } else {
            System.out.println("[WebSocket] 接收方处于与发送方的活跃会话，由前端明确ACK后再标记已读");
        }
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