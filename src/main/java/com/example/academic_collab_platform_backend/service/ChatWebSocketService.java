package com.example.academic_collab_platform_backend.service;

import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import com.example.academic_collab_platform_backend.dto.ChatMessageResponse;

public interface ChatWebSocketService {
    void handleUserConnect(Long userId, String sessionId);
    void handleUserDisconnect(Long userId);
    // 推送用户状态变更
    void broadcastUserStatus(Long userId, Boolean isOnline);
    // 推送全局未读消息数
    void pushUnreadCount(Long userId);
    // 推送每个发送者的未读消息数
    void pushUnreadMap(Long userId);
    // 记录用户当前活跃会话对象
    void setActivePeer(Long userId, Long peerUserId);
    // 清除用户当前活跃会话对象
    void clearActivePeer(Long userId);
    
    // 直接推送消息给指定用户（供MQ消费者调用）
    void sendMessageToUser(Long receiverId, ChatMessageResponse message);
} 