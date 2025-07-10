package com.example.academic_collab_platform_backend.service;

import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import com.example.academic_collab_platform_backend.dto.ChatMessageResponse;

public interface ChatWebSocketService {
    void handleUserConnect(Long userId, String sessionId);
    void handleUserDisconnect(Long userId);
    ChatMessageResponse handleSendMessage(Long senderId, ChatMessageRequest request);
    // 推送用户状态变更
    void broadcastUserStatus(Long userId, Boolean isOnline);
    // 推送全局未读消息数
    void pushUnreadCount(Long userId);
    // 推送每个发送者的未读消息数
    void pushUnreadMap(Long userId);
} 