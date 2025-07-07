package com.example.academic_collab_platform_backend.service;

import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import com.example.academic_collab_platform_backend.dto.ChatMessageResponse;

public interface ChatWebSocketService {
    void handleUserConnect(Long userId, String sessionId);
    void handleUserDisconnect(Long userId);
    ChatMessageResponse handleSendMessage(Long senderId, ChatMessageRequest request);
} 