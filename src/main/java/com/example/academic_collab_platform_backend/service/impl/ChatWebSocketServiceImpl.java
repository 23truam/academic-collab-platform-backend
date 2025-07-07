package com.example.academic_collab_platform_backend.service.impl;

import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import com.example.academic_collab_platform_backend.dto.ChatMessageResponse;
import com.example.academic_collab_platform_backend.service.ChatService;
import com.example.academic_collab_platform_backend.service.ChatWebSocketService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

@Service
public class ChatWebSocketServiceImpl implements ChatWebSocketService {
    @Autowired
    private ChatService chatService;
    @Autowired
    private SimpMessagingTemplate messagingTemplate;

    @Override
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
    }
} 