package com.example.academic_collab_platform_backend.service;

import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import com.example.academic_collab_platform_backend.dto.ChatMessageResponse;
import com.example.academic_collab_platform_backend.model.ChatMessage;

import java.util.List;

public interface ChatService {
    
    /**
     * 发送消息
     */
    ChatMessageResponse sendMessage(Long senderId, ChatMessageRequest request);
    
    /**
     * 获取聊天历史
     */
    List<ChatMessageResponse> getChatHistory(Long user1Id, Long user2Id, Integer limit);
    
    /**
     * 获取用户列表（用于聊天）
     */
    List<ChatMessageResponse> getUserList(Long currentUserId);
    
    /**
     * 标记消息为已读
     */
    void markMessagesAsRead(Long senderId, Long receiverId);
    
    /**
     * 获取未读消息数
     */
    Integer getUnreadMessageCount(Long userId);
    
    /**
     * 更新用户在线状态
     */
    void updateUserOnlineStatus(Long userId, Boolean isOnline, String sessionId);
} 