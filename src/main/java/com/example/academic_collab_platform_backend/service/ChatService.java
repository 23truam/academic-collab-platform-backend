package com.example.academic_collab_platform_backend.service;

import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import com.example.academic_collab_platform_backend.dto.ChatMessageResponse;
import com.example.academic_collab_platform_backend.dto.UserListDTO;
import com.example.academic_collab_platform_backend.model.ChatMessage;

import java.util.List;
import java.util.Map;

public interface ChatService {
    
    /**
     * 发送消息
     */
    ChatMessageResponse sendMessage(Long senderId, ChatMessageRequest request);
    
    /**
     * 获取聊天历史（带缓存）
     */
    List<ChatMessageResponse> getChatHistory(Long user1Id, Long user2Id, Integer limit);
    
    /**
     * 获取聊天历史（带缓存和登录时间标记）
     */
    Map<String, Object> getChatHistoryWithCache(Long user1Id, Long user2Id, Integer limit, Long loginTime);
    
    /**
     * 清除聊天缓存
     */
    void clearChatCache(Long user1Id, Long user2Id);
    
    /**
     * 获取用户列表（用于聊天）
     */
    List<UserListDTO> getUserList(Long currentUserId);
    
    /**
     * 标记消息为已读
     */
    void markMessagesAsRead(Long senderId, Long receiverId);
    
    /**
     * 获取未读消息数
     */
    Integer getUnreadMessageCount(Long userId);
    
    /**
     * 获取未读消息数（按用户分组）
     */
    Map<Long, Integer> getUnreadCountMap(Long currentUserId);
    
    /**
     * 更新用户在线状态
     */
    void updateUserOnlineStatus(Long userId, Boolean isOnline, String sessionId);
    
    /**
     * 处理并分发消息（供MQ消费者调用）
     */
    void processAndDispatch(ChatMessageRequest request);
    
    /**
     * 获取用户在线状态
     */
    com.example.academic_collab_platform_backend.model.UserOnlineStatus getUserOnlineStatus(Long userId);
    
    // 🆕 第二阶段：离线消息处理
    /**
     * 获取用户离线期间的消息
     */
    List<ChatMessageResponse> getOfflineMessages(Long userId, java.time.LocalDateTime lastLogoutTime, Integer limit);
    
    /**
     * 处理离线消息拉取请求
     */
    void processOfflineMessagePull(ChatMessageRequest request);
    
    // 🆕 第三阶段：监控和性能优化
    /**
     * 获取离线消息处理统计
     */
    Map<String, Object> getOfflineMessageStats(Long userId);
    
    /**
     * 批量标记消息为已读（优化版）
     */
    void batchMarkMessagesAsRead(Long userId, List<Long> messageIds);
} 