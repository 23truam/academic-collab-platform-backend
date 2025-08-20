package com.example.academic_collab_platform_backend.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.example.academic_collab_platform_backend.model.ChatMessage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;
import java.time.LocalDateTime;

@Mapper
public interface ChatMessageMapper extends BaseMapper<ChatMessage> {
    
    /**
     * 获取两个用户之间的聊天记录
     */
    List<ChatMessage> getChatHistory(@Param("user1Id") Long user1Id, 
                                    @Param("user2Id") Long user2Id, 
                                    @Param("limit") Integer limit);

    /**
     * 获取两个用户之间在指定时间之前的聊天记录（倒序，限制条数）
     */
    List<ChatMessage> getChatHistoryBeforeTime(@Param("user1Id") Long user1Id,
                                               @Param("user2Id") Long user2Id,
                                               @Param("beforeTime") LocalDateTime beforeTime,
                                               @Param("limit") Integer limit);

    /**
     * 获取两个用户之间在指定时间之后的聊天记录（含边界，倒序，限制条数）
     */
    List<ChatMessage> getChatHistoryAfterTime(@Param("user1Id") Long user1Id,
                                              @Param("user2Id") Long user2Id,
                                              @Param("afterTime") LocalDateTime afterTime,
                                              @Param("limit") Integer limit);
    
    /**
     * 获取用户的未读消息数
     */
    Integer getUnreadMessageCount(@Param("userId") Long userId);
    
    /**
     * 标记消息为已读
     */
    void markMessagesAsRead(@Param("senderId") Long senderId, 
                           @Param("receiverId") Long receiverId);

    List<Map<String, Object>> getUnreadCountMap(Long currentUserId);
    
    // 🆕 第二阶段：离线消息查询
    /**
     * 查询用户离线期间收到的消息
     */
    List<ChatMessage> getOfflineMessages(@Param("userId") Long userId, 
                                       @Param("lastLogoutTime") LocalDateTime lastLogoutTime,
                                       @Param("limit") Integer limit);
    
    // 🆕 第三阶段：批量操作优化
    /**
     * 批量标记消息为已读
     */
    void batchMarkAsRead(@Param("userId") Long userId, @Param("messageIds") List<Long> messageIds);
    
    /**
     * 批量查询消息
     */
    List<ChatMessage> selectBatchByIds(@Param("messageIds") List<Long> messageIds);
    
    /**
     * 统计用户在指定时间段内的消息数量
     */
    Integer countMessagesByTimeRange(@Param("userId") Long userId,
                                   @Param("startTime") LocalDateTime startTime,
                                   @Param("endTime") LocalDateTime endTime);
} 