package com.example.academic_collab_platform_backend.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.example.academic_collab_platform_backend.model.ChatMessage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;

@Mapper
public interface ChatMessageMapper extends BaseMapper<ChatMessage> {
    
    /**
     * 获取两个用户之间的聊天记录
     */
    List<ChatMessage> getChatHistory(@Param("user1Id") Long user1Id, 
                                    @Param("user2Id") Long user2Id, 
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
} 