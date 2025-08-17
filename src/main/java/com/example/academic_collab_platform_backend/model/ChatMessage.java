package com.example.academic_collab_platform_backend.model;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;


@Builder
@Data
@TableName("chat_messages")
public class ChatMessage {
    
    @TableId(type = IdType.AUTO)
    private Long id;
    
    private Long senderId;
    
    private Long receiverId;
    
    private String content;
    
    private String messageType;
    
    private Boolean isRead;

    // 客户端生成的幂等ID
    private String clientMsgId;
    
    private LocalDateTime createTime;
    
    private LocalDateTime updateTime;
} 