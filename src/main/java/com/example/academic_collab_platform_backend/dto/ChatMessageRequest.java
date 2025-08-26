package com.example.academic_collab_platform_backend.dto;

import lombok.Data;
import java.time.LocalDateTime;

@Data
public class ChatMessageRequest {
    private Long senderId;
    private Long receiverId;
    private String content;
    private String messageType = "TEXT";  // 默认为TEXT类型
    // 客户端生成的幂等ID（UUID）
    private String clientMsgId;
    


} 