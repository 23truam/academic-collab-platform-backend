package com.example.academic_collab_platform_backend.dto;

import lombok.Data;

@Data
public class ChatMessageRequest {
    private Long receiverId;
    private String content;
    private String messageType;
    // 客户端生成的幂等ID（UUID）
    private String clientMsgId;
} 