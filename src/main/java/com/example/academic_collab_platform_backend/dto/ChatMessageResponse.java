package com.example.academic_collab_platform_backend.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;


@Data
public class ChatMessageResponse {
    private Long id;
    private Long senderId;
    private String senderName;
    private Long receiverId;
    private String receiverName;
    private String content;
    private String messageType;
    private Boolean isRead;
    private LocalDateTime createTime;
} 