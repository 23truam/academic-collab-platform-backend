package com.example.academic_collab_platform_backend.event;

import com.example.academic_collab_platform_backend.dto.ChatMessageResponse;
import org.springframework.context.ApplicationEvent;

/**
 * 聊天消息推送事件
 */
public class ChatMessagePushEvent extends ApplicationEvent {
    private final Long receiverId;
    private final ChatMessageResponse message;

    public ChatMessagePushEvent(Object source, Long receiverId, ChatMessageResponse message) {
        super(source);
        this.receiverId = receiverId;
        this.message = message;
    }

    public Long getReceiverId() {
        return receiverId;
    }

    public ChatMessageResponse getMessage() {
        return message;
    }
}
