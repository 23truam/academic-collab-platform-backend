package com.example.academic_collab_platform_backend.model;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;


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
    
    // 🔧 手动添加 getter/setter 方法（防止Lombok问题）
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public Long getSenderId() { return senderId; }
    public void setSenderId(Long senderId) { this.senderId = senderId; }
    
    public Long getReceiverId() { return receiverId; }
    public void setReceiverId(Long receiverId) { this.receiverId = receiverId; }
    
    public String getContent() { return content; }
    public void setContent(String content) { this.content = content; }
    
    public String getMessageType() { return messageType; }
    public void setMessageType(String messageType) { this.messageType = messageType; }
    
    public Boolean getIsRead() { return isRead; }
    public void setIsRead(Boolean isRead) { this.isRead = isRead; }
    
    public String getClientMsgId() { return clientMsgId; }
    public void setClientMsgId(String clientMsgId) { this.clientMsgId = clientMsgId; }
    
    public LocalDateTime getCreateTime() { return createTime; }
    public void setCreateTime(LocalDateTime createTime) { this.createTime = createTime; }
    
    public LocalDateTime getUpdateTime() { return updateTime; }
    public void setUpdateTime(LocalDateTime updateTime) { this.updateTime = updateTime; }
    
    // 🔧 添加默认构造函数
    public ChatMessage() {}
    
    // 🔧 添加静态builder方法
    public static ChatMessageBuilder builder() {
        return new ChatMessageBuilder();
    }
    
    // 🔧 手动实现Builder类
    public static class ChatMessageBuilder {
        private Long id;
        private Long senderId;
        private Long receiverId;
        private String content;
        private String messageType;
        private Boolean isRead;
        private String clientMsgId;
        private LocalDateTime createTime;
        private LocalDateTime updateTime;
        
        public ChatMessageBuilder id(Long id) { this.id = id; return this; }
        public ChatMessageBuilder senderId(Long senderId) { this.senderId = senderId; return this; }
        public ChatMessageBuilder receiverId(Long receiverId) { this.receiverId = receiverId; return this; }
        public ChatMessageBuilder content(String content) { this.content = content; return this; }
        public ChatMessageBuilder messageType(String messageType) { this.messageType = messageType; return this; }
        public ChatMessageBuilder isRead(Boolean isRead) { this.isRead = isRead; return this; }
        public ChatMessageBuilder clientMsgId(String clientMsgId) { this.clientMsgId = clientMsgId; return this; }
        public ChatMessageBuilder createTime(LocalDateTime createTime) { this.createTime = createTime; return this; }
        public ChatMessageBuilder updateTime(LocalDateTime updateTime) { this.updateTime = updateTime; return this; }
        
        public ChatMessage build() {
            ChatMessage message = new ChatMessage();
            message.setId(id);
            message.setSenderId(senderId);
            message.setReceiverId(receiverId);
            message.setContent(content);
            message.setMessageType(messageType);
            message.setIsRead(isRead);
            message.setClientMsgId(clientMsgId);
            message.setCreateTime(createTime);
            message.setUpdateTime(updateTime);
            return message;
        }
    }
} 