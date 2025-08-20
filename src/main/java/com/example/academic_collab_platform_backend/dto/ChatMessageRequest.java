package com.example.academic_collab_platform_backend.dto;

import lombok.Data;
import java.time.LocalDateTime;

@Data
public class ChatMessageRequest {
    private Long senderId;
    private Long receiverId;
    private String content;
    private String messageType = "TEXT";  // 🆕 默认为TEXT，支持OFFLINE_PULL类型
    // 客户端生成的幂等ID（UUID）
    private String clientMsgId;
    
    // 🆕 第二阶段：离线消息处理支持
    private String triggerTime;  // 触发时间字符串，用于幂等性和调试
    private Integer batchSize;          // 批量处理大小
    private String sessionId;          // 会话ID，用于验证用户仍在线
    
    // 🆕 第三阶段：监控和性能优化
    private Long processStartTime;      // 处理开始时间戳
    private String processingNode;      // 处理节点标识
    
    /**
     * 🆕 工厂方法：创建离线消息拉取请求
     */
    public static ChatMessageRequest createOfflinePullRequest(Long userId, LocalDateTime lastLogoutTime, String sessionId) {
        ChatMessageRequest request = new ChatMessageRequest();
        request.setMessageType("OFFLINE_PULL");
        request.setSenderId(userId);          // 复用senderId存储用户ID
        request.setReceiverId(userId);        // 复用receiverId存储用户ID  
        request.setContent(lastLogoutTime.toString());  // 复用content存储下线时间
        request.setClientMsgId("offline_" + userId + "_" + System.currentTimeMillis() + "_" + sessionId.substring(0, Math.min(8, sessionId.length())));
        request.setTriggerTime(LocalDateTime.now().toString());
        request.setSessionId(sessionId);
        request.setBatchSize(100);  // 默认批量大小
        request.setProcessStartTime(System.currentTimeMillis());
        return request;
    }
    
    /**
     * 🆕 判断是否为离线消息拉取请求
     */
    public boolean isOfflinePullRequest() {
        return "OFFLINE_PULL".equals(this.messageType);
    }
    
    /**
     * 🆕 获取处理耗时（毫秒）
     */
    public long getProcessingTimeMs() {
        return processStartTime != null ? System.currentTimeMillis() - processStartTime : 0;
    }
    
    // 🔧 手动添加 getter/setter 方法（防止Lombok问题）
    public Long getSenderId() { return senderId; }
    public void setSenderId(Long senderId) { this.senderId = senderId; }
    
    public Long getReceiverId() { return receiverId; }
    public void setReceiverId(Long receiverId) { this.receiverId = receiverId; }
    
    public String getContent() { return content; }
    public void setContent(String content) { this.content = content; }
    
    public String getMessageType() { return messageType; }
    public void setMessageType(String messageType) { this.messageType = messageType; }
    
    public String getClientMsgId() { return clientMsgId; }
    public void setClientMsgId(String clientMsgId) { this.clientMsgId = clientMsgId; }
    
    public String getTriggerTime() { return triggerTime; }
    public void setTriggerTime(String triggerTime) { this.triggerTime = triggerTime; }
    
    public Integer getBatchSize() { return batchSize; }
    public void setBatchSize(Integer batchSize) { this.batchSize = batchSize; }
    
    public String getSessionId() { return sessionId; }
    public void setSessionId(String sessionId) { this.sessionId = sessionId; }
    
    public Long getProcessStartTime() { return processStartTime; }
    public void setProcessStartTime(Long processStartTime) { this.processStartTime = processStartTime; }
    
    public String getProcessingNode() { return processingNode; }
    public void setProcessingNode(String processingNode) { this.processingNode = processingNode; }
} 