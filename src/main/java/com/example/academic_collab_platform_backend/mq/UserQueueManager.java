package com.example.academic_collab_platform_backend.mq;

import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.core.*;
import org.springframework.amqp.rabbit.core.RabbitAdmin;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.amqp.core.MessageDeliveryMode;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 用户队列管理器 - 解决离线消息问题
 * 
 * 核心思路：
 * 1. 为每个用户创建专属的持久化队列
 * 2. 消息直接路由到接收者的队列中
 * 3. 用户上线时消费自己队列中的所有消息
 * 4. 队列支持持久化，服务重启后消息不丢失
 */
@Component
public class UserQueueManager {
    private static final Logger log = LoggerFactory.getLogger(UserQueueManager.class);
    
    private final RabbitAdmin rabbitAdmin;
    private final RabbitTemplate rabbitTemplate;
    private final DirectExchange chatExchange;
    
    /** 缓存已创建的队列，避免重复创建 */
    private final Map<Long, String> userQueueCache = new ConcurrentHashMap<>();
    
    public UserQueueManager(RabbitAdmin rabbitAdmin, RabbitTemplate rabbitTemplate, DirectExchange chatExchange) {
        this.rabbitAdmin = rabbitAdmin;
        this.rabbitTemplate = rabbitTemplate;
        this.chatExchange = chatExchange;
        log.info("🚀 [UserQueueManager] 用户队列管理器初始化完成");
    }
    
    /**
     * 为用户创建专属队列
     * 队列命名：chat.user.{userId}.queue
     */
    public String createUserQueue(Long userId) {
        String queueName = getUserQueueName(userId);
        
        // 检查缓存，避免重复创建
        if (userQueueCache.containsKey(userId)) {
            log.debug("📋 [UserQueue] 队列已存在: userId={}, queueName={}", userId, queueName);
            return queueName;
        }
        
        try {
            // 创建持久化队列，带死信队列配置
            Map<String, Object> args = new HashMap<>();
            args.put("x-dead-letter-exchange", ChatMQConstants.DLX);
            args.put("x-dead-letter-routing-key", ChatMQConstants.DLQ_ROUTING_KEY);
            
            Queue queue = QueueBuilder.durable(queueName)
                    .withArguments(args)
                    .build();
            
            // 声明队列
            rabbitAdmin.declareQueue(queue);
            
            // 绑定到交换机，路由键为用户ID
            Binding binding = BindingBuilder.bind(queue)
                    .to(chatExchange)
                    .with(getUserRoutingKey(userId));
            
            rabbitAdmin.declareBinding(binding);
            
            // 加入缓存
            userQueueCache.put(userId, queueName);
            
            log.info("✅ [UserQueue] 用户队列创建成功: userId={}, queueName={}, routingKey={}", 
                    userId, queueName, getUserRoutingKey(userId));
            
            return queueName;
            
        } catch (Exception e) {
            log.error("❌ [UserQueue] 用户队列创建失败: userId={}, error={}", userId, e.getMessage(), e);
            throw new RuntimeException("创建用户队列失败", e);
        }
    }
    
    /**
     * 发送消息到用户专属队列
     */
    public void sendToUserQueue(Long receiverId, ChatMessageRequest request) {
        // 确保接收者队列存在
        createUserQueue(receiverId);
        
        String routingKey = getUserRoutingKey(receiverId);
        
        try {
            // 设置消息持久化
            rabbitTemplate.convertAndSend(
                chatExchange.getName(),
                routingKey,
                request,
                message -> {
                    message.getMessageProperties().setDeliveryMode(MessageDeliveryMode.PERSISTENT);
                    message.getMessageProperties().setHeader("x-message-id", request.getClientMsgId());
                    message.getMessageProperties().setHeader("x-target-user-id", receiverId);
                    return message;
                }
            );
            
            log.info("📤 [UserQueue] 消息发送到用户队列: receiverId={}, routingKey={}, clientMsgId={}", 
                    receiverId, routingKey, request.getClientMsgId());
            
        } catch (Exception e) {
            log.error("❌ [UserQueue] 发送消息到用户队列失败: receiverId={}, error={}", receiverId, e.getMessage(), e);
            throw e;
        }
    }
    
    /**
     * 删除用户队列（用户注销时调用）
     */
    public void deleteUserQueue(Long userId) {
        String queueName = getUserQueueName(userId);
        try {
            rabbitAdmin.deleteQueue(queueName);
            userQueueCache.remove(userId);
            log.info("🗑️ [UserQueue] 用户队列删除成功: userId={}, queueName={}", userId, queueName);
        } catch (Exception e) {
            log.error("❌ [UserQueue] 用户队列删除失败: userId={}, error={}", userId, e.getMessage(), e);
        }
    }
    
    /**
     * 获取用户队列名称
     */
    public String getUserQueueName(Long userId) {
        return "chat.user." + userId + ".queue";
    }
    
    /**
     * 获取用户路由键
     */
    public String getUserRoutingKey(Long userId) {
        return "chat.user." + userId;
    }
    
    /**
     * 检查用户队列是否存在
     */
    public boolean isUserQueueExists(Long userId) {
        return userQueueCache.containsKey(userId);
    }
    
    /**
     * 获取队列中的消息数量（用于监控）
     */
    public Long getQueueMessageCount(Long userId) {
        String queueName = getUserQueueName(userId);
        try {
            Properties properties = rabbitAdmin.getQueueProperties(queueName);
            if (properties != null) {
                return (Long) properties.get("QUEUE_MESSAGE_COUNT");
            }
        } catch (Exception e) {
            log.error("❌ [UserQueue] 获取队列消息数量失败: userId={}, error={}", userId, e.getMessage());
        }
        return 0L;
    }
}
