package com.example.academic_collab_platform_backend.mq;

import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import com.example.academic_collab_platform_backend.dto.ChatMessageResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.core.MessageDeliveryMode;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.amqp.core.MessagePostProcessor;
import org.springframework.stereotype.Component;

import java.util.UUID;

/**
 * 聊天消息生产者 - 负责将聊天消息发送到RabbitMQ队列
 * 
 * 主要功能：
 * 1. 接收ChatMessageRequest对象
 * 2. 为消息生成唯一ID（用于幂等性控制）
 * 3. 设置消息持久化属性
 * 4. 发送到指定的交换机和路由键
 * 
 * 使用场景：
 * - 用户发送聊天消息时，先入队列，再异步处理
 * - 提升响应速度，避免直接处理阻塞用户操作
 */
@Component
public class ChatMessageProducer {
    private static final Logger log = LoggerFactory.getLogger(ChatMessageProducer.class);
    
    /** RabbitMQ发送模板 */
    private final RabbitTemplate rabbitTemplate;

    public ChatMessageProducer(RabbitTemplate rabbitTemplate) {
        this.rabbitTemplate = rabbitTemplate;
        log.info("✅ ChatMessageProducer initialized successfully");
    }

    /**
     * 发布聊天消息到队列
     * 
     * 处理流程：
     * 1. 生成消息唯一ID（用于消费端去重）
     * 2. 设置消息头信息（ID、持久化标志）
     * 3. 发送到chat.exchange交换机
     * 4. 根据chat.message路由键路由到chat.message.queue队列
     * 
     * @param request 聊天消息请求对象（包含发送者、接收者、内容等）
     */
    public void publish(ChatMessageRequest request) {
        // 1. 使用客户端生成的消息ID，避免重复生成UUID
        String messageId = request.getClientMsgId();
        
        // 如果客户端没有提供ID，则生成一个（兜底保护）
        if (messageId == null || messageId.trim().isEmpty()) {
            messageId = UUID.randomUUID().toString();
            log.warn("⚠️ [RabbitMQ] Client message ID is missing, generated new ID: {}", messageId);
        }
        
        log.info("🚀 [RabbitMQ] Publishing message to queue - ClientMsgId: {}, SenderId: {}, ReceiverId: {}, Content: {}", 
                messageId, request.getSenderId(), request.getReceiverId(), 
                request.getContent().length() > 50 ? request.getContent().substring(0, 50) + "..." : request.getContent());
        
        // 2. 设置消息属性：使用客户端消息ID + 持久化
        final String finalMessageId = messageId;  // lambda表达式需要final变量
        MessagePostProcessor headers = message -> {
            // 使用客户端提供的消息ID，保持端到端的幂等性
            message.getMessageProperties().setHeader("x-message-id", finalMessageId);
            
            // 设置消息持久化：即使RabbitMQ重启，消息也不会丢失
            message.getMessageProperties().setDeliveryMode(MessageDeliveryMode.PERSISTENT);
            
            return message;
        };
        
        // 3. 发送消息到RabbitMQ
        try {
            rabbitTemplate.convertAndSend(
                    ChatMQConstants.CHAT_EXCHANGE,     // 目标交换机
                    ChatMQConstants.CHAT_ROUTING_KEY,  // 路由键
                    request,                           // 消息体（自动序列化为JSON）
                    headers                            // 消息属性处理器
            );
            
            log.info("✅ [RabbitMQ] Message published successfully - Exchange: {}, RoutingKey: {}, ClientMsgId: {}", 
                    ChatMQConstants.CHAT_EXCHANGE, ChatMQConstants.CHAT_ROUTING_KEY, finalMessageId);
                    
        } catch (Exception e) {
            // 发送失败时记录详细错误信息，并重新抛出异常
            log.error("❌ [RabbitMQ] Failed to publish message - ClientMsgId: {}, Error: {}", finalMessageId, e.getMessage(), e);
            throw e;  // 重新抛出，让调用方知道发送失败
        }
    }
}