package com.example.academic_collab_platform_backend.mq;

import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import com.example.academic_collab_platform_backend.service.ChatService;
import com.example.academic_collab_platform_backend.util.RedisUtil;
import com.rabbitmq.client.Channel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

/**
 * 聊天消息消费者 - 负责从RabbitMQ队列中消费聊天消息并处理
 * 
 * 主要功能：
 * 1. 监听chat.message.queue队列
 * 2. 实现消息幂等性处理（避免重复处理）
 * 3. 调用业务逻辑处理消息（存库、推送WebSocket）
 * 4. 手动ACK确认消息处理结果
 * 5. 异常消息自动进入死信队列
 * 
 * 可靠性保证：
 * - 手动ACK：只有处理成功才确认消息
 * - 幂等性：相同messageId的消息只处理一次
 * - 死信队列：处理失败的消息进入DLQ，避免丢失
 * - Redis去重：24小时内相同消息ID不重复处理
 */
@Component
public class ChatMessageConsumer {
    private static final Logger log = LoggerFactory.getLogger(ChatMessageConsumer.class);

    /** 聊天业务服务 - 处理消息存储和推送 */
    private final ChatService chatService;
    
    /** Redis工具 - 用于幂等性控制 */
    private final RedisUtil redisUtil;

    public ChatMessageConsumer(ChatService chatService, RedisUtil redisUtil) {
        this.chatService = chatService;
        this.redisUtil = redisUtil;
        log.info("✅ ChatMessageConsumer initialized successfully");
    }

    /**
     * 消费聊天消息 - 从队列中接收并处理消息
     * 
     * 处理流程：
     * 1. 接收消息和元数据（delivery tag、message id等）
     * 2. 幂等性检查（通过Redis SETNX实现）
     * 3. 调用业务逻辑处理消息
     * 4. 手动ACK确认或NACK拒绝
     * 
     * @param request 反序列化后的聊天消息请求对象
     * @param message RabbitMQ原始消息对象（包含消息头等元数据）
     * @param channel RabbitMQ通道对象（用于ACK/NACK操作）
     */
    @RabbitListener(queues = ChatMQConstants.CHAT_QUEUE)
    public void onMessage(ChatMessageRequest request, Message message, Channel channel) throws Exception {
        // 1. 获取消息元数据
        long tag = message.getMessageProperties().getDeliveryTag();           // 消息标签（用于ACK/NACK）
        String messageId = (String) message.getMessageProperties().getHeaders().get("x-message-id");  // 消息唯一ID
        String dedupKey = "chat:msg:" + messageId;  // Redis去重键

        log.info("📨 [RabbitMQ] Received message - MessageId: {}, DeliveryTag: {}, SenderId: {}, ReceiverId: {}, Content: {}", 
                messageId, tag, request.getSenderId(), request.getReceiverId(),
                request.getContent().length() > 50 ? request.getContent().substring(0, 50) + "..." : request.getContent());

        try {
            // 2. 幂等性检查 - 避免重复处理相同消息
            if (messageId != null) {
                // 使用Redis SETNX命令：如果key不存在则设置，存在则返回false
                // 设置24小时过期时间，避免Redis内存泄漏
                Boolean firstTime = redisUtil.setIfAbsent(dedupKey, "1", 24, TimeUnit.HOURS);
                
                if (Boolean.FALSE.equals(firstTime)) {
                    // 消息已经处理过，直接确认并跳过处理
                    log.info("🔄 [RabbitMQ] Message already processed (idempotent) - MessageId: {}, skipping", messageId);
                    channel.basicAck(tag, false);  // 确认消息（不重新入队）
                    return;
                }
            }

            log.info("⚡ [RabbitMQ] Processing message - MessageId: {}", messageId);
            
            // 3. 调用业务处理逻辑
            // 这里会执行：消息存库 + WebSocket推送 + 缓存更新
            chatService.processAndDispatch(request);
            
            // 4. 处理成功，手动确认消息
            channel.basicAck(tag, false);  // false表示只确认当前消息，不批量确认
            log.info("✅ [RabbitMQ] Message processed successfully - MessageId: {}, DeliveryTag: {}", messageId, tag);
            
        } catch (Exception ex) {
            // 5. 处理失败的异常处理
            log.error("❌ [RabbitMQ] Failed to process message - MessageId: {}, DeliveryTag: {}, Error: {}", 
                    messageId, tag, ex.getMessage(), ex);
            
            // 拒绝消息并发送到死信队列
            // 参数说明：tag=消息标签, multiple=false不批量, requeue=false不重新入队
            channel.basicNack(tag, false, false);
            log.warn("💀 [RabbitMQ] Message sent to dead letter queue - MessageId: {}", messageId);
            
            // 重新抛出异常，让Spring知道处理失败（可选，用于监控统计）
            throw ex;
        }
    }
}
