package com.example.academic_collab_platform_backend.mq;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.core.*;
import org.springframework.amqp.rabbit.annotation.EnableRabbit;
import org.springframework.amqp.rabbit.connection.ConnectionFactory;
import org.springframework.amqp.rabbit.core.RabbitAdmin;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.HashMap;
import java.util.Map;

/**
 * RabbitMQ配置类 - 聊天消息队列的核心配置
 * 
 * 主要功能：
 * 1. 创建交换机、队列、绑定关系
 * 2. 配置消息序列化方式（JSON格式）
 * 3. 设置死信队列处理异常消息
 * 4. 配置消息可靠性（持久化、确认机制）
 */
@Configuration
@EnableRabbit  // 启用RabbitMQ支持
public class RabbitMQConfig {
    private static final Logger log = LoggerFactory.getLogger(RabbitMQConfig.class);

    /**
     * AMQP管理器 - 用于声明和管理队列、交换机等
     */
    @Bean
    public AmqpAdmin amqpAdmin(ConnectionFactory connectionFactory) {
        log.info("🔧 [RabbitMQ] Initializing AMQP Admin");
        return new RabbitAdmin(connectionFactory);
    }

    /**
     * JSON消息转换器 - 将Java对象转换为JSON发送，接收时再转回Java对象
     * 这样可以直接发送ChatMessageRequest对象，无需手动序列化
     */
    @Bean
    public Jackson2JsonMessageConverter jackson2JsonMessageConverter() {
        return new Jackson2JsonMessageConverter();
    }

    /**
     * RabbitMQ模板 - 用于发送消息的核心工具
     * 配置了消息可靠性保证：发送确认 + 路由失败处理
     */
    @Bean
    public RabbitTemplate rabbitTemplate(ConnectionFactory connectionFactory,
                                         Jackson2JsonMessageConverter converter) {
        RabbitTemplate template = new RabbitTemplate(connectionFactory);
        
        // 设置JSON转换器，支持对象自动序列化
        template.setMessageConverter(converter);
        
        // 开启mandatory模式：如果消息无法路由到队列，会触发return回调
        template.setMandatory(true);
        
        // 发送确认回调：确认消息是否成功到达交换机
        template.setConfirmCallback((correlationData, ack, cause) -> {
            if (ack) {
                // 消息成功到达交换机
                log.debug("✅ [RabbitMQ] Message confirmed: {}", correlationData);
            } else {
                // 消息未到达交换机，记录错误
                log.error("❌ [RabbitMQ] Message not confirmed: {}, cause: {}", correlationData, cause);
            }
        });
        
        // 路由失败回调：消息到达交换机但无法路由到队列时触发
        template.setReturnsCallback(returned -> {
            log.error("🔄 [RabbitMQ] Message returned: exchange={}, routingKey={}, replyText={}", 
                    returned.getExchange(), returned.getRoutingKey(), returned.getReplyText());
        });
        
        return template;
    }

    // ========== 交换机配置 ==========
    
    /**
     * 聊天消息主交换机 - Direct类型
     * - durable=true: 持久化，服务器重启后交换机仍存在
     * - autoDelete=false: 不自动删除，即使没有队列绑定也保留
     */
    @Bean
    public DirectExchange chatExchange() {
        log.info("🔗 [RabbitMQ] Creating chat exchange: {}", ChatMQConstants.CHAT_EXCHANGE);
        return new DirectExchange(ChatMQConstants.CHAT_EXCHANGE, true, false);
    }

    /**
     * 死信交换机 - 处理异常消息
     * 当主队列中的消息处理失败时，会被路由到这个交换机
     */
    @Bean
    public DirectExchange chatDLX() {
        log.info("💀 [RabbitMQ] Creating dead letter exchange: {}", ChatMQConstants.DLX);
        return new DirectExchange(ChatMQConstants.DLX, true, false);
    }

    // ========== 队列配置 ==========
    
    /**
     * 死信队列 - 存储处理失败的消息
     * 管理员可以查看这个队列来排查问题，决定是否重新处理
     */
    @Bean
    public Queue chatDLQ() {
        log.info("🪦 [RabbitMQ] Creating dead letter queue: {}", ChatMQConstants.DLQ);
        return QueueBuilder.durable(ChatMQConstants.DLQ).build();
    }

    /**
     * 死信队列绑定 - 将死信队列绑定到死信交换机
     */
    @Bean
    public Binding bindDLQ() {
        log.info("🔗 [RabbitMQ] Binding dead letter queue to exchange");
        return BindingBuilder.bind(chatDLQ()).to(chatDLX()).with(ChatMQConstants.DLQ_ROUTING_KEY);
    }

    /**
     * 聊天消息主队列 - 存储待处理的聊天消息
     * 
     * 关键配置：
     * - durable: 持久化队列，服务器重启后队列仍存在
     * - x-dead-letter-exchange: 指定死信交换机，处理失败的消息会发送到这里
     * - x-dead-letter-routing-key: 指定死信路由键
     * 
     * 消息处理流程：
     * 1. 正常情况：消息 → chatQueue → 消费者处理 → ACK确认
     * 2. 异常情况：消息 → chatQueue → 处理失败 → 发送到死信交换机 → 死信队列
     */
    @Bean
    public Queue chatQueue() {
        log.info("📬 [RabbitMQ] Creating chat queue: {}", ChatMQConstants.CHAT_QUEUE);
        
        // 配置死信队列参数
        Map<String, Object> args = new HashMap<>();
        args.put("x-dead-letter-exchange", ChatMQConstants.DLX);      // 死信交换机
        args.put("x-dead-letter-routing-key", ChatMQConstants.DLQ_ROUTING_KEY);  // 死信路由键
        
        return QueueBuilder.durable(ChatMQConstants.CHAT_QUEUE)
                .withArguments(args)  // 应用死信配置
                .build();
    }

    /**
     * 主队列绑定 - 将聊天队列绑定到主交换机
     * 
     * 绑定关系：chat.exchange → (routing key: chat.message) → chat.message.queue
     * 这样发送到chat.exchange且routing key为chat.message的消息都会进入chat.message.queue
     */
    @Bean
    public Binding bindChatQueue() {
        log.info("🔗 [RabbitMQ] Binding chat queue to exchange with routing key: {}", ChatMQConstants.CHAT_ROUTING_KEY);
        
        // 配置完成，输出总结信息
        log.info("✅ [RabbitMQ] Chat messaging system initialized successfully!");
        log.info("📊 [RabbitMQ] Configuration Summary:");
        log.info("   - 主交换机: {}", ChatMQConstants.CHAT_EXCHANGE);
        log.info("   - 主队列: {}", ChatMQConstants.CHAT_QUEUE);
        log.info("   - 路由键: {}", ChatMQConstants.CHAT_ROUTING_KEY);
        log.info("   - 死信交换机: {}", ChatMQConstants.DLX);
        log.info("   - 死信队列: {}", ChatMQConstants.DLQ);
        log.info("💡 [RabbitMQ] 消息流转路径: 发送者 → {} → {} → 消费者", 
                ChatMQConstants.CHAT_EXCHANGE, ChatMQConstants.CHAT_QUEUE);
        
        return BindingBuilder.bind(chatQueue())
                .to(chatExchange())
                .with(ChatMQConstants.CHAT_ROUTING_KEY);
    }
}