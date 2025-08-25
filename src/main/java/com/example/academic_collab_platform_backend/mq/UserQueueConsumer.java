package com.example.academic_collab_platform_backend.mq;

import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import com.example.academic_collab_platform_backend.service.ChatService;
import com.example.academic_collab_platform_backend.util.RedisUtil;
import com.rabbitmq.client.Channel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.core.RabbitAdmin;
import org.springframework.amqp.rabbit.listener.SimpleMessageListenerContainer;
import org.springframework.amqp.rabbit.listener.api.ChannelAwareMessageListener;
import org.springframework.amqp.support.converter.MessageConverter;
import org.springframework.amqp.rabbit.connection.ConnectionFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.Map;

/**
 * 用户队列消费者管理器 - 动态管理用户专属队列的消费者
 * 
 * 核心功能：
 * 1. 用户上线时创建消费者，监听用户专属队列
 * 2. 用户下线时停止消费者，释放资源
 * 3. 处理用户队列中的离线消息
 * 4. 支持消息幂等性处理
 */
@Component
public class UserQueueConsumer {
    private static final Logger log = LoggerFactory.getLogger(UserQueueConsumer.class);
    
    private final ConnectionFactory connectionFactory;
    private final ChatService chatService;
    private final RedisUtil redisUtil;
    private final MessageConverter messageConverter;
    private final UserQueueManager userQueueManager;
    
    /** 用户消费者容器映射 */
    private final Map<Long, SimpleMessageListenerContainer> userContainers = new ConcurrentHashMap<>();
    
    /** 是否启用用户队列模式 */
    @Value("${chat.rabbitmq.mode:user-queue}")
    private String rabbitmqMode;
    
    public UserQueueConsumer(ConnectionFactory connectionFactory, 
                           ChatService chatService, 
                           RedisUtil redisUtil,
                           MessageConverter messageConverter,
                           UserQueueManager userQueueManager) {
        this.connectionFactory = connectionFactory;
        this.chatService = chatService;
        this.redisUtil = redisUtil;
        this.messageConverter = messageConverter;
        this.userQueueManager = userQueueManager;
        log.info("🚀 [UserQueueConsumer] 用户队列消费者管理器初始化完成");
    }
    
    @PostConstruct
    public void init() {
        log.info("📋 [UserQueueConsumer] 当前消息模式: {}", rabbitmqMode);
        log.info("🔧 [UserQueueConsumer] ConnectionFactory: {}", connectionFactory.getClass().getName());
        log.info("🔧 [UserQueueConsumer] MessageConverter: {}", messageConverter.getClass().getName());
        
        if (!"user-queue".equalsIgnoreCase(rabbitmqMode)) {
            log.info("⏭️ [UserQueueConsumer] 非用户队列模式，跳过初始化");
            return;
        }
        
        // TODO: 系统启动时，可以从Redis中恢复在线用户的消费者
        log.info("✅ [UserQueueConsumer] 用户队列消费者管理器就绪");
    }
    
    /**
     * 用户上线时启动消费者
     */
    public void startConsumerForUser(Long userId) {
        log.info("🔍 [UserQueueConsumer] 检查消息模式: rabbitmqMode={}, userId={}", rabbitmqMode, userId);
        
        if (!"user-queue".equalsIgnoreCase(rabbitmqMode)) {
            log.warn("⏭️ [UserQueueConsumer] 非用户队列模式，跳过用户消费者启动: userId={}, mode={}", userId, rabbitmqMode);
            return;
        }
        
        if (userContainers.containsKey(userId)) {
            log.info("📋 [UserQueueConsumer] 用户消费者已存在: userId={}", userId);
            return;
        }
        
        try {
            // 确保用户队列存在
            String queueName = userQueueManager.createUserQueue(userId);
            
            // 创建消息监听容器
            SimpleMessageListenerContainer container = new SimpleMessageListenerContainer(connectionFactory);
            container.setQueueNames(queueName);
            container.setConcurrentConsumers(1);  // 每个用户队列只需要一个消费者
            container.setMaxConcurrentConsumers(1);
            container.setAcknowledgeMode(org.springframework.amqp.core.AcknowledgeMode.MANUAL);
            
            // 设置消息监听器
            container.setMessageListener(new UserMessageListener(userId));
            
            // 启动容器
            container.start();
            
            // 缓存容器
            userContainers.put(userId, container);
            
            log.info("✅ [UserQueueConsumer] 用户消费者启动成功: userId={}, queueName={}", userId, queueName);
            
        } catch (Exception e) {
            log.error("❌ [UserQueueConsumer] 用户消费者启动失败: userId={}, error={}", userId, e.getMessage(), e);
            throw new RuntimeException("启动用户消费者失败", e);
        }
    }
    
    /**
     * 用户下线时停止消费者
     */
    public void stopConsumerForUser(Long userId) {
        SimpleMessageListenerContainer container = userContainers.remove(userId);
        if (container != null) {
            try {
                container.stop();
                log.info("🛑 [UserQueueConsumer] 用户消费者停止成功: userId={}", userId);
            } catch (Exception e) {
                log.error("❌ [UserQueueConsumer] 用户消费者停止失败: userId={}, error={}", userId, e.getMessage());
            }
        }
    }
    
    /**
     * 获取用户队列中的消息数量
     */
    public Long getUserQueueMessageCount(Long userId) {
        return userQueueManager.getQueueMessageCount(userId);
    }
    
    /**
     * 检查用户消费者是否在运行
     */
    public boolean isConsumerRunning(Long userId) {
        SimpleMessageListenerContainer container = userContainers.get(userId);
        return container != null && container.isRunning();
    }
    
    @PreDestroy
    public void shutdown() {
        log.info("🔄 [UserQueueConsumer] 关闭所有用户消费者...");
        userContainers.forEach((userId, container) -> {
            try {
                container.stop();
                log.info("🛑 [UserQueueConsumer] 用户消费者已关闭: userId={}", userId);
            } catch (Exception e) {
                log.error("❌ [UserQueueConsumer] 关闭用户消费者失败: userId={}, error={}", userId, e.getMessage());
            }
        });
        userContainers.clear();
        log.info("✅ [UserQueueConsumer] 所有用户消费者已关闭");
    }
    
    /**
     * 用户专属消息监听器
     */
    private class UserMessageListener implements ChannelAwareMessageListener {
        private final Long userId;
        
        public UserMessageListener(Long userId) {
            this.userId = userId;
        }
        
        @Override
        public void onMessage(Message message, Channel channel) throws Exception {
            long tag = message.getMessageProperties().getDeliveryTag();
            String messageId = (String) message.getMessageProperties().getHeaders().get("x-message-id");
            String dedupKey = "chat:msg:user:" + userId + ":" + messageId;
            
            try {
                log.info("📨 [UserQueue] 收到用户队列消息: userId={}, messageId={}, deliveryTag={}", 
                        userId, messageId, tag);
                
                // 反序列化消息
                ChatMessageRequest request = (ChatMessageRequest) messageConverter.fromMessage(message);
                
                // 幂等性检查
                if (messageId != null) {
                    Boolean firstTime = redisUtil.setIfAbsent(dedupKey, "1", 24, TimeUnit.HOURS);
                    if (Boolean.FALSE.equals(firstTime)) {
                        log.info("🔄 [UserQueue] 消息已处理过: userId={}, messageId={}", userId, messageId);
                        channel.basicAck(tag, false);
                        return;
                    }
                }
                
                log.info("⚡ [UserQueue] 处理用户队列消息: userId={}, messageId={}, senderId={}, receiverId={}, content={}", 
                        userId, messageId, request.getSenderId(), request.getReceiverId(), request.getContent());
                
                // 调用业务处理逻辑
                log.info("🔄 [UserQueue] 调用业务处理逻辑: userId={}, messageId={}", userId, messageId);
                chatService.processAndDispatch(request);
                log.info("🎯 [UserQueue] 业务处理逻辑完成: userId={}, messageId={}", userId, messageId);
                
                // 确认消息
                channel.basicAck(tag, false);
                
                log.info("✅ [UserQueue] 用户队列消息处理成功: userId={}, messageId={}, deliveryTag={}", 
                        userId, messageId, tag);
                
            } catch (Exception e) {
                log.error("❌ [UserQueue] 用户队列消息处理失败: userId={}, messageId={}, deliveryTag={}, error={}", 
                        userId, messageId, tag, e.getMessage(), e);
                
                // 拒绝消息并发送到死信队列
                channel.basicNack(tag, false, false);
                log.warn("💀 [UserQueue] 消息发送到死信队列: userId={}, messageId={}", userId, messageId);
                
                throw e;
            }
        }
    }
}
