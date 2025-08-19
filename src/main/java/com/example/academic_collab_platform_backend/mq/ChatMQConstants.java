package com.example.academic_collab_platform_backend.mq;

/**
 * 聊天消息队列常量配置
 * 
 * RabbitMQ 基本概念：
 * - Exchange（交换机）：消息的路由中心，决定消息发送到哪个队列
 * - Queue（队列）：存储消息的容器，消费者从队列中取消息
 * - Routing Key（路由键）：交换机用来决定消息路由到哪个队列的关键字
 * - Dead Letter（死信）：处理失败的消息会被发送到死信队列，避免消息丢失
 */
public final class ChatMQConstants {
    
    // ========== 主要消息流转相关 ==========
    
    /** 聊天消息交换机名称 - 消息发布的入口点 */
    public static final String CHAT_EXCHANGE = "chat.exchange";
    
    /** 聊天消息路由键 - 用于将消息路由到正确的队列 */
    public static final String CHAT_ROUTING_KEY = "chat.message";
    
    /** 聊天消息队列名称 - 存储待处理的聊天消息 */
    public static final String CHAT_QUEUE = "chat.message.queue";

    // ========== 死信队列相关（异常处理） ==========
    
    /** 死信交换机名称 - 处理失败消息的交换机 */
    public static final String DLX = "chat.dlx";
    
    /** 死信队列名称 - 存储处理失败的消息，便于排查问题 */
    public static final String DLQ = "chat.message.dlq";
    
    /** 死信路由键 - 将失败消息路由到死信队列 */
    public static final String DLQ_ROUTING_KEY = "chat.message.dlq";

    /** 私有构造函数，防止实例化常量类 */
    private ChatMQConstants() {}
}
