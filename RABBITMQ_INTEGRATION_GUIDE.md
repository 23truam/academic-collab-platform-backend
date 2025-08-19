# RabbitMQ 集成使用指南 🐰

## 功能概述
已成功集成 RabbitMQ 到聊天系统，支持异步消息处理，提升系统性能和可扩展性。

## 日志监控说明

### 🚀 启动时的日志提示
当应用启动时，你将在控制台看到以下关键日志：

```
🔧 [RabbitMQ] Initializing AMQP Admin
🔗 [RabbitMQ] Creating chat exchange: chat.exchange  
📬 [RabbitMQ] Creating chat queue: chat.message.queue
🔗 [RabbitMQ] Binding chat queue to exchange with routing key: chat.message
✅ [RabbitMQ] Chat messaging system initialized successfully!
📊 [RabbitMQ] Configuration Summary:
   - Exchange: chat.exchange
   - Queue: chat.message.queue  
   - Routing Key: chat.message
   - Dead Letter Exchange: chat.dlx
   - Dead Letter Queue: chat.message.dlq
✅ ChatMessageProducer initialized successfully
✅ ChatMessageConsumer initialized successfully
```

### 📨 消息发送时的日志 (RabbitMQ模式)
```
🐰 [ChatService] Using RabbitMQ mode - SenderId: 1, ReceiverId: 2, ClientMsgId: xxx-xxx-xxx
🚀 [RabbitMQ] Publishing message to queue - MessageId: yyy-yyy-yyy, SenderId: 1, ReceiverId: 2, Content: Hello...
✅ [RabbitMQ] Message published successfully - Exchange: chat.exchange, RoutingKey: chat.message, MessageId: yyy-yyy-yyy
✅ [ChatService] Message queued successfully (RabbitMQ mode) - ClientMsgId: xxx-xxx-xxx
```

### 📮 消息消费时的日志
```
📨 [RabbitMQ] Received message - MessageId: yyy-yyy-yyy, DeliveryTag: 1, SenderId: 1, ReceiverId: 2, Content: Hello...
⚡ [RabbitMQ] Processing message - MessageId: yyy-yyy-yyy
📮 [ChatService] Processing message from MQ - SenderId: 1, ReceiverId: 2, ClientMsgId: xxx-xxx-xxx
✅ [ChatService] Message processed and dispatched successfully from MQ - MessageId: 123, ClientMsgId: xxx-xxx-xxx
✅ [RabbitMQ] Message processed successfully - MessageId: yyy-yyy-yyy, DeliveryTag: 1
```

### 🔄 直发模式的日志
```
🔄 [ChatService] Using Direct mode - SenderId: 1, ReceiverId: 2, ClientMsgId: xxx-xxx-xxx
```

### ❌ 异常处理日志
```
❌ [RabbitMQ] Failed to publish message - MessageId: yyy-yyy-yyy, Error: Connection refused
❌ [RabbitMQ] Failed to process message - MessageId: yyy-yyy-yyy, DeliveryTag: 1, Error: Database connection failed
💀 [RabbitMQ] Message sent to dead letter queue - MessageId: yyy-yyy-yyy
```

## 使用方式

### 1. 确保 RabbitMQ 运行
```bash
# Windows
rabbitmq-server

# 或通过服务管理器启动 RabbitMQ 服务
```

### 2. 检查配置
确保 `application.yml` 中的配置正确：
```yaml
chat:
  messaging:
    mode: rabbit  # 使用 RabbitMQ 模式
    # mode: direct  # 切换回直发模式
```

### 3. 启动应用
```bash
mvn spring-boot:run
```

### 4. 验证 RabbitMQ 集成
- **管理界面**: 访问 http://localhost:15672 (guest/guest)
- **日志验证**: 查看控制台是否出现上述日志
- **队列验证**: 在 RabbitMQ 管理界面查看队列 `chat.message.queue` 是否创建成功

### 5. 测试消息发送
通过前端或 API 发送聊天消息，观察：
- 控制台日志显示 RabbitMQ 模式
- 消息成功发布和消费
- WebSocket 推送正常工作

## 模式切换

### 切换到 RabbitMQ 模式
```yaml
chat:
  messaging:
    mode: rabbit
```

### 切换到直发模式（回退）
```yaml
chat:
  messaging:
    mode: direct
```

## 监控要点

1. **启动成功**: 看到所有 ✅ 初始化日志
2. **消息流转**: 发送时看到 🚀 发布日志，处理时看到 📨 接收日志
3. **性能提升**: RabbitMQ 模式下接口响应更快（只返回入队确认）
4. **异常处理**: 关注 ❌ 和 💀 日志，表示需要处理的问题
5. **队列监控**: 在 RabbitMQ 管理界面监控队列长度和消费速率

## 故障排查

### 常见问题
- **连接失败**: 检查 RabbitMQ 服务是否启动
- **队列未创建**: 检查应用启动日志中的配置信息
- **消息未消费**: 检查消费者日志和 DLQ 中是否有消息
- **性能问题**: 调整 `prefetch`、`concurrency` 参数

### 日志级别调整
在 `application.yml` 中添加：
```yaml
logging:
  level:
    com.example.academic_collab_platform_backend.mq: DEBUG
    com.example.academic_collab_platform_backend.service.impl.ChatServiceImpl: DEBUG
```

现在你的聊天系统已经完全集成了 RabbitMQ，通过丰富的日志可以清楚地监控消息的流转过程！🎉
