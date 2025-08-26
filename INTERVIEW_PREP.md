## 学术协作平台后端（Spring Boot）面试准备文档

本项目基于 Spring Boot + MyBatis-Plus + MySQL + Redis + WebSocket(STOMP) + RabbitMQ + JWT + Spring Security，提供学术检索、合作分析、即时通信等完整功能。本文面向技术面试，按模块说明：关键逻辑、为什么这样设计、如何实现、可选替代方案，并给出少量关键代码片段。

---

## 1. 用户认证与权限管理（JWT + Spring Security + WebSocket 鉴权）

- **关键逻辑**
  - 无状态 HTTP 鉴权：前端携带 `Authorization: Bearer <token>`；过滤器解析并设置 `SecurityContext`。
  - WebSocket 鉴权：握手阶段读取 `token`，解析 `userId` 绑定到会话属性，再通过自定义 `HandshakeHandler` 映射为 `Principal`，实现点对点目的地推送。
- **为何选型**
  - JWT 天然适配无状态与横向扩展，避免 Session 共享。
  - Spring Security 对过滤器链与端点放行控制简单清晰；STOMP 的 user 目的地提供面向用户的队列语义。
- **实现要点**
  - 生成与校验 JWT；注册自定义 `OncePerRequestFilter`；在 WebSocket 握手拦截器中解析 `userId`，并通过 `DefaultHandshakeHandler` 返回 `Principal`。
- **关键代码**

```java
// JwtUtil - 生成与校验 Token
public String generateToken(String username, String userId) {
    Map<String, Object> claims = new HashMap<>();
    claims.put("userId", userId);
    return Jwts.builder()
            .setClaims(claims)
            .setSubject(username)
            .setIssuedAt(new Date())
            .setExpiration(new Date(System.currentTimeMillis() + expiration))
            .signWith(getSigningKey(), SignatureAlgorithm.HS256)
            .compact();
}
```

```java
// SecurityConfig - 无状态鉴权与过滤器注册
http.cors().and().csrf().disable()
   .sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS)
   .and().authorizeRequests()
   .antMatchers("/api/auth/**", "/ws/**", "/api/collaboration/**").permitAll()
   .anyRequest().authenticated();
http.addFilterBefore(new JwtAuthenticationFilter(jwtUtil), UsernamePasswordAuthenticationFilter.class);
```

```java
// WebSocketConfig - 握手拦截 + user 目的地
registry.addEndpoint("/ws")
        .addInterceptors(new UserIdHandshakeInterceptor(jwtUtil))
        .setHandshakeHandler(new DefaultHandshakeHandler(){
            protected Principal determineUser(ServerHttpRequest req, WebSocketHandler h, Map<String,Object> attrs){
                String userId = (String) attrs.get("userId");
                return () -> userId;
            }
        })
        .setAllowedOriginPatterns("*")
        .withSockJS();
registry.setApplicationDestinationPrefixes("/app");
registry.enableSimpleBroker("/topic", "/queue");
registry.setUserDestinationPrefix("/user");
```

- **替代方案**：OAuth2/OIDC；将 token 放置于握手头/子协议；使用 Spring Security 新式 `SecurityFilterChain` 替代旧适配器。

---

## 2. Redis 缓存与分布式锁

- **关键逻辑**
  - 统一 `RedisTemplate<String,String>` 提升序列化性能；封装对象读写；以 `SETNX + EX` 获取锁、Lua 原子释放锁；List 用作单用户顺序队列。
- **为何选型**
  - 热点数据缓存显著降低数据库压力；分布式锁保障并发一致性；列表结构实现轻量有序队列。
- **实现要点**
  - `tryLock/releaseLock/executeWithLock` 通用封装；`lpush/rpop` 构建用户专属队列；统一 `ObjectMapper`（含 JavaTimeModule）。
- **关键代码**

```java
// 获取/释放分布式锁
public boolean tryLock(String key, String val, long timeout, TimeUnit unit) {
    return Boolean.TRUE.equals(redisTemplate.opsForValue().setIfAbsent(key, val, timeout, unit));
}
public boolean releaseLock(String key, String val){
    String lua = "if redis.call('get', KEYS[1]) == ARGV[1] then return redis.call('del', KEYS[1]) else return 0 end";
    DefaultRedisScript<Long> script = new DefaultRedisScript<>(lua, Long.class);
    Long r = redisTemplate.execute(script, Arrays.asList(key), val);
    return r != null && r == 1L;
}
```

- **替代方案**：Spring Cache + Caffeine 二级缓存；Redisson 锁；限流可用 Guava RateLimiter；消息中间件替代列表队列。

---

## 3. 搜索历史管理（异步 + 分布式锁 + 延迟双删）

- **关键逻辑**
  - 同步落库，异步处理用户队列；同一用户通过 Redis List 保序；Redis 分布式锁串行化批处理；延迟双删保障缓存一致性。
- **为何选型**
  - 保障搜索主流程低延迟；避免并发覆盖；防止缓存脏读。
- **实现要点**
  - `addSearchHistory`：立即入库 + `lpush` 入队 + 线程池异步消费。
  - `processUserSearchQueue`：获取锁后批量 `rpop`、合并去重、回填缓存。
  - `clearHistory`：先删缓存、删库，再 500ms 延迟二次删除。
- **关键代码**

```java
// 入队 + 异步处理
searchHistoryMapper.insert(history);
redisUtil.lpush("user_search_queue:" + history.getUserId(), objectMapper.writeValueAsString(history));
taskExecutor.execute(() -> processUserSearchQueue(history.getUserId()));
```

```java
// 分布式锁保护的批处理
boolean lock = redisUtil.tryLock(lockKey, lockValue, 10, TimeUnit.SECONDS);
if (lock) {
    try { /* 批量 rpop -> 合并去重 -> 回填缓存 */ } finally { redisUtil.releaseLock(lockKey, lockValue); }
}
```

- **替代方案**：DB 幂等键 + 事务；Kafka/RabbitMQ 串行化；布隆过滤器去重；Redisson 延迟队列实现双删。

---

## 4. 学术搜索与数据管理（MyBatis-Plus + 缓存）

- **关键逻辑**
  - `QueryWrapper` 构建动态条件；分页；作者/论文对象缓存到 Redis（全量预热 + 局部按需）。
- **为何选型**
  - MyBatis-Plus 降低样板代码成本；Redis 提升命中与响应。
- **关键代码**

```java
// 论文搜索 - 标题/摘要模糊 + 分页
QueryWrapper<Paper> w = new QueryWrapper<>();
if (query != null && !query.isEmpty()) {
    w.lambda().like(Paper::getTitle, query).or().like(Paper::getAbstractText, query);
}
return paperMapper.selectPage(new Page<>(page, size), w);
```

```java
// 作者全量缓存预热（简化版）
List<Author> all = authorMapper.selectList(null);
redisUtil.setObject("all_authors", all, 24, TimeUnit.HOURS);
for (Author a : all) {
    redisUtil.setObject("author:" + a.getId(), a, 24, TimeUnit.HOURS);
}
```

- **替代方案**：jOOQ/QueryDSL；全文检索用 Elasticsearch/OpenSearch；分页使用游标/Keyset。

---

## 5. 学者合作关系分析（两层 GCN + 词袋向量 + 余弦 + JS 散度）

- **关键逻辑**
  - 从论文标题构建作者词袋特征矩阵 X；由作者共同发文构建归一化邻接矩阵 A；两层简化 GCN：H1=ReLU(A·X)，H2=A·H1；
    得到作者嵌入后，基于嵌入余弦 + 研究方向匹配（余弦 + JS 散度）计算综合得分并排序推荐。
- **为何选型**
  - 图结构明显、规模适中，纯 Java 线性代数实现可控、可解释，无需额外训练流水线。
- **关键代码**

```java
// 两层 GCN 嵌入（简化版）
RealMatrix h1 = adjacencyMatrix.multiply(featureMatrix); // ReLU(h1)
for (int i=0;i<h1.getRowDimension();i++) for(int j=0;j<h1.getColumnDimension();j++)
    if (h1.getEntry(i,j) < 0) h1.setEntry(i,j,0);
RealMatrix h2 = adjacencyMatrix.multiply(h1);
// 行向量归一化后写入 authorEmbeddings
```

- **替代方案**：Node2Vec/GraphSAGE/LightGCN；Faiss/向量库近邻；或以 TF-IDF + 共现做启发式打分。

---

## 6. 实时通信与消息管理（WebSocket + STOMP + RabbitMQ + DLQ）

- **关键逻辑**
  - 双模式：Direct（直推）与 Rabbit（入队异步）。
  - 幂等：Redis SETNX 去重 24h；手动 ACK/NACK，失败入 DLQ。
  - 用户专属队列：`chat.user.{userId}.queue` 解决离线消息，上线即消费。
- **为何选型**
  - MQ 降噪削峰、持久化、可恢复；DLQ 便于排障回溯；用户队列提升离线可达性。
- **关键代码**

```java
// ChatService - 发送入口（模式切换）
if ("rabbit".equalsIgnoreCase(messagingMode)) {
    request.setSenderId(senderId);
    chatMessageProducer.publish(request);
    return fastAckResponse();
}
return processAndDispatchInternal(senderId, request);
```

```java
// RabbitMQ 队列 + DLQ（部分）
Map<String,Object> args = new HashMap<>();
args.put("x-dead-letter-exchange", ChatMQConstants.DLX);
args.put("x-dead-letter-routing-key", ChatMQConstants.DLQ_ROUTING_KEY);
return QueueBuilder.durable(ChatMQConstants.CHAT_QUEUE).withArguments(args).build();
```

```java
// 消费者 - 幂等 + 手动 ACK/NACK
String messageId = headers.get("x-message-id");
Boolean first = redisUtil.setIfAbsent("chat:msg:"+messageId, "1", 24, TimeUnit.HOURS);
if (Boolean.FALSE.equals(first)) { channel.basicAck(tag,false); return; }
chatService.processAndDispatch(request);
channel.basicAck(tag,false);
```

- **替代方案**：Kafka（高吞吐）；RocketMQ（事务/延时更强）；DB Outbox + Debezium；Redis Stream。

---

## 7. 在线状态与心跳

- **关键逻辑**：心跳接口验证 JWT 后写 `online:{email}` TTL=15s；查询时判断 key 是否存在；登录/登出时间记录在 `UserOnlineStatus`。

```java
String token = auth.substring(7);
String email = jwtUtil.extractEmail(token);
if (email==null || !jwtUtil.validateToken(token,email)) return 401;
redisUtil.set("online:"+email, String.valueOf(System.currentTimeMillis()), 15, TimeUnit.SECONDS);
```

- **替代方案**：STOMP 心跳帧；在线状态事件化；模式匹配批量清理。

---

## 8. 异步执行（搜索历史专用线程池）

```java
@Bean("searchHistoryTaskExecutor")
public AsyncTaskExecutor executor(){
  ThreadPoolTaskExecutor ex = new ThreadPoolTaskExecutor();
  ex.setCorePoolSize(10); ex.setMaxPoolSize(50); ex.setQueueCapacity(1000);
  ex.setThreadNamePrefix("search-history-");
  ex.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
  ex.setWaitForTasksToCompleteOnShutdown(true); ex.setAwaitTerminationSeconds(30);
  ex.initialize(); return ex;
}
```

---

## 9. 论文智能推荐（当前仓库未包含，给出可选实现方案）

- **适配层**：`WebClient` 异步调用 + 超时/重试（指数退避）+ 熔断；异常降级返回缓存推荐；异步写入 MQ，消费者落库与缓存。
- **一致性与可观测**：请求/重试/超时指标；推荐结果 TTL；幂等键避免重复写。
- **替代方案**：离线批处理 + 向量检索（FAISS/pgvector/ES dense vector）。

---

## 10. 可量化成效（面试表达要点）

- **缓存命中率**：作者与聊天历史等热点，Redis 命中率目标 95%+，显著降低 DB 压力。
- **通信尾延迟**：Rabbit 模式异步化 + 幂等 + ACK，将服务端处理从 ~80ms 降至 ~10ms 级别（结合负载场景描述）。
- **并发一致性**：搜索历史分布式锁 + 延迟双删，避免覆盖/脏读；用户队列保障同一用户有序。
- **可解释预测**：嵌入相似、共同合作者、方向匹配拆解为指标，利于业务沟通与调参。

---

## 11. 常见追问与答法

- **JWT 失效与登出**：黑名单/版本号（tokenVersion）/签发时间比对；本项目利用登出时间辅助消息分割与会话处理。
- **WS 鉴权安全性**：建议 token 放握手头/子协议；后端限频 + 二次验签；避免明文 Query 泄露。
- **MQ 顺序与幂等**：单用户单队列有序；`SETNX` 去重 + 24h TTL；失败 NACK 入 DLQ。
- **GCN 简化假设**：未引入训练权重，工程可解释；可演进为 Node2Vec/LightGCN 等方案。

---

## 附：核心文件索引

- 安全与鉴权：`config/SecurityConfig.java`、`config/JwtAuthenticationFilter.java`、`util/JwtUtil.java`
- WS 配置与鉴权：`config/WebSocketConfig.java`、`config/UserIdHandshakeInterceptor.java`
- Redis 配置与工具：`config/RedisConfirguration.java`、`util/RedisUtil.java`
- 搜索历史：`service/impl/SearchHistoryServiceImpl.java`、`config/AsyncConfig.java`
- 学术检索：`service/impl/PaperServiceImpl.java`、`service/impl/AuthorServiceImpl.java`
- 合作分析：`service/impl/CollaborationServiceImpl.java`
- 即时通信：`service/impl/ChatServiceImpl.java`、`mq/*`

---

## 附加章节：高频面试题（贴合本项目）

1) JWT 无状态鉴权如何实现“登出立刻失效”？
- 场景：用户在 A 端登出，B 端持旧 token 的请求应立即被拒绝。
- 方案A（黑名单）：登出时将 token jti 放入 Redis，值为过期时间；过滤器阶段命中则拒绝。
- 方案B（版本号）：为用户维护 `tokenVersion` 于 Redis；签发时把 version 放在 token 内，鉴权时比对不一致则拒绝。
- 方案C（签发时间对比）：记录 `lastLogoutTime`，若 `token.iat < lastLogoutTime` 拒绝。
- 本项目落地：已落库 `lastLogoutTime`，可在 `JwtAuthenticationFilter` 中补充对比逻辑（与 `UserOnlineStatus` 同表即可）。
- 监控与风控：黑名单命中率、异常拒绝统计、跨端登出时延；高频登出刷新的滥用检测。

2) 延迟双删为什么有效？在哪些场景可能不够？
- 原理：删缓存→写DB→并发读可能回源旧值→延迟再删一次兜底，降低脏读窗口。
- 参数：延迟时间 >= 回源+写入的P95；通过埋点动态调参。
- 不足：极端写热点、批处理回源慢、跨服务写入；此时建议事件总线（binlog/消息）驱动失效、或使用版本号（写穿时带版本校验）。
- 本项目做法：`clearHistory` 即时删 + 500ms 再删，结合分布式锁保证批处理串行。

3) Redis 分布式锁如何保证正确释放？为什么要用 Lua？
- 正确性：获取（SET key val NX EX t）+ 释放（只有持有者才能删）→ 需要“比较 + 删除”原子执行。
- Lua 脚本：在 Redis 侧原子校验 value 一致后 del，避免误删他人锁。
- 可靠性：设置过期避免死锁；执行超时/长 GC 可开启 WatchDog（Redisson）续期；尽量缩短临界区。
- 本项目做法：`RedisUtil.releaseLock` 用脚本实现校验+删除。

4) 聊天消息幂等与顺序如何保证？
- 幂等：
  - 生产端：`clientMsgId`；消费端：消息头 `x-message-id`；Redis `SETNX` + 24h TTL 去重。
  - DB：唯一键 `(sender_id, client_msg_id)` 兜底（`DuplicateKeyException` 返回既有记录）。
- 顺序：
  - 单用户单队列/单消费者；或在同一分区内按 key 路由（Kafka）。
- 失败处理：手动 ACK/NACK；失败 NACK 不重回→进入 DLQ，后续人工/自动补偿。

5) RabbitMQ 与 Kafka 选型对比？为什么这里选 RabbitMQ？
- Kafka：顺序依赖分区键，高吞吐、批量高效、生态偏流处理；精确一次需事务成本。
- RabbitMQ：交换机-路由键灵活，易用的延迟/优先级/死信，适合请求-响应/任务分发。
- 本项目：消息量中等，重视路由/DLQ/可达性与可维护性，RabbitMQ 成本更低、实现更直观。

6) WebSocket 鉴权为什么不建议把 token 放在 query 参数？
- 风险：URL 经常被日志、CDN、代理记录；浏览器历史可见；易泄露。
- 建议：放握手头（Sec-WebSocket-Protocol/Authorization）或 STOMP CONNECT 帧头；服务器端二次校验并限流。
- 若必须用 query：启用 HTTPS、缩短 TTL、最小化权限并在握手后立即服务端侧校验。

7) MyBatis-Plus 动态查询的易错点？
- 条件优先级：`and(w -> w.like(...).or().like(...))`，避免 or 漏出。
- 分页与索引：ORDER BY 需命中索引；避免在大表 `or like` 触发全表扫描。
- N+1：先取主键集合再批量查询映射表；或用业务缓存减少回表。
- 防注入：仅用条件构造器，不拼接原始 SQL 片段。

8) GCN 为何不训练权重也能工作？局限是什么？
- 原理：`A_norm X` 相当于邻域聚合的平滑滤波，结合 ReLU 与行归一化也能形成可用的几何嵌入。
- 局限：
  - 无任务监督，表达难适配具体目标；
  - 图同质性假设较强，跨域迁移差；
  - 计算/内存随图规模增长。
- 演进：LightGCN/Node2Vec；或引入可训练层与度量学习。

9) 搜索历史如何应对多终端高并发写入？
- 队列化：同一用户写入走 Redis List，服务端异步批处理，减少热点行竞争。
- 串行化：分布式锁 + 本地锁兜底，确保批处理临界区独占。
- 合并与限长：批量合并去重（按 id/time），限制最近 N 条，降低缓存写放大。
- 失败补偿：异常进入“用户队列死信”或打点重试；埋点观测队列堆积与处理时延。

10) 如何观测系统的可靠性与性能？
- 指标：Redis 命中/穿透率、锁获取失败率、消息 confirm/return/DLQ 数、WebSocket 在线率、99线延迟。
- 追踪：`clientMsgId` 贯穿前后端、队列与DB，打通链路；异常链路采样率提升。
- 压测：Direct vs Rabbit 模式 A/B；模拟离线→上线消息风暴；锁争用下的处理能力与尾延迟。

11) JWT 与 WebSocket 的用户态如何打通？
- 握手：校验 token，解析 userId 存入会话属性，通过 `HandshakeHandler` 生成 `Principal`。
- 发送：使用 `convertAndSendToUser(userId, "/queue/...", payload)`；无需在 payload 再放身份。
- 状态：同用户多 session 需管理 sessionId 与在线表；断线重连时恢复消费者与未读统计。

12) 延迟双删时间如何确定？
- 经验：取（DB 回源 + 业务处理）P95 ~ P99；
- 实证：埋点测量删除→回源→写回的时间分布；动态配置；
- 兜底：热点键使用事件驱动失效或版本号比对，避免长回源导致的再次污染。


