# 作者缓存功能说明

## 概述

为了提高文献搜索的性能，我们实现了作者信息的Redis缓存功能。第一次查询时会将所有作者信息加载到Redis缓存中，后续查询将直接从缓存获取，避免重复的MySQL查询。

## 功能特性

1. **自动缓存加载**: 应用启动时自动加载所有作者信息到Redis
2. **智能缓存策略**: 优先从缓存获取，缓存未命中时从数据库加载并缓存
3. **缓存过期管理**: 缓存默认24小时过期，可配置
4. **缓存清理功能**: 支持手动清理缓存
5. **性能监控**: 提供缓存状态检查接口

## 缓存键设计

- `all_authors`: 存储所有作者列表
- `author:{id}`: 存储单个作者信息（按ID）
- `author_name:{name}`: 存储单个作者信息（按姓名）

## API接口

### 1. 初始化缓存
```http
POST /api/authors/init-cache
```

**响应示例:**
```json
{
    "success": true,
    "message": "作者缓存初始化成功"
}
```

### 2. 检查缓存状态
```http
GET /api/authors/cache-status
```

**响应示例:**
```json
{
    "success": true,
    "cacheWorking": true,
    "responseTime": "15ms",
    "message": "缓存状态正常"
}
```

### 3. 作者搜索（已优化为使用缓存）
```http
GET /api/authors/search?keyword=关键词&page=1&size=10
```

### 4. 获取作者详情（已优化为使用缓存）
```http
GET /api/authors/detail?name=作者姓名
```

## 配置说明

### 缓存过期时间
在 `AuthorServiceImpl.java` 中可以修改缓存过期时间：
```java
private static final long CACHE_EXPIRE_TIME = 24; // 缓存过期时间（小时）
```

### Redis配置
确保 `application.properties` 中配置了正确的Redis连接信息：
```properties
spring.redis.host=localhost
spring.redis.port=6379
spring.redis.database=0
```

## 使用流程

1. **应用启动**: 自动加载所有作者到Redis缓存
2. **首次访问**: 如果缓存为空，自动从数据库加载并缓存
3. **后续访问**: 直接从Redis缓存获取，提高响应速度
4. **缓存更新**: 可通过API手动重新加载缓存

## 性能优化

- **减少数据库查询**: 避免重复的MySQL查询
- **提高响应速度**: Redis内存访问比数据库查询快
- **降低数据库压力**: 减少数据库连接和查询负载
- **支持高并发**: Redis支持高并发访问

## 注意事项

1. 确保Redis服务正常运行
2. 缓存数据可能与数据库存在短暂不一致（24小时内）
3. 如需立即更新数据，可调用缓存初始化接口
4. 缓存占用内存，请根据实际数据量调整过期时间

## 故障排除

### 缓存未生效
1. 检查Redis服务是否正常运行
2. 检查Redis连接配置是否正确
3. 调用 `/api/authors/cache-status` 检查缓存状态

### 数据不一致
1. 调用 `/api/authors/init-cache` 重新加载缓存
2. 检查数据库连接是否正常
3. 查看应用日志中的错误信息

### 性能问题
1. 检查Redis内存使用情况
2. 调整缓存过期时间
3. 监控缓存命中率 