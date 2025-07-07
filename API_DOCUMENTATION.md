# 学术合作平台后端API文档

## 基础信息
- 基础URL: `http://localhost:8080/api`
- 认证方式: JWT Token (Bearer Token)
- 响应格式: JSON

## 认证相关API

### 1. 用户注册
- **URL**: `POST /auth/register`
- **请求体**:
```json
{
    "username": "testuser",
    "email": "test@example.com",
    "password": "123456"
}
```
- **响应**:
```json
{
    "token": "eyJhbGciOiJIUzI1NiJ9...",
    "username": "testuser",
    "userId": 1,
    "message": "注册成功"
}
```

### 2. 用户登录
- **URL**: `POST /auth/login`
- **请求体**:
```json
{
    "username": "testuser",
    "password": "123456"
}
```
- **响应**:
```json
{
    "token": "eyJhbGciOiJIUzI1NiJ9...",
    "username": "testuser",
    "userId": 1,
    "message": "登录成功"
}
```

### 3. Token验证
- **URL**: `GET /auth/validate`
- **请求头**: `Authorization: Bearer <token>`
- **响应**:
```json
{
    "token": "eyJhbGciOiJIUzI1NiJ9...",
    "username": "testuser",
    "userId": 1,
    "message": "Token有效"
}
```

## 文献相关API

### 1. 搜索文献
- **URL**: `GET /papers/search`
- **参数**:
  - `query`: 搜索关键词 (可选)
  - `page`: 页码 (默认1)
  - `size`: 每页大小 (默认10)
- **响应**: 分页的文献列表

### 2. 根据ID获取文献
- **URL**: `GET /papers/{id}`
- **响应**: 单个文献详情

### 3. 根据作者获取文献
- **URL**: `GET /papers/by-author/{authorId}`
- **响应**: 该作者的所有文献

### 4. 根据年份获取文献
- **URL**: `GET /papers/by-year`
- **参数**:
  - `year`: 年份
  - `keyword`: 关键词 (可选)
- **响应**: 指定年份的文献列表

### 5. 根据会议/期刊获取文献
- **URL**: `GET /papers/by-venue`
- **参数**:
  - `venue`: 会议/期刊名称
  - `page`: 页码 (默认1)
  - `size`: 每页大小 (默认10)
- **响应**: 分页的文献列表

### 6. 文献统计
- **URL**: `GET /papers/statistics`
- **响应**: 文献相关统计数据

## 作者相关API

### 1. 搜索作者
- **URL**: `GET /authors/search`
- **参数**:
  - `keyword`: 搜索关键词 (可选)
  - `page`: 页码 (默认1)
  - `size`: 每页大小 (默认10)
- **响应**: 分页的作者列表

### 2. 根据ID获取作者
- **URL**: `GET /authors/{id}`
- **响应**: 单个作者详情

### 3. 根据文献获取作者
- **URL**: `GET /authors/by-paper/{paperId}`
- **响应**: 该文献的所有作者

### 4. 获取顶级作者
- **URL**: `GET /authors/top-authors`
- **参数**:
  - `limit`: 返回数量 (默认10)
  - `year`: 年份 (可选)
- **响应**: 顶级作者列表

### 5. 根据机构获取作者
- **URL**: `GET /authors/by-institution`
- **参数**:
  - `institution`: 机构名称
  - `page`: 页码 (默认1)
  - `size`: 每页大小 (默认10)
- **响应**: 分页的作者列表

### 6. 作者统计
- **URL**: `GET /authors/statistics`
- **响应**: 作者相关统计数据

## 合作分析API

### 1. 分析合作关系
- **URL**: `GET /collaboration/analyze`
- **参数**:
  - `authorId`: 作者ID
  - `startYear`: 开始年份 (可选)
  - `endYear`: 结束年份 (可选)
- **响应**: 合作分析结果

### 2. 预测合作者
- **URL**: `POST /collaboration/predict`
- **请求体**:
```json
{
    "authorId": 1,
    "directions": ["机器学习", "深度学习"],
    "minPapers": 5,
    "startYear": 2020,
    "endYear": 2024
}
```
- **响应**: 预测的合作者列表

## 搜索历史API

### 1. 获取搜索历史
- **URL**: `GET /search-history`
- **参数**:
  - `userId`: 用户ID
  - `page`: 页码 (默认1)
  - `size`: 每页大小 (默认10)
- **响应**: 分页的搜索历史

### 2. 添加搜索历史
- **URL**: `POST /search-history`
- **请求体**:
```json
{
    "userId": "user123",
    "keyword": "机器学习",
    "type": "paper"
}
```

### 3. 清除搜索历史
- **URL**: `DELETE /search-history`
- **参数**:
  - `userId`: 用户ID
- **响应**: 成功清除

## 错误响应格式
```json
{
    "error": "错误信息",
    "status": "error"
}
```

## 使用示例

### 1. 注册新用户
```bash
curl -X POST http://localhost:8080/api/auth/register \
  -H "Content-Type: application/json" \
  -d '{
    "username": "testuser",
    "email": "test@example.com",
    "password": "123456"
  }'
```

### 2. 登录获取Token
```bash
curl -X POST http://localhost:8080/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{
    "username": "testuser",
    "password": "123456"
  }'
```

### 3. 使用Token访问API
```bash
curl -X GET http://localhost:8080/api/papers/search?query=machine%20learning \
  -H "Authorization: Bearer <your-token>"
``` 