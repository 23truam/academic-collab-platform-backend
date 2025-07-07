# 学术合作平台后端

## 项目简介
这是一个基于Spring Boot + MyBatis-Plus的学术合作平台后端项目，提供文献搜索、作者查找、合作分析等功能。

## 技术栈
- Spring Boot 3.5.3
- MyBatis-Plus 3.5.3.1
- MySQL 8.0
- Maven

## 项目结构
```
src/main/java/com/example/academic_collab_platform_backend/
├── config/                 # 配置类
│   └── MybatisPlusConfig.java
├── controller/             # 控制器层
│   ├── PaperController.java
│   ├── AuthorController.java
│   ├── SearchHistoryController.java
│   └── CollaborationController.java
├── model/                  # 实体类
│   ├── Paper.java
│   ├── Author.java
│   ├── PaperAuthor.java
│   ├── SearchHistory.java
│   └── CollaborationResult.java
├── mapper/                 # 数据访问层
│   ├── PaperMapper.java
│   ├── AuthorMapper.java
│   ├── PaperAuthorMapper.java
│   └── SearchHistoryMapper.java
├── servcie/                # 服务层
│   ├── PaperService.java
│   ├── AuthorService.java
│   ├── SearchHistoryService.java
│   ├── CollaborationService.java
│   └── impl/               # 服务实现类
│       ├── PaperServiceImpl.java
│       ├── AuthorServiceImpl.java
│       ├── SearchHistoryServiceImpl.java
│       └── CollaborationServiceImpl.java
├── util/                   # 工具类
│   └── DataImporter.java
└── AcademicCollabPlatformBackendApplication.java
```

## 数据库设计
- **papers**: 文献表
- **authors**: 作者表
- **paper_authors**: 文献-作者关系表
- **search_history**: 搜索历史表

## 安装和运行

### 1. 环境要求
- JDK 17+
- MySQL 8.0+
- Maven 3.6+

### 2. 数据库配置
1. 创建数据库：
```sql
CREATE DATABASE academic_collab DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
```

2. 执行建表SQL：
```sql
-- 在 src/main/resources/sql/init.sql 中
```

### 3. 配置文件
修改 `src/main/resources/application.yml` 中的数据库连接信息：
```yaml
spring:
  datasource:
    url: jdbc:mysql://localhost:3306/academic_collab?allowPublicKeyRetrieval=true&useSSL=false&serverTimezone=UTC&characterEncoding=utf8
    username: your_username
    password: your_password
```

### 4. 运行项目
```bash
mvn spring-boot:run
```

项目启动后会自动导入 `dblp.json` 中的数据到MySQL数据库。

## API接口

### 文献相关
- `GET /api/papers/search?query=关键词&page=1&size=10` - 搜索文献
- `GET /api/papers/by-author/{authorId}` - 获取指定作者的文献

### 作者相关
- `GET /api/authors/search?keyword=关键词&page=1&size=10` - 搜索作者
- `GET /api/authors/by-paper/{paperId}` - 获取指定文献的作者

### 搜索历史
- `GET /api/search-history?userId=用户ID&page=1&size=10` - 获取搜索历史
- `POST /api/search-history` - 添加搜索历史
- `DELETE /api/search-history?userId=用户ID` - 清除搜索历史

### 合作分析
- `GET /api/collaboration/analyze?authorId=作者ID&startYear=2020&endYear=2023` - 分析合作关系
- `POST /api/collaboration/predict` - 预测潜在合作者

## 注意事项
1. 首次启动时会自动导入dblp.json数据，请确保文件存在
2. 所有接口都支持跨域访问
3. 分页参数从1开始计数 