-- 创建数据库
CREATE DATABASE IF NOT EXISTS academic_collab DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

USE academic_collab;

-- 文献表
CREATE TABLE IF NOT EXISTS papers (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    title VARCHAR(512) NOT NULL,
    abstract_text TEXT,
    url VARCHAR(1024),
    year INT
);

-- 作者表
CREATE TABLE IF NOT EXISTS authors (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(128) NOT NULL UNIQUE
);

-- 文献-作者关系表
CREATE TABLE IF NOT EXISTS paper_authors (
    paper_id BIGINT NOT NULL,
    author_id BIGINT NOT NULL,
    PRIMARY KEY (paper_id, author_id),
    FOREIGN KEY (paper_id) REFERENCES papers(id) ON DELETE CASCADE,
    FOREIGN KEY (author_id) REFERENCES authors(id) ON DELETE CASCADE
);

-- 用户表
CREATE TABLE IF NOT EXISTS users (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    username VARCHAR(64) NOT NULL UNIQUE,
    email VARCHAR(128) NOT NULL UNIQUE,
    password VARCHAR(255) NOT NULL,
    role VARCHAR(32) DEFAULT 'USER',
    create_time DATETIME DEFAULT CURRENT_TIMESTAMP,
    update_time DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    enabled BOOLEAN DEFAULT TRUE
);

-- 搜索历史表
CREATE TABLE IF NOT EXISTS search_history (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id VARCHAR(128) NOT NULL,
    keyword VARCHAR(256) NOT NULL,
    type VARCHAR(32) NOT NULL,
    timestamp BIGINT NOT NULL
);

-- 创建索引
CREATE INDEX idx_papers_year ON papers(year);
CREATE INDEX idx_papers_title ON papers(title);
CREATE INDEX idx_authors_name ON authors(name);
CREATE INDEX idx_users_username ON users(username);
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_search_history_user_id ON search_history(user_id);
CREATE INDEX idx_search_history_timestamp ON search_history(timestamp);

-- 修改user_online_status表的last_online_time字段为last_login_time
ALTER TABLE user_online_status CHANGE last_online_time last_login_time DATETIME NULL COMMENT '最近一次登录时间'; 