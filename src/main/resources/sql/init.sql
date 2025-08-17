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
-- 为避免重复执行导致的错误，以下索引创建均做存在性判断

-- papers(year)
SET @exists_idx_papers_year := (
    SELECT COUNT(1) FROM information_schema.statistics
    WHERE table_schema = DATABASE() AND table_name = 'papers' AND index_name = 'idx_papers_year'
);
SET @sql_idx_papers_year := IF(@exists_idx_papers_year = 0,
    'CREATE INDEX idx_papers_year ON papers(year)', 'SELECT 1');
PREPARE stmt FROM @sql_idx_papers_year; EXECUTE stmt; DEALLOCATE PREPARE stmt;

-- papers(title)
SET @exists_idx_papers_title := (
    SELECT COUNT(1) FROM information_schema.statistics
    WHERE table_schema = DATABASE() AND table_name = 'papers' AND index_name = 'idx_papers_title'
);
SET @sql_idx_papers_title := IF(@exists_idx_papers_title = 0,
    'CREATE INDEX idx_papers_title ON papers(title)', 'SELECT 1');
PREPARE stmt FROM @sql_idx_papers_title; EXECUTE stmt; DEALLOCATE PREPARE stmt;

-- authors(name)
SET @exists_idx_authors_name := (
    SELECT COUNT(1) FROM information_schema.statistics
    WHERE table_schema = DATABASE() AND table_name = 'authors' AND index_name = 'idx_authors_name'
);
SET @sql_idx_authors_name := IF(@exists_idx_authors_name = 0,
    'CREATE INDEX idx_authors_name ON authors(name)', 'SELECT 1');
PREPARE stmt FROM @sql_idx_authors_name; EXECUTE stmt; DEALLOCATE PREPARE stmt;

-- users(username)
SET @exists_idx_users_username := (
    SELECT COUNT(1) FROM information_schema.statistics
    WHERE table_schema = DATABASE() AND table_name = 'users' AND index_name = 'idx_users_username'
);
SET @sql_idx_users_username := IF(@exists_idx_users_username = 0,
    'CREATE INDEX idx_users_username ON users(username)', 'SELECT 1');
PREPARE stmt FROM @sql_idx_users_username; EXECUTE stmt; DEALLOCATE PREPARE stmt;

-- users(email)
SET @exists_idx_users_email := (
    SELECT COUNT(1) FROM information_schema.statistics
    WHERE table_schema = DATABASE() AND table_name = 'users' AND index_name = 'idx_users_email'
);
SET @sql_idx_users_email := IF(@exists_idx_users_email = 0,
    'CREATE INDEX idx_users_email ON users(email)', 'SELECT 1');
PREPARE stmt FROM @sql_idx_users_email; EXECUTE stmt; DEALLOCATE PREPARE stmt;

-- search_history(user_id)
SET @exists_idx_sh_user_id := (
    SELECT COUNT(1) FROM information_schema.statistics
    WHERE table_schema = DATABASE() AND table_name = 'search_history' AND index_name = 'idx_search_history_user_id'
);
SET @sql_idx_sh_user_id := IF(@exists_idx_sh_user_id = 0,
    'CREATE INDEX idx_search_history_user_id ON search_history(user_id)', 'SELECT 1');
PREPARE stmt FROM @sql_idx_sh_user_id; EXECUTE stmt; DEALLOCATE PREPARE stmt;

-- search_history(timestamp)
SET @exists_idx_sh_ts := (
    SELECT COUNT(1) FROM information_schema.statistics
    WHERE table_schema = DATABASE() AND table_name = 'search_history' AND index_name = 'idx_search_history_timestamp'
);
SET @sql_idx_sh_ts := IF(@exists_idx_sh_ts = 0,
    'CREATE INDEX idx_search_history_timestamp ON search_history(timestamp)', 'SELECT 1');
PREPARE stmt FROM @sql_idx_sh_ts; EXECUTE stmt; DEALLOCATE PREPARE stmt;