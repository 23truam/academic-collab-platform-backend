-- 插入测试用户数据（如果不存在）
INSERT IGNORE INTO users (id, username, email, password, role, create_time, update_time, enabled) VALUES
(1, '张三', 'zhangsan@example.com', '$2a$10$N.zmdr9k7uOCQb376NoUnuTJ8iAt6Z5EHsM8lE9lBOsl7iKTVEFDa', 'USER', NOW(), NOW(), true),
(2, '李四', 'lisi@example.com', '$2a$10$N.zmdr9k7uOCQb376NoUnuTJ8iAt6Z5EHsM8lE9lBOsl7iKTVEFDa', 'USER', NOW(), NOW(), true),
(3, '王五', 'wangwu@example.com', '$2a$10$N.zmdr9k7uOCQb376NoUnuTJ8iAt6Z5EHsM8lE9lBOsl7iKTVEFDa', 'USER', NOW(), NOW(), true),
(4, '赵六', 'zhaoliu@example.com', '$2a$10$N.zmdr9k7uOCQb376NoUnuTJ8iAt6Z5EHsM8lE9lBOsl7iKTVEFDa', 'USER', NOW(), NOW(), true);

-- 不再初始化聊天记录

-- 插入用户在线状态
INSERT IGNORE INTO user_online_status (user_id, is_online, last_login_time, session_id) VALUES
(1, true, NOW(), NULL),
(2, false, DATE_SUB(NOW(), INTERVAL 1 HOUR), NULL),
(3, true, NOW(), NULL),
(4, false, DATE_SUB(NOW(), INTERVAL 30 MINUTE), NULL);


