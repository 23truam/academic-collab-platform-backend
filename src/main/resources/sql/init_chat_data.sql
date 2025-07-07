-- 插入测试用户数据（如果不存在）
INSERT IGNORE INTO users (id, username, email, password, role, create_time, update_time, enabled) VALUES
(1, '张三', 'zhangsan@example.com', '$2a$10$N.zmdr9k7uOCQb376NoUnuTJ8iAt6Z5EHsM8lE9lBOsl7iKTVEFDa', 'USER', NOW(), NOW(), true),
(2, '李四', 'lisi@example.com', '$2a$10$N.zmdr9k7uOCQb376NoUnuTJ8iAt6Z5EHsM8lE9lBOsl7iKTVEFDa', 'USER', NOW(), NOW(), true),
(3, '王五', 'wangwu@example.com', '$2a$10$N.zmdr9k7uOCQb376NoUnuTJ8iAt6Z5EHsM8lE9lBOsl7iKTVEFDa', 'USER', NOW(), NOW(), true),
(4, '赵六', 'zhaoliu@example.com', '$2a$10$N.zmdr9k7uOCQb376NoUnuTJ8iAt6Z5EHsM8lE9lBOsl7iKTVEFDa', 'USER', NOW(), NOW(), true);

-- 插入一些测试聊天消息
INSERT INTO chat_messages (sender_id, receiver_id, content, message_type, is_read, create_time, update_time) VALUES
(1, 2, '你好，李四！我想和你讨论一下关于机器学习的研究方向。', 'TEXT', false, DATE_SUB(NOW(), INTERVAL 2 HOUR), DATE_SUB(NOW(), INTERVAL 2 HOUR)),
(2, 1, '你好张三！我也对机器学习很感兴趣，你有什么想法？', 'TEXT', false, DATE_SUB(NOW(), INTERVAL 1 HOUR), DATE_SUB(NOW(), INTERVAL 1 HOUR)),
(1, 2, '我觉得深度学习在自然语言处理方面有很大的潜力。', 'TEXT', false, DATE_SUB(NOW(), INTERVAL 30 MINUTE), DATE_SUB(NOW(), INTERVAL 30 MINUTE)),
(2, 1, '同意！我们可以一起研究这个方向。', 'TEXT', false, DATE_SUB(NOW(), INTERVAL 15 MINUTE), DATE_SUB(NOW(), INTERVAL 15 MINUTE)),
(1, 3, '王五，你最近在研究什么？', 'TEXT', false, DATE_SUB(NOW(), INTERVAL 1 HOUR), DATE_SUB(NOW(), INTERVAL 1 HOUR)),
(3, 1, '我在研究计算机视觉，特别是目标检测算法。', 'TEXT', false, DATE_SUB(NOW(), INTERVAL 45 MINUTE), DATE_SUB(NOW(), INTERVAL 45 MINUTE)),
(1, 4, '赵六，你有兴趣参加我们的学术讨论吗？', 'TEXT', false, DATE_SUB(NOW(), INTERVAL 30 MINUTE), DATE_SUB(NOW(), INTERVAL 30 MINUTE)),
(4, 1, '当然！我很乐意参与讨论。', 'TEXT', false, DATE_SUB(NOW(), INTERVAL 10 MINUTE), DATE_SUB(NOW(), INTERVAL 10 MINUTE));

-- 插入用户在线状态
INSERT IGNORE INTO user_online_status (user_id, is_online, last_online_time, session_id) VALUES
(1, true, NOW(), NULL),
(2, false, DATE_SUB(NOW(), INTERVAL 1 HOUR), NULL),
(3, true, NOW(), NULL),
(4, false, DATE_SUB(NOW(), INTERVAL 30 MINUTE), NULL); 