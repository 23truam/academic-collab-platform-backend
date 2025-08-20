-- 🆕 第二阶段+第三阶段：数据库优化脚本
-- 为聊天系统添加性能优化索引

-- 1. 为离线消息查询优化的复合索引
-- 这个索引支持按接收者、时间、已读状态的组合查询
CREATE INDEX IF NOT EXISTS idx_chat_messages_offline 
ON chat_messages (receiver_id, create_time, is_read);

-- 2. 为发送者查询优化的索引
CREATE INDEX IF NOT EXISTS idx_chat_messages_sender_time 
ON chat_messages (sender_id, create_time);

-- 3. 为消息ID批量查询优化的索引（主键已有，此索引为补充）
CREATE INDEX IF NOT EXISTS idx_chat_messages_batch_query 
ON chat_messages (id, receiver_id, is_read);

-- 4. 为未读消息统计优化的索引
CREATE INDEX IF NOT EXISTS idx_chat_messages_unread_count 
ON chat_messages (receiver_id, is_read, sender_id);

-- 5. 为聊天历史查询优化的复合索引
CREATE INDEX IF NOT EXISTS idx_chat_messages_history 
ON chat_messages (sender_id, receiver_id, create_time);

-- 6. 🆕 第三阶段：为时间范围查询优化的索引
CREATE INDEX IF NOT EXISTS idx_chat_messages_time_range 
ON chat_messages (receiver_id, create_time, message_type);

-- 7. 为用户在线状态查询优化
CREATE INDEX IF NOT EXISTS idx_user_online_status_lookup 
ON user_online_status (user_id, is_online, last_logout_time);

-- 8. 查看索引使用情况的分析查询
-- 执行后可以查看索引的实际效果

-- 查看离线消息查询的执行计划
EXPLAIN SELECT id, sender_id, receiver_id, content, message_type, is_read, client_msg_id, create_time, update_time
FROM chat_messages 
WHERE receiver_id = 1
  AND create_time > '2025-01-01 00:00:00'
  AND is_read = false
ORDER BY create_time ASC
LIMIT 100;

-- 查看聊天历史查询的执行计划
EXPLAIN SELECT * FROM chat_messages 
WHERE (sender_id = 1 AND receiver_id = 2) 
   OR (sender_id = 2 AND receiver_id = 1)
ORDER BY create_time DESC 
LIMIT 50;

-- 查看未读消息统计的执行计划
EXPLAIN SELECT COUNT(*) FROM chat_messages 
WHERE receiver_id = 1 AND is_read = FALSE;

-- 查看批量消息查询的执行计划
EXPLAIN SELECT id, sender_id, receiver_id, content, message_type, is_read, client_msg_id, create_time, update_time
FROM chat_messages 
WHERE id IN (1, 2, 3, 4, 5)
ORDER BY create_time ASC;

-- 🆕 第三阶段：性能监控查询
-- 这些查询可以帮助监控系统性能

-- 查看表的行数和大小
SELECT 
    TABLE_NAME,
    TABLE_ROWS as '行数',
    ROUND(((DATA_LENGTH + INDEX_LENGTH) / 1024 / 1024), 2) as '大小MB'
FROM information_schema.TABLES 
WHERE TABLE_SCHEMA = DATABASE() 
  AND TABLE_NAME IN ('chat_messages', 'user_online_status')
ORDER BY TABLE_ROWS DESC;

-- 查看索引使用情况
SHOW INDEX FROM chat_messages;
SHOW INDEX FROM user_online_status;

-- 🆕 第三阶段：慢查询优化建议
-- 如果发现慢查询，可以考虑以下优化：

-- 1. 定期清理过期数据（可选）
-- DELETE FROM chat_messages 
-- WHERE create_time < DATE_SUB(NOW(), INTERVAL 90 DAY);

-- 2. 分析表以更新统计信息
ANALYZE TABLE chat_messages;
ANALYZE TABLE user_online_status;

-- 3. 优化表结构（如果需要）
-- OPTIMIZE TABLE chat_messages;
-- OPTIMIZE TABLE user_online_status;

-- 输出优化完成信息
SELECT 
    'Database optimization completed!' as message,
    NOW() as timestamp,
    'Indexes created for offline messages, batch processing, and performance monitoring' as details;
