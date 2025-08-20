-- 修复用户在线状态表中的null值问题
-- 执行前请先备份数据库

-- 1. 查看有多少null值
SELECT 
    COUNT(*) as total_records,
    COUNT(CASE WHEN is_online IS NULL THEN 1 END) as null_count,
    COUNT(CASE WHEN is_online = true THEN 1 END) as online_count,
    COUNT(CASE WHEN is_online = false THEN 1 END) as offline_count
FROM user_online_status;

-- 2. 查看具体的null值记录
SELECT user_id, is_online, last_login_time, session_id 
FROM user_online_status 
WHERE is_online IS NULL;

-- 3. 将null值设置为false（离线）
UPDATE user_online_status 
SET is_online = false 
WHERE is_online IS NULL;

-- 4. 验证修复结果
SELECT 
    COUNT(*) as total_records,
    COUNT(CASE WHEN is_online IS NULL THEN 1 END) as null_count,
    COUNT(CASE WHEN is_online = true THEN 1 END) as online_count,
    COUNT(CASE WHEN is_online = false THEN 1 END) as offline_count
FROM user_online_status;

-- 5. 查看所有用户状态（可选）
SELECT user_id, is_online, last_login_time, session_id 
FROM user_online_status 
ORDER BY user_id;
