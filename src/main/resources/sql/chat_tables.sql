-- 聊天消息表
CREATE TABLE IF NOT EXISTS chat_messages (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    sender_id BIGINT NOT NULL,
    receiver_id BIGINT NOT NULL,
    content TEXT NOT NULL,
    message_type VARCHAR(20) DEFAULT 'TEXT' COMMENT '消息类型：TEXT, IMAGE, FILE',
    is_read BOOLEAN DEFAULT FALSE COMMENT '是否已读',
    client_msg_id VARCHAR(64) NULL COMMENT '客户端生成的幂等ID',
    create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    update_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    INDEX idx_sender_receiver (sender_id, receiver_id),
    INDEX idx_create_time (create_time),
    UNIQUE KEY uniq_sender_client_msg_id (sender_id, client_msg_id),
    FOREIGN KEY (sender_id) REFERENCES users(id),
    FOREIGN KEY (receiver_id) REFERENCES users(id)
);

-- 用户在线状态表
CREATE TABLE IF NOT EXISTS user_online_status (
    user_id BIGINT PRIMARY KEY,
    is_online BOOLEAN DEFAULT FALSE,
    last_login_time DATETIME NULL,
    session_id VARCHAR(255),
    last_logout_time DATETIME NULL,
    FOREIGN KEY (user_id) REFERENCES users(id)
);

-- 聊天会话表（可选，用于记录最近聊天）
CREATE TABLE IF NOT EXISTS chat_sessions (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user1_id BIGINT NOT NULL,
    user2_id BIGINT NOT NULL,
    last_message_id BIGINT,
    last_message_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    unread_count1 INT DEFAULT 0 COMMENT '用户1的未读消息数',
    unread_count2 INT DEFAULT 0 COMMENT '用户2的未读消息数',
    create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    update_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    UNIQUE KEY unique_user_pair (user1_id, user2_id),
    FOREIGN KEY (user1_id) REFERENCES users(id),
    FOREIGN KEY (user2_id) REFERENCES users(id),
    FOREIGN KEY (last_message_id) REFERENCES chat_messages(id)
); 