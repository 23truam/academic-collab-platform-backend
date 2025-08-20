package com.example.academic_collab_platform_backend.model;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@TableName("user_online_status")
public class UserOnlineStatus {
    
    // user_id 与 users(id) 一一对应，非自增主键
    @TableId(type = IdType.INPUT)
    private Long userId;
    
    private Boolean isOnline;
    
    private LocalDateTime lastLoginTime;
    
    private String sessionId;

    private LocalDateTime lastLogoutTime;
    
    // 🔧 手动添加 getter/setter 方法（防止Lombok问题）
    public Long getUserId() { return userId; }
    public void setUserId(Long userId) { this.userId = userId; }
    
    public Boolean getIsOnline() { return isOnline; }
    public void setIsOnline(Boolean isOnline) { this.isOnline = isOnline; }
    
    public LocalDateTime getLastLoginTime() { return lastLoginTime; }
    public void setLastLoginTime(LocalDateTime lastLoginTime) { this.lastLoginTime = lastLoginTime; }
    
    public String getSessionId() { return sessionId; }
    public void setSessionId(String sessionId) { this.sessionId = sessionId; }
    
    public LocalDateTime getLastLogoutTime() { return lastLogoutTime; }
    public void setLastLogoutTime(LocalDateTime lastLogoutTime) { this.lastLogoutTime = lastLogoutTime; }
} 