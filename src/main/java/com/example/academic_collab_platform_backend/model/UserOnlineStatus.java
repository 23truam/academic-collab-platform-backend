package com.example.academic_collab_platform_backend.model;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@TableName("user_online_status")
public class UserOnlineStatus {
    
    @TableId(type = IdType.AUTO)
    private Long userId;
    
    private Boolean isOnline;
    
    private LocalDateTime lastLoginTime;
    
    private String sessionId;

    private LocalDateTime lastLogoutTime;
} 