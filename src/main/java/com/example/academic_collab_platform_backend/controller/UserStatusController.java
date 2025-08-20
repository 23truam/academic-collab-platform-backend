package com.example.academic_collab_platform_backend.controller;

import com.example.academic_collab_platform_backend.mapper.UserOnlineStatusMapper;
import com.example.academic_collab_platform_backend.service.ChatService;
import com.example.academic_collab_platform_backend.util.UserContextUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/user")
@CrossOrigin(origins = "*")
public class UserStatusController {
    @Autowired
    private UserOnlineStatusMapper userOnlineStatusMapper;
    @Autowired
    private UserContextUtil userContextUtil;
    @Autowired
    private com.example.academic_collab_platform_backend.mapper.UserMapper userMapper;

    @Autowired
    private ChatService chatService;

    @PostMapping("/online")
    public ResponseEntity<?> setOnline(@RequestBody Map<String, Boolean> body) {
        Long userId = userContextUtil.getCurrentUserId();
        Boolean online = body.get("isOnline");
        
        // 🆕 添加null检查和默认值
        if (online == null) {
            System.out.println("⚠️ [UserStatus] 前端传递的isOnline为null，默认设置为false");
            online = false;  // 默认为离线
        }
        
        System.out.println("🔄 [UserStatus] 更新用户状态: userId=" + userId + ", isOnline=" + online);
        chatService.updateUserOnlineStatus(userId, online, null);
        return ResponseEntity.ok().build();
    }

    @GetMapping("/all-with-status")
    public ResponseEntity<?> getAllUsersWithStatus() {
        return ResponseEntity.ok(userMapper.selectAllUsersWithStatus());
    }
} 