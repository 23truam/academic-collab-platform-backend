package com.example.academic_collab_platform_backend.controller;

import com.example.academic_collab_platform_backend.mapper.UserOnlineStatusMapper;
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

    @PostMapping("/online")
    public ResponseEntity<?> setOnline(@RequestBody Map<String, Boolean> body) {
        Long userId = userContextUtil.getCurrentUserId();
        Boolean online = body.get("isOnline");
        userOnlineStatusMapper.updateOnlineStatus(userId, online);
        return ResponseEntity.ok().build();
    }

    @GetMapping("/all-with-status")
    public ResponseEntity<?> getAllUsersWithStatus() {
        return ResponseEntity.ok(userMapper.selectAllUsersWithStatus());
    }
} 