package com.example.academic_collab_platform_backend.controller;

import com.example.academic_collab_platform_backend.service.HeartbeatService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;

@RestController
@RequestMapping("/api/heartbeat")
@CrossOrigin(origins = "*")
public class HeartbeatController {

    @Autowired
    private HeartbeatService heartbeatService;

    /**
     * 心跳接口 - 用户定时发送心跳包保持在线状态
     */
    @PostMapping
    public ResponseEntity<?> heartbeat(HttpServletRequest request) {
        return heartbeatService.handleHeartbeat(request);
    }

    /**
     * 检查用户在线状态
     */
    @GetMapping("/status/{email}")
    public ResponseEntity<?> checkOnlineStatus(@PathVariable String email) {
        return heartbeatService.checkOnlineStatus(email);
    }

    /**
     * 获取所有在线用户列表
     */
    @GetMapping("/online-users")
    public ResponseEntity<?> getOnlineUsers() {
        return heartbeatService.getOnlineUsers();
    }
} 