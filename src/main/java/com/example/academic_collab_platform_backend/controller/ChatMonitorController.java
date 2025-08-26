package com.example.academic_collab_platform_backend.controller;

import com.example.academic_collab_platform_backend.service.ChatService;
import com.example.academic_collab_platform_backend.util.UserContextUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

/**
 * 🆕 第三阶段：聊天系统监控接口
 * 
 * 提供离线消息处理统计、性能监控等功能
 */
@RestController
@RequestMapping("/api/chat/monitor")
@CrossOrigin(origins = "*")
public class ChatMonitorController {
    
    @Autowired
    private ChatService chatService;
    
    @Autowired
    private UserContextUtil userContextUtil;
    

    
    /**
     * 获取系统健康状态
     */
    @GetMapping("/health")
    public ResponseEntity<?> getHealthStatus() {
        Map<String, Object> health = new HashMap<>();
        health.put("status", "healthy");
        health.put("timestamp", System.currentTimeMillis());
        Map<String, Object> features = new HashMap<>();
        features.put("pushPullMode", true);
        features.put("offlineMessages", false);
        features.put("batchProcessing", true);
        features.put("monitoring", true);
        health.put("features", features);
        
        return ResponseEntity.ok(health);
    }
    

}
