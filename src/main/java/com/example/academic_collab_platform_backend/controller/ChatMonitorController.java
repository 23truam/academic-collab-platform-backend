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
     * 获取当前用户的离线消息处理统计
     */
    @GetMapping("/offline-stats")
    public ResponseEntity<?> getOfflineStats() {
        try {
            Long userId = userContextUtil.getCurrentUserId();
            Map<String, Object> stats = chatService.getOfflineMessageStats(userId);
            
            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("data", stats);
            response.put("userId", userId);
            
            return ResponseEntity.ok(response);
            
        } catch (Exception e) {
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "获取离线消息统计失败: " + e.getMessage());
            return ResponseEntity.status(500).body(error);
        }
    }
    
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
        features.put("offlineMessages", true);
        features.put("batchProcessing", true);
        features.put("monitoring", true);
        health.put("features", features);
        
        return ResponseEntity.ok(health);
    }
    
    /**
     * 🆕 第三阶段：批量标记消息为已读（优化接口）
     */
    @PostMapping("/batch-mark-read")
    public ResponseEntity<?> batchMarkAsRead(@RequestBody Map<String, Object> request) {
        try {
            Long userId = userContextUtil.getCurrentUserId();
            @SuppressWarnings("unchecked")
            java.util.List<Long> messageIds = (java.util.List<Long>) request.get("messageIds");
            
            if (messageIds == null || messageIds.isEmpty()) {
                Map<String, Object> error = new HashMap<>();
                error.put("success", false);
                error.put("message", "消息ID列表不能为空");
                return ResponseEntity.badRequest().body(error);
            }
            
            chatService.batchMarkMessagesAsRead(userId, messageIds);
            
            Map<String, Object> success = new HashMap<>();
            success.put("success", true);
            success.put("message", "批量标记成功");
            success.put("count", messageIds.size());
            return ResponseEntity.ok(success);
            
        } catch (Exception e) {
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "批量标记失败: " + e.getMessage());
            return ResponseEntity.status(500).body(error);
        }
    }
}
