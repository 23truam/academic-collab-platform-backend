package com.example.academic_collab_platform_backend.controller;

import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import com.example.academic_collab_platform_backend.dto.ChatMessageResponse;
import com.example.academic_collab_platform_backend.dto.UserListDTO;
import com.example.academic_collab_platform_backend.service.ChatService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import javax.servlet.http.HttpServletRequest;
import com.example.academic_collab_platform_backend.util.JwtUtil;
import com.example.academic_collab_platform_backend.util.ResponseUtil;

import java.util.List;
import java.util.Map;
import com.example.academic_collab_platform_backend.service.ChatWebSocketService;

@RestController
@RequestMapping("/api/chat")
@CrossOrigin(origins = "*", exposedHeaders = {"X-Cache"})
public class ChatController {

    @Autowired
    private ChatService chatService;
    @Autowired
    private JwtUtil jwtUtil;
    @Autowired
    private ChatWebSocketService chatWebSocketService;

    private Long getCurrentUserId(HttpServletRequest request) {
        String token = request.getHeader("Authorization");
        if (token != null && token.startsWith("Bearer ")) {
            token = token.substring(7);
        }
        return Long.valueOf(jwtUtil.extractUserId(token));
    }



    /**
     * 获取聊天历史
     */
    @GetMapping("/history/{userId}")
    public ResponseEntity<?> getChatHistory(
        @PathVariable Long userId,
        @RequestParam(defaultValue = "200") Integer limit,
        HttpServletRequest httpRequest
    ) {
        try {
            Long currentUserId = getCurrentUserId(httpRequest);
            List<ChatMessageResponse> messages = chatService.getChatHistory(currentUserId, userId, limit);
            return ResponseEntity.ok(ResponseUtil.success(messages));
        } catch(Exception e) {
            return ResponseEntity.badRequest().body(ResponseUtil.error(e.getMessage()));
        }
    }

    /**
     * 获取聊天历史（带缓存）
     */
    @GetMapping("/history-with-cache/{userId}")
    public ResponseEntity<?> getChatHistoryWithCache(
        @PathVariable Long userId,
        @RequestParam(defaultValue = "200") Integer limit,
        @RequestParam(required = false) Long loginTime,
        HttpServletRequest httpRequest
    ) {
        try {
            Long currentUserId = getCurrentUserId(httpRequest);
            // 如果前端未传 loginTime，则默认使用 token 的签发时间作为登录时刻
            if (loginTime == null) {
                String token = httpRequest.getHeader("Authorization");
                if (token != null && token.startsWith("Bearer ")) {
                    token = token.substring(7);
                }
                Long issuedAt = jwtUtil.extractIssuedAtEpochMillis(token);
                if (issuedAt != null) {
                    loginTime = issuedAt;
                }
            }
            Map<String, Object> result = chatService.getChatHistoryWithCache(currentUserId, userId, limit, loginTime);
            boolean cacheHit = Boolean.TRUE.equals(result.get("cacheHit"));
            return ResponseEntity.ok()
                    .header("X-Cache", cacheHit ? "HIT" : "MISS")
                    .body(ResponseUtil.success(result));
        } catch(Exception e) {
            return ResponseEntity.badRequest().body(ResponseUtil.error(e.getMessage()));
        }
    }

    /**
     * 设置当前活跃会话对端
     */
    @PostMapping("/active-session/{peerUserId}")
    public ResponseEntity<?> setActiveSession(@PathVariable Long peerUserId, HttpServletRequest httpRequest) {
        try {
            Long currentUserId = getCurrentUserId(httpRequest);
            chatWebSocketService.setActivePeer(currentUserId, peerUserId);
            return ResponseEntity.ok(ResponseUtil.successMsg("active session set"));
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(ResponseUtil.error(e.getMessage()));
        }
    }

    /**
     * 清除当前活跃会话
     */
    @DeleteMapping("/active-session")
    public ResponseEntity<?> clearActiveSession(HttpServletRequest httpRequest) {
        try {
            Long currentUserId = getCurrentUserId(httpRequest);
            chatWebSocketService.clearActivePeer(currentUserId);
            return ResponseEntity.ok(ResponseUtil.successMsg("active session cleared"));
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(ResponseUtil.error(e.getMessage()));
        }
    }

    /**
     * 清除聊天缓存
     */
    @DeleteMapping("/cache/{userId}")
    public ResponseEntity<?> clearChatCache(@PathVariable Long userId, HttpServletRequest httpRequest) {
        try {
            Long currentUserId = getCurrentUserId(httpRequest);
            chatService.clearChatCache(currentUserId, userId);
            return ResponseEntity.ok(ResponseUtil.successMsg("缓存已清除"));
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(ResponseUtil.error(e.getMessage()));
        }
    }

    /**
     * 获取用户列表
     */
    @GetMapping("/users")
    public ResponseEntity<?> getUserList(HttpServletRequest httpRequest){
        try{
            Long currentUserId = getCurrentUserId(httpRequest);
            List<UserListDTO> users=chatService.getUserList(currentUserId);
            return ResponseEntity.ok(ResponseUtil.success(users));
        }catch(Exception e){
            return ResponseEntity.badRequest().body(ResponseUtil.error(e.getMessage()));
        }
    }


    /**
     * 标记消息为已读
     */
    @PostMapping("/mark-read/{userId}")
    public ResponseEntity<?> markMessagesAsRead(@PathVariable Long userId, HttpServletRequest httpRequest) {
        try {
            Long currentUserId = getCurrentUserId(httpRequest);
            chatService.markMessagesAsRead(userId, currentUserId);
            // 新增：推送未读数
            chatWebSocketService.pushUnreadMap(currentUserId);
            return ResponseEntity.ok(ResponseUtil.successMsg("消息已标记为已读"));
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(ResponseUtil.error(e.getMessage()));
        }
    }

    /**
     * 获取未读消息数
     */
    @GetMapping("/unread-count")
    public ResponseEntity<?> getUnreadMessageCount(HttpServletRequest httpRequest) {
        try {
            Long currentUserId = getCurrentUserId(httpRequest);
            Integer count = chatService.getUnreadMessageCount(currentUserId);
            return ResponseEntity.ok(ResponseUtil.success(count));
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(ResponseUtil.error(e.getMessage()));
        }
    }

    /**
     * 获取每个用户的未读消息数map
     */
    @GetMapping("/unread-map")
    public ResponseEntity<?> getUnreadMap(HttpServletRequest httpRequest) {
        try {
            Long currentUserId = getCurrentUserId(httpRequest);
            Map<Long, Integer> unreadMap = chatService.getUnreadCountMap(currentUserId);
            return ResponseEntity.ok(ResponseUtil.success(unreadMap));
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(ResponseUtil.error(e.getMessage()));
        }
    }
} 