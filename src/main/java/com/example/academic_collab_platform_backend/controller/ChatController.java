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
import org.springframework.web.multipart.MultipartRequest;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/chat")
@CrossOrigin(origins = "*")
public class ChatController {

    @Autowired
    private ChatService chatService;
    @Autowired
    private JwtUtil jwtUtil;

    private Long getCurrentUserId(HttpServletRequest request) {
        String token = request.getHeader("Authorization");
        if (token != null && token.startsWith("Bearer ")) {
            token = token.substring(7);
        }
        return Long.valueOf(jwtUtil.extractUserId(token));
    }

    /**
     * 发送消息
     */
    @PostMapping("/send")
    public ResponseEntity<?> sendMessage(@RequestBody ChatMessageRequest request,HttpServletRequest httpRequest){
        try{
            Long senderId=getCurrentUserId(httpRequest);
            ChatMessageResponse response=chatService.sendMessage(senderId,request);
            return ResponseEntity.ok(ResponseUtil.success(response));
        }catch (Exception e){
            return ResponseEntity.badRequest().body(ResponseUtil.error(e.getMessage()));
        }
    }

    /**
     * 获取聊天历史
     */
    @GetMapping("/history/{userId}")
    public ResponseEntity<?> getChatHistory(@PathVariable Long userId, @RequestParam(defaultValue = "50") Integer limit, HttpServletRequest httpRequest, MultipartRequest multipartRequest){
        try{
            Long currentUserId=getCurrentUserId(httpRequest);
            List<ChatMessageResponse> messages=chatService.getChatHistory(currentUserId,userId,limit);
            return ResponseEntity.ok(ResponseUtil.success(messages));
        }catch(Exception e){
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
} 