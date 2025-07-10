package com.example.academic_collab_platform_backend.controller;

import com.example.academic_collab_platform_backend.dto.ChatMessageRequest;
import com.example.academic_collab_platform_backend.dto.ChatMessageResponse;
import com.example.academic_collab_platform_backend.service.ChatService;
import com.example.academic_collab_platform_backend.service.ChatWebSocketService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.messaging.handler.annotation.SendTo;
import org.springframework.messaging.simp.SimpMessageHeaderAccessor;
import org.springframework.stereotype.Controller;

import java.util.Map;

@Controller
public class ChatWebSocketController {

/*    @Autowired
    private ChatWebSocketService chatWebSocketService;

    @MessageMapping("/chat.sendMessage")
    @SendTo("/topic/public")
    public ChatMessageResponse sendMessage(@Payload ChatMessageRequest chatMessageRequest, SimpMessageHeaderAccessor headerAccessor) {
        // 从WebSocket session获取userId
        String userIdStr = (String) headerAccessor.getSessionAttributes().get("userId");
        Long senderId = userIdStr != null ? Long.valueOf(userIdStr) : null;
        if (senderId == null) {
            throw new RuntimeException("未认证用户，无法发送消息");
        }
        return chatWebSocketService.handleSendMessage(senderId, chatMessageRequest);
    }

    @MessageMapping("/chat.addUser")
    @SendTo("/topic/public")
    public ChatMessageResponse addUser(@Payload Map<String, Object> payload, 
                                     SimpMessageHeaderAccessor headerAccessor) {
        // 添加用户到WebSocket session
        String userId = payload.get("userId").toString();
        headerAccessor.getSessionAttributes().put("userId", userId);
        chatWebSocketService.handleUserConnect(Long.valueOf(userId), headerAccessor.getSessionId());
        ChatMessageResponse response = new ChatMessageResponse();
        response.setContent("用户 " + userId + " 已连接");
        response.setMessageType("SYSTEM");
        return response;
    }

    @MessageMapping("/chat.leave")
    @SendTo("/topic/public")
    public ChatMessageResponse leaveUser(@Payload Map<String, Object> payload,
                                       SimpMessageHeaderAccessor headerAccessor) {
        String userId = payload.get("userId").toString();
        chatWebSocketService.handleUserDisconnect(Long.valueOf(userId));
        ChatMessageResponse response = new ChatMessageResponse();
        response.setContent("用户 " + userId + " 已断开连接");
        response.setMessageType("SYSTEM");
        return response;
    }*/

    @Autowired
    ChatWebSocketService chatWebSocketService;

    @MessageMapping("/chat.addUser")
    @SendTo("/topic/public")
    public ChatMessageResponse addUser(@Payload Map<String,Object> payload,SimpMessageHeaderAccessor headerAccessor){
        String userId=payload.get("userId").toString();
        headerAccessor.getSessionAttributes().put("userId",userId);
        // 确保attributes中也有userId，供HandshakeHandler使用
        headerAccessor.setUser(() -> userId);
        chatWebSocketService.handleUserConnect(Long.valueOf(userId), headerAccessor.getSessionId());
        return null;
    }

    @MessageMapping("/chat.sendMessage")
    public void sendMessage(@Payload ChatMessageRequest chatMessageRequest, SimpMessageHeaderAccessor headerAccessor){
        String currentUserId = headerAccessor.getSessionAttributes().get("userId").toString();
        System.out.println("[WebSocket] ChatWebSocketController.sendMessage 被调用, userId=" + currentUserId + ", 消息内容: " + chatMessageRequest);
        Long userId = currentUserId != null ? Long.valueOf(currentUserId) : null;
        if(userId == null)
            throw new RuntimeException("未认证用户，无法发送消息!");
        chatWebSocketService.handleSendMessage(userId, chatMessageRequest);
        // 不再return，也不@SendTo
    }

    @MessageMapping("/chat.leave")
    @SendTo("/topic/public")
    public ChatMessageResponse leaveUser(@Payload Map<String,Object> payload,SimpMessageHeaderAccessor headerAccessor){
        String userId=payload.get("userId").toString();
        headerAccessor.getSessionAttributes().put("userId",userId);
        chatWebSocketService.handleUserDisconnect(Long.valueOf(userId));
        ChatMessageResponse response=new ChatMessageResponse();
        response.setContent("用户"+userId+"已断开链接");
        response.setMessageType("system");
        return response;
    }


} 