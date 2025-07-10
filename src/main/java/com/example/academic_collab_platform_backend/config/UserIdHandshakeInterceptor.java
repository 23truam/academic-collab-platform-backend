package com.example.academic_collab_platform_backend.config;

import com.example.academic_collab_platform_backend.util.JwtUtil;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.web.socket.WebSocketHandler;
import org.springframework.web.socket.server.HandshakeInterceptor;
import java.util.List;
import java.util.Map;
import org.springframework.http.server.ServletServerHttpRequest;

public class UserIdHandshakeInterceptor implements HandshakeInterceptor {
    private final JwtUtil jwtUtil;
    public UserIdHandshakeInterceptor(JwtUtil jwtUtil) {
        this.jwtUtil = jwtUtil;
    }

    @Override
    public boolean beforeHandshake(ServerHttpRequest request, ServerHttpResponse response,
                                   WebSocketHandler  wsHandler, Map<String, Object> attributes) throws Exception {
        String token = null;
        if (request instanceof ServletServerHttpRequest) {
            ServletServerHttpRequest servletRequest = (ServletServerHttpRequest) request;
            token = servletRequest.getServletRequest().getParameter("token");
        }
        System.out.println("[WebSocket] 握手阶段 token: " + token);
        if (token != null) {
            String userId = jwtUtil.extractUserId(token);
            System.out.println("[WebSocket] 解析到userId: " + userId);
            if (userId != null) {
                attributes.put("userId", userId);
            }
        }
        return true;
    }


    @Override
    public void afterHandshake(ServerHttpRequest request, ServerHttpResponse response,
                              WebSocketHandler wsHandler, Exception exception) {
        String token=null;
        if(request instanceof ServletServerHttpRequest){
            ServletServerHttpRequest serverHttpRequest=(ServletServerHttpRequest) request;
            token = serverHttpRequest.getServletRequest().getParameter("token");
        }
        System.out.println("[Websocket] 连接成功 token: "+token);
        if(token!=null){
            String userId=jwtUtil.extractUserId(token);
            System.out.println("[Websocket] 连接成功 userId: "+userId);
        }
    }
} 