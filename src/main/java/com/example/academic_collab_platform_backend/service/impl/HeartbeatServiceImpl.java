package com.example.academic_collab_platform_backend.service.impl;

import com.example.academic_collab_platform_backend.service.HeartbeatService;
import com.example.academic_collab_platform_backend.util.JwtUtil;
import com.example.academic_collab_platform_backend.util.RedisUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import javax.servlet.http.HttpServletRequest;
import java.util.concurrent.TimeUnit;

@Service
public class HeartbeatServiceImpl implements HeartbeatService {
    @Autowired
    private RedisUtil redisUtil;
    @Autowired
    private JwtUtil jwtUtil;

    @Override
    public ResponseEntity<?> handleHeartbeat(HttpServletRequest request) {
        try {
            String authHeader = request.getHeader("Authorization");
            if (authHeader == null || !authHeader.startsWith("Bearer ")) {
                return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body("Token not found");
            }
            String token = authHeader.substring(7);
            String email = jwtUtil.extractEmail(token);
            if (email == null || !jwtUtil.validateToken(token, email)) {
                return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body("Invalid token");
            }
            String onlineKey = "online:" + email;
            redisUtil.set(onlineKey, String.valueOf(System.currentTimeMillis()), 15, TimeUnit.SECONDS);
            return ResponseEntity.ok().body("Heartbeat received");
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Heartbeat failed: " + e.getMessage());
        }
    }


    @Override
    public ResponseEntity<?> checkOnlineStatus(String email) {
        try {
            String onlineKey = "online:" + email;
            boolean isOnline = redisUtil.exists(onlineKey);
            return ResponseEntity.ok().body(isOnline);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Check status failed: " + e.getMessage());
        }
    }


    @Override
    public ResponseEntity<?> getOnlineUsers() {
        try {
            return ResponseEntity.ok().body("Online users check endpoint - implement pattern matching as needed");
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Get online users failed: " + e.getMessage());
        }
    }
} 