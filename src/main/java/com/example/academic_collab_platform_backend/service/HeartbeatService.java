package com.example.academic_collab_platform_backend.service;

import javax.servlet.http.HttpServletRequest;
import org.springframework.http.ResponseEntity;

public interface HeartbeatService {
    ResponseEntity<?> handleHeartbeat(HttpServletRequest request);
    ResponseEntity<?> checkOnlineStatus(String email);
    ResponseEntity<?> getOnlineUsers();
} 