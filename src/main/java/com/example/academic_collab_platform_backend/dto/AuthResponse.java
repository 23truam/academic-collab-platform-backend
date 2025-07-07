package com.example.academic_collab_platform_backend.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class AuthResponse {
    
    private String token;
    private String username;
    private String userId;
    private String message;
    
    public AuthResponse(String token, String username, String userId) {
        this.token = token;
        this.username = username;
        this.userId = userId;
    }
} 