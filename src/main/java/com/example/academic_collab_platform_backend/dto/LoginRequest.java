package com.example.academic_collab_platform_backend.dto;

import lombok.Data;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;

@Data
public class LoginRequest {
    
    private String email;
    private String username;
    
    @NotBlank(message = "密码不能为空")
    private String password;
} 