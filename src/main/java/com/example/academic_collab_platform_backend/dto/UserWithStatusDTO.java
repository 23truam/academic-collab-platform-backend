package com.example.academic_collab_platform_backend.dto;

import lombok.Data;

@Data
public class UserWithStatusDTO {
    private Long id;
    private String username;
    private String email;
    private Boolean isOnline;
} 