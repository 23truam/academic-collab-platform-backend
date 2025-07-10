package com.example.academic_collab_platform_backend.dto;

public class UserListDTO {
    private Long userId;
    private String username;
    private Boolean isOnline;

    public UserListDTO() {}
    public UserListDTO(Long userId, String username, Boolean isOnline) {
        this.userId = userId;
        this.username = username;
        this.isOnline = isOnline;
    }
    public Long getUserId() { return userId; }
    public void setUserId(Long userId) { this.userId = userId; }
    public String getUsername() { return username; }
    public void setUsername(String username) { this.username = username; }
    public Boolean getIsOnline() { return isOnline; }
    public void setIsOnline(Boolean isOnline) { this.isOnline = isOnline; }
} 