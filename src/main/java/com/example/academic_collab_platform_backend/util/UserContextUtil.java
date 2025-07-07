package com.example.academic_collab_platform_backend.util;

import com.example.academic_collab_platform_backend.model.User;
import com.example.academic_collab_platform_backend.mapper.UserMapper;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class UserContextUtil {

    @Resource
    private UserMapper userMapper;

    /**
     * 获取当前登录用户的userId
     */
    public Long getCurrentUserId() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.getPrincipal() != null) {
            String email = authentication.getPrincipal().toString();
            User user = userMapper.selectByEmail(email);
            if (user != null) {
                return user.getId();
            }
        }
        return null;
    }
} 