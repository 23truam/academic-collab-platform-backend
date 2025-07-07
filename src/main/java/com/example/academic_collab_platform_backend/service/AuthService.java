package com.example.academic_collab_platform_backend.service;

import com.example.academic_collab_platform_backend.dto.LoginRequest;
import com.example.academic_collab_platform_backend.dto.RegisterRequest;
import com.example.academic_collab_platform_backend.model.User;

import java.util.Map;

/**
 * 用户认证与鉴权业务接口，定义注册、登录、token校验等操作。
 */
public interface AuthService {
    /**
     * 用户注册
     * @param request 注册请求体
     * @return 注册用户对象
     */
    User register(RegisterRequest request);

    /**
     * 用户登录（支持用户名或邮箱）
     * @param request 登录请求体
     * @return 登录用户对象
     */
    User login(LoginRequest request);

    /**
     * 校验token有效性
     * @param token token字符串
     * @return 校验结果，包含用户信息
     */
    Map<String, Object> validateToken(String token);

    /**
     * 用户退出登录
     * @param token token字符串
     * @return 退出登录结果
     */
    Map<String, Object> logout(String token);
} 