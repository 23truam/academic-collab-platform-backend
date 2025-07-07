package com.example.academic_collab_platform_backend.controller;

import com.example.academic_collab_platform_backend.dto.AuthResponse;
import com.example.academic_collab_platform_backend.dto.LoginRequest;
import com.example.academic_collab_platform_backend.dto.RegisterRequest;
import com.example.academic_collab_platform_backend.model.User;
import com.example.academic_collab_platform_backend.service.AuthService;
import com.example.academic_collab_platform_backend.util.JwtUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.HashMap;
import java.util.Map;

/**
 * 用户认证与鉴权接口控制器，负责处理注册、登录、token校验等请求。
 * 仅做参数接收和响应封装，具体业务由AuthService实现。
 */
@RestController
@RequestMapping("/api/auth")
@CrossOrigin(origins = "*")
public class AuthController {

    @Autowired
    private AuthService authService;

    @Autowired
    private JwtUtil jwtUtil;

    /**
     * 用户注册
     * @param request 注册请求体
     * @return 注册结果，包含用户信息
     */
    @PostMapping("/register")
    public ResponseEntity<?> register(@Valid @RequestBody RegisterRequest request) {
        try {
            User registeredUser = authService.register(request);
            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("message", "注册成功");
            response.put("email", registeredUser.getEmail());
            response.put("userId", registeredUser.getId());
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("message", e.getMessage());
            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * 用户登录
     * @param request 登录请求体
     * @return 登录结果，包含token和用户信息
     */
    @PostMapping("/login")
    public ResponseEntity<?> login(@Valid @RequestBody LoginRequest request) {
        try {
            User user = authService.login(request);
            String token = jwtUtil.generateToken(user.getEmail(), String.valueOf(user.getId()));
            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("message", "登录成功");
            response.put("token", token);
            response.put("email", user.getEmail());
            response.put("userId", user.getId());
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("message", e.getMessage());
            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * 校验token有效性
     * @param token Authorization头部token
     * @return 校验结果，包含用户信息
     */
    @GetMapping("/validate")
    public ResponseEntity<?> validateToken(@RequestHeader("Authorization") String token) {
        Map<String, Object> response = authService.validateToken(token);
        if (Boolean.TRUE.equals(response.get("success"))) {
            return ResponseEntity.ok(response);
        } else {
            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * 用户退出登录
     * @param token Authorization头部token，可选
     * @param tokenParam url参数token，可选
     * @return 退出登录结果
     */
    @PostMapping("/logout")
    public ResponseEntity<?> logout(
            @RequestHeader(value = "Authorization", required = false) String token,
            @RequestParam(value = "token", required = false) String tokenParam) {
        try {
            // 优先header，无header则取url参数
            String realToken = token != null ? token : tokenParam;
            Map<String, Object> response = authService.logout(realToken);
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("message", e.getMessage());
            return ResponseEntity.badRequest().body(response);
        }
    }
} 