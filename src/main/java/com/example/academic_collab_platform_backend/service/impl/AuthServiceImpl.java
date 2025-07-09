package com.example.academic_collab_platform_backend.service.impl;

import com.example.academic_collab_platform_backend.dto.LoginRequest;
import com.example.academic_collab_platform_backend.dto.RegisterRequest;
import com.example.academic_collab_platform_backend.model.User;
import com.example.academic_collab_platform_backend.service.AuthService;
import com.example.academic_collab_platform_backend.service.UserService;
import com.example.academic_collab_platform_backend.service.ChatService;
import com.example.academic_collab_platform_backend.util.JwtUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.Map;

/**
 * 用户认证与鉴权业务实现类，负责注册、登录、token校验等具体业务逻辑。
 */
@Service
public class AuthServiceImpl implements AuthService {
    @Autowired
    private UserService userService;
    @Autowired
    private JwtUtil jwtUtil;
    @Autowired
    private PasswordEncoder passwordEncoder;
    @Autowired
    private ChatService chatService;
    @Autowired
    private com.example.academic_collab_platform_backend.mapper.UserOnlineStatusMapper userOnlineStatusMapper;

    /**
     * 用户注册。
     */
    @Override
    public User register(RegisterRequest request) {
        User user = new User();
        user.setUsername(request.getUsername());
        user.setEmail(request.getEmail());
        user.setPassword(request.getPassword());
        return userService.register(user);
    }

    /**
     * 用户登录。
     */
    @Override
    public User login(LoginRequest request) {
        String email = request.getEmail();
        String username = request.getUsername();
        String password = request.getPassword();
        if ((email == null || email.isEmpty()) && (username == null || username.isEmpty())) {
            throw new RuntimeException("请输入邮箱或用户名");
        }
        if (password == null || password.isEmpty()) {
            throw new RuntimeException("密码不能为空");
        }
        if (email != null && !email.isEmpty()) {
            return userService.login(email, password);
        } else {
            User user = userService.findByUsername(username);
            if (user == null) {
                throw new RuntimeException("用户不存在");
            }
            if (!passwordEncoder.matches(password, user.getPassword())) {
                throw new RuntimeException("密码错误");
            }
            if (!user.getEnabled()) {
                throw new RuntimeException("账户已被禁用");
            }
            return user;
        }
    }

    /**
     * 校验token有效性。
     */
    @Override
    public Map<String,Object> validateToken(String token){
        Map<String,Object> response=new HashMap<>();
        if(token!=null && token.startsWith("Bearer ")){
            token=token.substring(7);
        }try {
            String email = jwtUtil.extractEmail(token);
            String userId = jwtUtil.extractUserId(token);

            if (jwtUtil.validateToken(token, email)) {
                response.put("success", true);
                response.put("message", "Tokne 有效");
                response.put("email", email);
                response.put("userId", userId);
            } else {
                response.put("success", false);
                response.put("message", "Token验证无效");
            }
        }catch (Exception e){
            response.put("success", false);
            response.put("message", "Token验证失败");
        }
            return response;
    }

    /**
     * 用户退出登录。
     */
    @Override
    public Map<String, Object> logout(String token) {
        Map<String, Object> response = new HashMap<>();
        try {
            if (token != null && token.startsWith("Bearer ")) {
                token = token.substring(7);
            }
            String userId = jwtUtil.extractUserId(token);
            if (userId != null) {
                // 修改用户在线状态为false
                chatService.updateUserOnlineStatus(Long.valueOf(userId), false, null);
                // 记录退出时间
                userOnlineStatusMapper.updateLastLogoutTime(Long.valueOf(userId));
                response.put("success", true);
                response.put("message", "退出登录成功");
            } else {
                response.put("success", false);
                response.put("message", "Token无效");
            }
        } catch (Exception e) {
            response.put("success", false);
            response.put("message", "退出登录失败: " + e.getMessage());
        }
        return response;
    }
} 