package com.example.academic_collab_platform_backend.service;

import com.example.academic_collab_platform_backend.model.User;
import java.util.List;

public interface UserService {
    
    User register(User user);
    
    User login(String email, String password);
    
    User findByEmail(String email);
    
    User findById(Long id);
    
    boolean existsByEmail(String email);
    
    User findByUsername(String username);
    
    boolean existsByUsername(String username);
    
    User save(User user);
    
    List<User> findAll();
    
    void deleteById(Long id);
} 