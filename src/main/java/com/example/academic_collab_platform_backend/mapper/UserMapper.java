package com.example.academic_collab_platform_backend.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.example.academic_collab_platform_backend.model.User;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface UserMapper extends BaseMapper<User> {
    User selectByEmail(String email);
    List<com.example.academic_collab_platform_backend.dto.UserWithStatusDTO> selectAllUsersWithStatus();
} 