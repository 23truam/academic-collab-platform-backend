package com.example.academic_collab_platform_backend.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.example.academic_collab_platform_backend.model.UserOnlineStatus;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Update;

@Mapper
public interface UserOnlineStatusMapper extends BaseMapper<UserOnlineStatus> {
    void updateOnlineStatus(Long userId, Boolean isOnline);

    @Update("UPDATE user_online_status SET last_logout_time = NOW() WHERE user_id = #{userId}")
    void updateLastLogoutTime(@Param("userId") Long userId);
} 