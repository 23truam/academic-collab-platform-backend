package com.example.academic_collab_platform_backend.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.example.academic_collab_platform_backend.model.PaperAuthor;
import org.apache.ibatis.annotations.Mapper;

@Mapper
public interface PaperAuthorMapper extends BaseMapper<PaperAuthor> {
    /**
     * 统计每位作者对应的论文数量
     * 返回列表元素为 Map，包含键：authorId（Long），cnt（Integer）
     */
    java.util.List<java.util.Map<String, Object>> selectAuthorPaperCounts();
} 