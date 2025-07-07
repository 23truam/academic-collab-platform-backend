package com.example.academic_collab_platform_backend.service;

import com.example.academic_collab_platform_backend.model.Author;
import java.util.List;

/**
 * 教授检索业务接口，定义教授的关键词检索等操作。
 */
public interface ProfessorSearchService {
    /**
     * 根据关键词检索教授
     * @param keyword 关键词
     * @return 检索到的作者列表
     */
    List<Author> searchAuthorsByKeyword(String keyword);
} 