package com.example.academic_collab_platform_backend.service.impl;

import com.example.academic_collab_platform_backend.model.Author;
import com.example.academic_collab_platform_backend.service.ProfessorSearchService;
import com.example.academic_collab_platform_backend.service.AuthorService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 教授检索业务实现类，负责教授的关键词检索等具体业务逻辑。
 */
@Service
public class ProfessorSearchServiceImpl implements ProfessorSearchService {
    @Autowired
    private AuthorService authorService;

    /**
     * 根据关键词检索教授。
     */
    @Override
    public List<Author> searchAuthorsByKeyword(String keyword) {
        return authorService.searchAuthorsByKeyword(keyword);
    }
} 