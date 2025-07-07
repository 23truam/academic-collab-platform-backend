package com.example.academic_collab_platform_backend.controller;

import com.example.academic_collab_platform_backend.model.Author;
import com.example.academic_collab_platform_backend.service.ProfessorSearchService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 教授检索接口控制器，负责处理教授的关键词检索请求。
 * 仅做参数接收和响应封装，具体业务由ProfessorSearchService实现。
 */
@RestController
@RequestMapping("/api/professor-search")
@CrossOrigin(origins = "*")
public class ProfessorSearchController {

    @Autowired
    private ProfessorSearchService professorSearchService;

    /**
     * 根据关键词检索教授
     * @param keyword 关键词
     * @return 检索到的作者列表
     */
    @GetMapping
    public ResponseEntity<List<Author>> searchProfessors(@RequestParam String keyword) {
        try {
            List<Author> authors = professorSearchService.searchAuthorsByKeyword(keyword);
            return ResponseEntity.ok(authors);
        } catch (Exception e) {
            return ResponseEntity.badRequest().build();
        }
    }
} 