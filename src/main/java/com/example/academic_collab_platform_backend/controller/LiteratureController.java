package com.example.academic_collab_platform_backend.controller;

import com.example.academic_collab_platform_backend.model.Paper;
import com.example.academic_collab_platform_backend.service.LiteratureService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 文献检索接口控制器，负责处理文献的关键词检索请求。
 */
@RestController
@RequestMapping("/api/literature")
@CrossOrigin(origins = "*")
public class LiteratureController {

    @Autowired
    private LiteratureService literatureService;

    /**
     * 根据关键词检索文献
     * @param request 检索请求体（包含query字段）
     * @return 检索到的论文列表
     */
    @PostMapping("/search")
    public ResponseEntity<List<Paper>> searchLiterature(@RequestBody LiteratureService.SearchRequest request) {
        try {
            List<Paper> papers = literatureService.searchPapersByKeyword(request.getQuery());
            return ResponseEntity.ok(papers);
        } catch (Exception e) {
            return ResponseEntity.badRequest().build();
        }
    }
} 