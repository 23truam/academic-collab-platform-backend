package com.example.academic_collab_platform_backend.controller;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.example.academic_collab_platform_backend.model.Paper;
import com.example.academic_collab_platform_backend.service.PaperService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 论文相关接口控制器，负责处理论文的检索、详情、统计等请求。
 * 仅做参数接收和响应封装，具体业务由PaperService实现。
 */
@RestController
@RequestMapping("/api/papers")
@CrossOrigin(origins = "*")
public class PaperController {

    @Autowired
    private PaperService paperService;

    /**
     * 分页搜索论文
     * @param query 关键词
     * @param page 页码
     * @param size 每页数量
     * @return 分页论文列表
     */
    @GetMapping("/search")
    public IPage<Paper> search(
            @RequestParam(required = false, defaultValue = "") String query,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size) {
        return paperService.searchPapers(query, page, size);
    }

    /**
     * 根据作者ID获取论文列表
     * @param authorId 作者ID
     * @return 论文列表
     */
    @GetMapping("/by-author/{authorId}")
    public List<Paper> getPapersByAuthor(@PathVariable Long authorId) {
        return paperService.getPapersByAuthorId(authorId);
    }
    
    /**
     * 根据论文ID获取论文详情
     * @param id 论文ID
     * @return 论文对象，找不到返回404
     */
    @GetMapping("/{id}")
    public ResponseEntity<Paper> getPaperById(@PathVariable Long id) {
        Paper paper = paperService.getPaperById(id);
        if (paper != null) {
            return ResponseEntity.ok(paper);
        } else {
            return ResponseEntity.notFound().build();
        }
    }
    
    /**
     * 根据年份和关键词获取论文列表
     * @param year 年份
     * @param keyword 关键词
     * @return 论文列表
     */
    @GetMapping("/by-year")
    public List<Paper> getPapersByYear(
            @RequestParam Integer year,
            @RequestParam(required = false) String keyword) {
        return paperService.getPapersByYear(year, keyword);
    }
    
    /**
     * 根据会议/期刊分页获取论文
     * @param venue 会议/期刊名
     * @param page 页码
     * @param size 每页数量
     * @return 论文列表
     */
    @GetMapping("/by-venue")
    public List<Paper> getPapersByVenue(
            @RequestParam String venue,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size) {
        return paperService.getPapersByVenue(venue, page, size);
    }
    
    /**
     * 获取论文统计信息
     * @return 统计数据
     */
    @GetMapping("/statistics")
    public ResponseEntity<?> getPaperStatistics() {
        return ResponseEntity.ok(paperService.getPaperStatistics());
    }
} 