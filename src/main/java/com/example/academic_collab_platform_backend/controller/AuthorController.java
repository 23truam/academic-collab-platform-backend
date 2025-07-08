package com.example.academic_collab_platform_backend.controller;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.example.academic_collab_platform_backend.model.Author;
import com.example.academic_collab_platform_backend.dto.AuthorDetailResponse;
import com.example.academic_collab_platform_backend.service.AuthorService;
import com.example.academic_collab_platform_backend.service.impl.AuthorServiceImpl;
import com.example.academic_collab_platform_backend.util.ResponseUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.HashMap;
import java.util.Map;
import java.util.ArrayList;

/**
 * 作者相关接口控制器，负责处理作者的查询、详情、统计等请求。
 * 仅做参数接收和响应封装，具体业务由AuthorService实现。
 */
@RestController
@RequestMapping("/api/authors")
@CrossOrigin(origins = "*")
public class AuthorController {

    @Autowired
    private AuthorService authorService;

    /**
     * 分页搜索作者
     * @param keyword 关键词
     * @param page 页码
     * @param size 每页数量
     * @return 分页作者列表
     */
    @GetMapping("/search")
    public IPage<Author> search(
            @RequestParam(required = false, defaultValue = "") String keyword,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size) {
        return authorService.searchAuthors(keyword, page, size);
    }

    /**
     * 根据论文ID获取作者列表
     * @param paperId 论文ID
     * @return 作者列表
     */
    @GetMapping("/by-paper/{paperId}")
    public List<Author> getAuthorsByPaper(@PathVariable Long paperId) {
        return authorService.getAuthorsByPaperId(paperId);
    }
    
    /**
     * 根据作者ID获取作者详情
     * @param id 作者ID
     * @return 作者对象，找不到返回404
     */
    @GetMapping("/{id}")
    public ResponseEntity<Author> getAuthorById(@PathVariable Long id) {
        Author author = authorService.getAuthorById(id);
        if (author != null) {
            return ResponseEntity.ok(author);
        } else {
            return ResponseEntity.notFound().build();
        }
    }
    
    /**
     * 获取论文数最多的前N位作者
     * @param limit 数量
     * @param year 可选，按年份过滤
     * @return 作者列表
     */
    @GetMapping("/top-authors")
    public List<Author> getTopAuthors(
            @RequestParam(defaultValue = "10") int limit,
            @RequestParam(required = false) Integer year) {
        return authorService.getTopAuthors(limit, year);
    }
    
    /**
     * 根据机构分页获取作者
     * @param institution 机构名
     * @param page 页码
     * @param size 每页数量
     * @return 作者列表
     */
    @GetMapping("/by-institution")
    public List<Author> getAuthorsByInstitution(
            @RequestParam String institution,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size) {
        return authorService.getAuthorsByInstitution(institution, page, size);
    }
    
    /**
     * 获取作者统计信息
     * @return 统计数据
     */
    @GetMapping("/statistics")
    public ResponseEntity<?> getAuthorStatistics() {
        return ResponseEntity.ok(authorService.getAuthorStatistics());
    }

    /**
     * 获取作者详情（含论文、合作者、统计等）
     * @param name 作者名
     * @return 详情数据，找不到时success=false
     */
    @GetMapping("/detail")
    public Map<String, Object> getAuthorDetail(@RequestParam String name) {
        AuthorDetailResponse resp = authorService.getAuthorDetailByName(name);
        if (resp == null) {
            System.out.println("Author not found: " + name);
            return ResponseUtil.error("未找到作者");
        }
        System.out.println("Author found: " + name);
        System.out.println("Resp: " + resp);
        return ResponseUtil.success(resp);
    }

    /**
     * 获取所有作者及其论文数（用于首页统计）
     * @return [{name, paperCount}...]
     */
    @GetMapping("/all-with-count")
    public List<Map<String, Object>> getAllAuthorsWithPaperCount() {
        List<Author> allAuthors = authorService.getTopAuthors(Integer.MAX_VALUE, null);
        List<Map<String, Object>> result = new ArrayList<>();
        for (Author author : allAuthors) {
            Map<String, Object> map = new HashMap<>();
            map.put("name", author.getName());
            map.put("paperCount", author.getPaperCount() != null ? author.getPaperCount() : 0);
            result.add(map);
        }
        return result;
    }

    /**
     * 初始化作者缓存
     * @return 缓存初始化结果
     */
    @PostMapping("/init-cache")
    public ResponseEntity<Map<String, Object>> initAuthorCache() {
        try {
            // 强制重新加载缓存
            if (authorService instanceof AuthorServiceImpl) {
                AuthorServiceImpl authorServiceImpl = (AuthorServiceImpl) authorService;
                authorServiceImpl.clearAuthorCache();
                // 触发缓存加载
                authorService.getAuthorStatistics();
                return ResponseEntity.ok(ResponseUtil.successMsg("作者缓存初始化成功"));
            } else {
                return ResponseEntity.ok(ResponseUtil.error("服务类型不匹配"));
            }
        } catch (Exception e) {
            return ResponseEntity.ok(ResponseUtil.error("缓存初始化失败: " + e.getMessage()));
        }
    }

    /**
     * 测试缓存状态
     * @return 缓存状态信息
     */
    @GetMapping("/cache-status")
    public ResponseEntity<Map<String, Object>> getCacheStatus() {
        try {
            if (authorService instanceof AuthorServiceImpl) {
                AuthorServiceImpl authorServiceImpl = (AuthorServiceImpl) authorService;
                // 测试缓存是否正常工作
                long startTime = System.currentTimeMillis();
                authorService.getAuthorStatistics();
                long endTime = System.currentTimeMillis();
                Map<String, Object> result = ResponseUtil.successMsg("缓存状态正常");
                result.put("cacheWorking", true);
                result.put("responseTime", endTime - startTime + "ms");
                return ResponseEntity.ok(result);
            } else {
                return ResponseEntity.ok(ResponseUtil.error("服务类型不匹配"));
            }
        } catch (Exception e) {
            return ResponseEntity.ok(ResponseUtil.error("缓存状态检查失败: " + e.getMessage()));
        }
    }
}