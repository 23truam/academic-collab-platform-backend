package com.example.academic_collab_platform_backend.controller;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.example.academic_collab_platform_backend.model.SearchHistory;
import com.example.academic_collab_platform_backend.service.SearchHistoryService;
import com.example.academic_collab_platform_backend.util.ResponseUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

/**
 * 最近搜索历史接口控制器，负责处理用户最近搜索历史的增删查请求。
 * 仅做参数接收和响应封装，具体业务由SearchHistoryService实现。
 */
@RestController
@RequestMapping("/api/search-history")
@CrossOrigin(origins = "*")
public class SearchHistoryController {

    @Autowired
    private SearchHistoryService searchHistoryService;

    /**
     * 分页获取指定用户的搜索历史
     * @param userId 用户ID
     * @param page 页码
     * @param size 每页数量
     * @return 分页搜索历史列表
     */
    @GetMapping
    public Map<String, Object> getHistory(
            @RequestParam String userId,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size) {
        IPage<SearchHistory> result = searchHistoryService.getHistory(userId, page, size);
        return ResponseUtil.success(result);
    }

    /**
     * 添加一条搜索历史（同时写入MySQL和Redis）
     * @param item 搜索历史对象
     */
    @PostMapping
    public Map<String, Object> addHistory(@RequestBody SearchHistory item) throws JsonProcessingException {
        searchHistoryService.addSearchHistory(item);
        return ResponseUtil.successMsg("搜索历史添加成功");
    }

    /**
     * 清空指定用户的搜索历史
     * @param userId 用户ID
     */
    @DeleteMapping
    public Map<String, Object> clearHistory(@RequestParam String userId) {
        searchHistoryService.clearHistory(userId);
        return ResponseUtil.successMsg("搜索历史清空成功");
    }

    /**
     * 获取指定用户的最近搜索历史（优先从Redis获取）
     * @param userId 用户ID
     * @param limit 限制数量
     * @return 最近搜索历史列表
     */
    @GetMapping("/recent")
    public Map<String, Object> getRecentSearchHistory(
            @RequestParam String userId,
            @RequestParam(defaultValue = "5") int limit) {
        List<SearchHistory> result = searchHistoryService.getRecentSearchHistory(userId, limit);
        return ResponseUtil.success(result);
    }
} 
 