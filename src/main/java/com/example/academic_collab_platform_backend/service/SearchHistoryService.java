package com.example.academic_collab_platform_backend.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.example.academic_collab_platform_backend.model.SearchHistory;
import com.fasterxml.jackson.core.JsonProcessingException;

import java.util.List;

/**
 * 搜索历史相关业务接口，定义搜索历史的增删查等操作。
 */
public interface SearchHistoryService {
    /**
     * 分页获取指定用户的搜索历史
     * @param userId 用户ID
     * @param page 页码
     * @param size 每页数量
     * @return 分页搜索历史列表
     */
    IPage<SearchHistory> getHistory(String userId, int page, int size);

    /**
     * 添加一条搜索历史
     * @param item 搜索历史对象
     */
    void addHistory(SearchHistory item);

    /**
     * 清空指定用户的搜索历史
     * @param userId 用户ID
     */
    void clearHistory(String userId);

    /**
     * 添加一条搜索历史（带缓存）
     * @param history 搜索历史对象
     */
    void addSearchHistory(SearchHistory history) throws JsonProcessingException;

    /**
     * 获取指定用户的最近搜索历史（优先查缓存）
     * @param userId 用户ID
     * @param limit 返回条数
     * @return 搜索历史列表
     */
    List<SearchHistory> getRecentSearchHistory(String userId, int limit);
} 