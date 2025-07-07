package com.example.academic_collab_platform_backend.service;

import com.example.academic_collab_platform_backend.model.Paper;
import java.util.List;

/**
 * 文献检索业务接口，定义文献的关键词检索等操作。
 */
public interface LiteratureService {
    /**
     * 根据关键词检索文献
     * @param keyword 关键词
     * @return 检索到的论文列表
     */
    List<Paper> searchPapersByKeyword(String keyword);

    /**
     * 检索请求体，包含关键词字段
     */
    class SearchRequest {
        private String query;
        public String getQuery() { return query; }
        public void setQuery(String query) { this.query = query; }
    }
} 