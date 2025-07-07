package com.example.academic_collab_platform_backend.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.example.academic_collab_platform_backend.model.Paper;

import java.util.List;
import java.util.Map;

/**
 * 论文相关业务接口，定义论文的检索、详情、统计等操作。
 */
public interface PaperService {
    /**
     * 分页搜索论文
     * @param query 关键词
     * @param page 页码
     * @param size 每页数量
     * @return 分页论文列表
     */
    IPage<Paper> searchPapers(String query, int page, int size);

    /**
     * 根据作者ID获取论文列表
     * @param authorId 作者ID
     * @return 论文列表
     */
    List<Paper> getPapersByAuthorId(Long authorId);
    
    /**
     * 根据关键词模糊搜索论文（无分页）
     * @param keyword 关键词
     * @return 论文列表
     */
    List<Paper> searchPapersByKeyword(String keyword);

    /**
     * 根据论文ID获取论文对象
     * @param id 论文ID
     * @return 论文对象
     */
    Paper getPaperById(Long id);

    /**
     * 根据年份和关键词获取论文列表
     * @param year 年份
     * @param keyword 关键词
     * @return 论文列表
     */
    List<Paper> getPapersByYear(Integer year, String keyword);

    /**
     * 根据会议/期刊分页获取论文
     * @param venue 会议/期刊名
     * @param page 页码
     * @param size 每页数量
     * @return 论文列表
     */
    List<Paper> getPapersByVenue(String venue, int page, int size);

    /**
     * 获取论文统计信息
     * @return 统计数据
     */
    Map<String, Object> getPaperStatistics();
} 