package com.example.academic_collab_platform_backend.service;

import com.example.academic_collab_platform_backend.model.CollaborationResult;

import java.util.List;

/**
 * 合作关系分析与预测业务接口，定义合作分析与合作预测等操作。
 */
public interface CollaborationService {
    /**
     * 分析指定作者的合作关系
     * @param authorId 作者ID
     * @param startYear 可选，起始年份
     * @param endYear 可选，结束年份
     * @return 合作分析结果列表
     */
    List<CollaborationResult> analyzeCollaboration(Long authorId, Integer startYear, Integer endYear);

    /**
     * 预测指定作者的潜在合作对象
     * @param authorId 作者ID
     * @param directions 研究方向列表
     * @param minPapers 最少合作论文数
     * @param startYear 可选，起始年份
     * @param endYear 可选，结束年份
     * @return 预测的合作作者姓名列表
     */
    List<String> predictCollaborators(Long authorId, List<String> directions, Integer minPapers, Integer startYear, Integer endYear);
} 