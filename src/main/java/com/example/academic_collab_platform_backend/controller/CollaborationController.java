package com.example.academic_collab_platform_backend.controller;

import com.example.academic_collab_platform_backend.model.CollaborationResult;
import com.example.academic_collab_platform_backend.service.CollaborationService;
import com.example.academic_collab_platform_backend.dto.CollaborationPredictRequest;
import com.example.academic_collab_platform_backend.dto.CollaborationPredictResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

/**
 * 合作关系分析与预测接口控制器，负责处理学者合作关系分析与合作预测请求。
 * 仅做参数接收和响应封装，具体业务由CollaborationService实现。
 */
@RestController
@RequestMapping("/api/collaboration")
@CrossOrigin(origins = "*")
public class CollaborationController {

    @Autowired
    private CollaborationService collaborationService;

    /**
     * 分析指定作者的合作关系
     * @param authorId 作者ID
     * @param startYear 可选，起始年份
     * @param endYear 可选，结束年份
     * @return 合作分析结果列表
     */
    @GetMapping("/analyze")
    public List<CollaborationResult> analyze(
            @RequestParam Long authorId,
            @RequestParam(required = false) Integer startYear,
            @RequestParam(required = false) Integer endYear) {
        return collaborationService.analyzeCollaboration(authorId, startYear, endYear);
    }

    /**
     * 预测指定作者的潜在合作对象
     * @param request 请求参数（authorId, directions, minPapers, startYear, endYear）
     * @return 预测的合作作者详细列表
     */
    @PostMapping("/predict")
    public List<CollaborationPredictResponse> predict(@RequestBody CollaborationPredictRequest request) {
        return collaborationService.predictCollaborators(
            request.getAuthorId(),
            request.getDirections(),
            request.getMinPapers(),
            request.getStartYear(),
            request.getEndYear()
        );
    }
} 