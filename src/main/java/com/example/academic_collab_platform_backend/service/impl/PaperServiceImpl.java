package com.example.academic_collab_platform_backend.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.example.academic_collab_platform_backend.mapper.PaperAuthorMapper;
import com.example.academic_collab_platform_backend.mapper.PaperMapper;
import com.example.academic_collab_platform_backend.model.Paper;
import com.example.academic_collab_platform_backend.model.PaperAuthor;
import com.example.academic_collab_platform_backend.service.PaperService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 论文相关业务实现类，负责论文的检索、详情、统计等具体业务逻辑。
 */
@Service
public class PaperServiceImpl implements PaperService {

    @Autowired
    private PaperMapper paperMapper;

    @Autowired
    private PaperAuthorMapper paperAuthorMapper;

    /**
     * 分页搜索论文，根据标题或摘要模糊匹配。
     */
    @Override
    public IPage<Paper> searchPapers(String query, int page, int size) {
        QueryWrapper<Paper> wrapper = new QueryWrapper<>();
        if (query != null && !query.isEmpty()) {
            wrapper.lambda().like(Paper::getTitle, query)
                    .or().like(Paper::getAbstractText, query);
        }
        return paperMapper.selectPage(new Page<>(page, size), wrapper);
    }

    /**
     * 根据作者ID获取论文列表。
     */
    @Override
    public List<Paper> getPapersByAuthorId(Long authorId) {
        // 先查找作者与论文的关联关系
        List<PaperAuthor> relations = paperAuthorMapper.selectList(
                new QueryWrapper<PaperAuthor>().eq("author_id", authorId)
        );
        // 提取所有论文ID
        List<Long> paperIds = relations.stream().map(PaperAuthor::getPaperId).collect(Collectors.toList());
        // 查询所有论文
        return paperIds.isEmpty() ? Collections.emptyList() : paperMapper.selectBatchIds(paperIds);
    }

    /**
     * 根据关键词模糊搜索论文（无分页）。
     */
    @Override
    public List<Paper> searchPapersByKeyword(String keyword) {
        QueryWrapper<Paper> wrapper = new QueryWrapper<>();
        if (keyword != null && !keyword.isEmpty()) {
            wrapper.lambda().like(Paper::getTitle, keyword)
                    .or().like(Paper::getAbstractText, keyword);
        }
        return paperMapper.selectList(wrapper);
    }

    /**
     * 根据论文ID获取论文对象。
     */
    @Override
    public Paper getPaperById(Long id) {
        return paperMapper.selectById(id);
    }

    /**
     * 根据年份和关键词获取论文列表。
     */
    @Override
    public List<Paper> getPapersByYear(Integer year, String keyword) {
        QueryWrapper<Paper> wrapper = new QueryWrapper<>();
        if (year != null) {
            wrapper.lambda().eq(Paper::getYear, year);
        }
        if (keyword != null && !keyword.isEmpty()) {
            // 组合条件：标题或摘要包含关键词
            wrapper.lambda().and(w -> w.like(Paper::getTitle, keyword)
                    .or().like(Paper::getAbstractText, keyword));
        }
        return paperMapper.selectList(wrapper);
    }

    /**
     * 根据会议/期刊分页获取论文。
     */
    @Override
    public List<Paper> getPapersByVenue(String venue, int page, int size) {
        // 这里简化实现，实际可按venue过滤
        QueryWrapper<Paper> wrapper = new QueryWrapper<>();
        return paperMapper.selectPage(new Page<>(page, size), wrapper).getRecords();
    }

    /**
     * 获取论文统计信息。
     */
    @Override
    public Map<String, Object> getPaperStatistics() {
        Map<String, Object> stats = new HashMap<>();
        stats.put("totalPapers", paperMapper.selectCount(null));
        stats.put("message", "论文统计信息");
        return stats;
    }
} 