package com.example.academic_collab_platform_backend.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.example.academic_collab_platform_backend.mapper.AuthorMapper;
import com.example.academic_collab_platform_backend.mapper.PaperAuthorMapper;
import com.example.academic_collab_platform_backend.mapper.PaperMapper;
import com.example.academic_collab_platform_backend.model.Author;
import com.example.academic_collab_platform_backend.model.CollaborationResult;
import com.example.academic_collab_platform_backend.model.Paper;
import com.example.academic_collab_platform_backend.model.PaperAuthor;
import com.example.academic_collab_platform_backend.service.CollaborationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 合作关系分析与预测业务实现类，负责合作分析与合作预测等具体业务逻辑。
 */
@Service
public class CollaborationServiceImpl implements CollaborationService {

    @Autowired
    private PaperAuthorMapper paperAuthorMapper;

    @Autowired
    private PaperMapper paperMapper;

    @Autowired
    private AuthorMapper authorMapper;

    /**
     * 分析指定作者的合作关系。
     * 统计与其合作的作者、合作论文数、最近合作年份等。
     */
    @Override
    public List<CollaborationResult> analyzeCollaboration(Long authorId, Integer startYear, Integer endYear) {
        // 1. 查出该作者参与的所有paper_id
        List<PaperAuthor> myPapers = paperAuthorMapper.selectList(
                new QueryWrapper<PaperAuthor>().eq("author_id", authorId)
        );
        List<Long> paperIds = myPapers.stream().map(PaperAuthor::getPaperId).collect(Collectors.toList());
        if (paperIds.isEmpty()) return Collections.emptyList();




        // 2. 根据年份过滤论文
        QueryWrapper<Paper> paperWrapper = new QueryWrapper<>();
        paperWrapper.in("id", paperIds);
        if (startYear != null) paperWrapper.ge("year", startYear);
        if (endYear != null) paperWrapper.le("year", endYear);
        List<Paper> filteredPapers = paperMapper.selectList(paperWrapper);
        List<Long> filteredPaperIds = filteredPapers.stream().map(Paper::getId).collect(Collectors.toList());

        if (filteredPaperIds.isEmpty()) return Collections.emptyList();

        // 3. 查出这些paper_id下的所有作者
        List<PaperAuthor> allAuthors = paperAuthorMapper.selectList(
                new QueryWrapper<PaperAuthor>().in("paper_id", filteredPaperIds)
        );

        // 4. 统计合作者
        Map<Long, Integer> coauthorCount = new HashMap<>();
        Map<Long, Integer> coauthorLastYear = new HashMap<>();
        for (PaperAuthor pa : allAuthors) {
            if (!pa.getAuthorId().equals(authorId)) {
                coauthorCount.put(pa.getAuthorId(), coauthorCount.getOrDefault(pa.getAuthorId(), 0) + 1);
                // 找到该合作者的最新合作年份
                Paper paper = paperMapper.selectById(pa.getPaperId());
                if (paper != null) {
                    coauthorLastYear.put(pa.getAuthorId(), 
                        Math.max(coauthorLastYear.getOrDefault(pa.getAuthorId(), 0), paper.getYear()));
                }
            }
        }

        // 5. 返回结果
        List<CollaborationResult> result = new ArrayList<>();
        for (Map.Entry<Long, Integer> entry : coauthorCount.entrySet()) {
            CollaborationResult cr = new CollaborationResult();
            cr.setAuthor1Id(authorId);
            cr.setAuthor2Id(entry.getKey());
            cr.setPaperCount(entry.getValue());
            cr.setLastYear(coauthorLastYear.get(entry.getKey()));
            
            // 获取作者姓名
            Author author1 = authorMapper.selectById(authorId);
            Author author2 = authorMapper.selectById(entry.getKey());
            if (author1 != null) cr.setAuthor1Name(author1.getName());
            if (author2 != null) cr.setAuthor2Name(author2.getName());
            
            result.add(cr);
        }
        return result;
    }

    /**
     * 预测指定作者的潜在合作对象。
     * 简化实现：返回所有未合作过的作者姓名列表。
     */
    @Override
    public List<String> predictCollaborators(Long authorId, List<String> directions, Integer minPapers, Integer startYear, Integer endYear) {
        // 查找该作者已合作过的所有作者ID
        List<PaperAuthor> myPapers = paperAuthorMapper.selectList(
                new QueryWrapper<PaperAuthor>().eq("author_id", authorId)
        );
        Set<Long> coauthorIds = myPapers.stream().map(PaperAuthor::getPaperId)
                .flatMap(paperId -> {
                    List<PaperAuthor> paperAuthors = paperAuthorMapper.selectList(
                            new QueryWrapper<PaperAuthor>().eq("paper_id", paperId)
                    );
                    return paperAuthors.stream().map(PaperAuthor::getAuthorId);
                })
                .filter(id -> !id.equals(authorId))
                .collect(Collectors.toSet());

        // 获取所有作者
        List<Author> allAuthors = authorMapper.selectList(null);
        // 过滤掉已合作过的作者，返回姓名
        return allAuthors.stream()
                .filter(author -> !coauthorIds.contains(author.getId()))
                .map(Author::getName)
                .collect(Collectors.toList());
    }
} 