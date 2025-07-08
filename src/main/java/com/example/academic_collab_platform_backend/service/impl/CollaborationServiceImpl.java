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
import com.example.academic_collab_platform_backend.dto.CollaborationPredictResponse;
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
     * 迁移前端算法，返回详细DTO列表。
     */
    @Override
    public List<CollaborationPredictResponse> predictCollaborators(Long authorId, List<String> directions, Integer minPapers, Integer startYear, Integer endYear) {
        // 1. 获取该作者的所有论文
        List<PaperAuthor> myPapers = paperAuthorMapper.selectList(
                new QueryWrapper<PaperAuthor>().eq("author_id", authorId)
        );
        List<Long> myPaperIds = myPapers.stream().map(PaperAuthor::getPaperId).collect(Collectors.toList());
        if (myPaperIds.isEmpty()) return Collections.emptyList();

        // 2. 过滤论文（按年份）
        QueryWrapper<Paper> paperWrapper = new QueryWrapper<>();
        paperWrapper.in("id", myPaperIds);
        if (startYear != null) paperWrapper.ge("year", startYear);
        if (endYear != null) paperWrapper.le("year", endYear);
        List<Paper> authorPapers = paperMapper.selectList(paperWrapper);
        if (authorPapers.isEmpty()) return Collections.emptyList();

        // 3. 获取合作者集合
        Set<Long> coauthorIds = new HashSet<>();
        for (Paper paper : authorPapers) {
            List<PaperAuthor> pas = paperAuthorMapper.selectList(new QueryWrapper<PaperAuthor>().eq("paper_id", paper.getId()));
            for (PaperAuthor pa : pas) {
                if (!pa.getAuthorId().equals(authorId)) {
                    coauthorIds.add(pa.getAuthorId());
                }
            }
        }

        // 4. 获取所有作者，过滤掉自己和已合作过的
        List<Author> allAuthors = authorMapper.selectList(null);
        List<Author> potentialCoauthors = allAuthors.stream()
                .filter(a -> !a.getId().equals(authorId) && !coauthorIds.contains(a.getId()))
                .collect(Collectors.toList());

        // 5. 计算每个潜在合作者的分数
        List<CollaborationPredictResponse> result = new ArrayList<>();
        for (Author coauthor : potentialCoauthors) {
            // 获取潜在合作者的论文
            List<PaperAuthor> coPapers = paperAuthorMapper.selectList(
                    new QueryWrapper<PaperAuthor>().eq("author_id", coauthor.getId())
            );
            List<Long> coPaperIds = coPapers.stream().map(PaperAuthor::getPaperId).collect(Collectors.toList());
            if (coPaperIds.isEmpty()) continue;

            QueryWrapper<Paper> coPaperWrapper = new QueryWrapper<>();
            coPaperWrapper.in("id", coPaperIds);
            if (startYear != null) coPaperWrapper.ge("year", startYear);
            if (endYear != null) coPaperWrapper.le("year", endYear);
            List<Paper> coauthorPapers = paperMapper.selectList(coPaperWrapper);
            if (coauthorPapers.size() < (minPapers != null ? minPapers : 1)) continue;

            // 计算共同合作者数
            Set<Long> coCoauthorIds = new HashSet<>();
            for (Paper paper : coauthorPapers) {
                List<PaperAuthor> pas = paperAuthorMapper.selectList(new QueryWrapper<PaperAuthor>().eq("paper_id", paper.getId()));
                for (PaperAuthor pa : pas) {
                    if (!pa.getAuthorId().equals(coauthor.getId())) {
                        coCoauthorIds.add(pa.getAuthorId());
                    }
                }
            }
            int commonCoauthors = (int) coCoauthorIds.stream().filter(coauthorIds::contains).count();

            // 研究方向向量（简单实现：统计论文标题/方向字段出现的关键词）
            double[] authorVec = getDirectionVector(authorPapers, directions);
            double[] coauthorVec = getDirectionVector(coauthorPapers, directions);
            double cosineScore = cosineSimilarity(authorVec, coauthorVec);
            double jsScore = 1 - Math.min(1, jsDivergence(authorVec, coauthorVec));
            double directionScore = 0.7 * cosineScore + 0.3 * jsScore;
            double baseScore = Math.max(0.3, Math.min(0.9, directionScore));
            double finalDirectionScore = baseScore + (Math.random() * 0.1 - 0.05);

            CollaborationPredictResponse resp = new CollaborationPredictResponse();
            resp.setName(coauthor.getName());
            resp.setPaperCount(coauthorPapers.size());
            resp.setCommonCoauthors(commonCoauthors);
            resp.setDirectionScore(String.format("%.1f%%", finalDirectionScore * 100));
            resp.setTotalScore(String.format("%.1f%%", finalDirectionScore * 100));
            resp.setScoreValue(finalDirectionScore);
            List<CollaborationPredictResponse.Indicator> indicators = new ArrayList<>();
            CollaborationPredictResponse.Indicator i1 = new CollaborationPredictResponse.Indicator();
            i1.setName("论文数量"); i1.setValue(coauthorPapers.size());
            CollaborationPredictResponse.Indicator i2 = new CollaborationPredictResponse.Indicator();
            i2.setName("共同合作者"); i2.setValue(commonCoauthors);
            CollaborationPredictResponse.Indicator i3 = new CollaborationPredictResponse.Indicator();
            i3.setName("研究方向匹配度"); i3.setValue(finalDirectionScore * 10);
            indicators.add(i1); indicators.add(i2); indicators.add(i3);
            resp.setIndicators(indicators);
            result.add(resp);
        }
        // 按分数排序，取前10
        result.sort((a, b) -> Double.compare(b.getScoreValue(), a.getScoreValue()));
        return result.size() > 10 ? result.subList(0, 10) : result;
    }

    // 余弦相似度
    private double cosineSimilarity(double[] v1, double[] v2) {
        if (v1.length != v2.length) return 0;
        double dot = 0, norm1 = 0, norm2 = 0;
        for (int i = 0; i < v1.length; i++) {
            dot += v1[i] * v2[i];
            norm1 += v1[i] * v1[i];
            norm2 += v2[i] * v2[i];
        }
        if (norm1 == 0 && norm2 == 0) return 1;
        if (norm1 == 0 || norm2 == 0) return 0;
        return dot / (Math.sqrt(norm1) * Math.sqrt(norm2));
    }

    // JS散度
    private double jsDivergence(double[] p, double[] q) {
        if (p.length != q.length) return 1;
        double[] pNorm = normalize(p);
        double[] qNorm = normalize(q);
        double[] m = new double[p.length];
        for (int i = 0; i < p.length; i++) m[i] = (pNorm[i] + qNorm[i]) / 2;
        return (kl(pNorm, m) + kl(qNorm, m)) / 2;
    }
    private double kl(double[] a, double[] b) {
        double sum = 0;
        for (int i = 0; i < a.length; i++) {
            if (a[i] == 0) continue;
            sum += a[i] * Math.log(a[i] / b[i]);
        }
        return sum;
    }
    private double[] normalize(double[] arr) {
        double sum = Arrays.stream(arr).sum();
        if (sum == 0) return Arrays.stream(arr).map(x -> 1.0 / arr.length).toArray();
        return Arrays.stream(arr).map(x -> x / sum).toArray();
    }

    // 研究方向向量
    private double[] getDirectionVector(List<Paper> papers, List<String> directions) {
        double[] vec = new double[directions.size()];
        if (papers == null || papers.isEmpty()) return vec;
        for (Paper paper : papers) {
            String title = paper.getTitle() != null ? paper.getTitle().toLowerCase() : "";
            for (int i = 0; i < directions.size(); i++) {
                String dir = directions.get(i);
                if (title.contains(dir)) {
                    vec[i]++;
                }
            }
        }
        // 若全为0，兜底均匀分布
        boolean allZero = Arrays.stream(vec).allMatch(v -> v == 0);
        if (allZero) Arrays.fill(vec, 1.0);
        return vec;
    }
} 