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

import javax.annotation.PostConstruct;
import java.util.*;
import java.util.stream.Collectors;
import org.apache.commons.math3.linear.*;

/**
 * 合作关系分析与预测业务实现类，使用词袋模型和两层GCN进行嵌入
 */
@Service
public class CollaborationServiceImpl implements CollaborationService {

    @Autowired
    private PaperAuthorMapper paperAuthorMapper;

    @Autowired
    private PaperMapper paperMapper;

    @Autowired
    private AuthorMapper authorMapper;

    // 嵌入向量缓存 (作者ID -> 嵌入向量)
    private Map<Long, double[]> authorEmbeddings = new HashMap<>();

    // 词汇表索引映射
    private Map<String, Integer> vocabulary = new HashMap<>();

    // 邻接矩阵
    private RealMatrix adjacencyMatrix;

    // 作者ID到矩阵索引的映射
    private Map<Long, Integer> authorIndexMap = new HashMap<>();
    private Map<Integer, Long> indexAuthorMap = new HashMap<>();

    // 标记是否已初始化
    private boolean embeddingsInitialized = false;

    /**
     * 初始化嵌入向量（在服务启动后执行）
     */
    @PostConstruct
    public void initEmbeddings() {
        try {
            // 1. 构建词汇表
            buildVocabulary();

            // 2. 构建作者索引映射
            List<Author> allAuthors = authorMapper.selectList(null);
            for (int i = 0; i < allAuthors.size(); i++) {
                Author author = allAuthors.get(i);
                authorIndexMap.put(author.getId(), i);
                indexAuthorMap.put(i, author.getId());
            }

            // 3. 构建邻接矩阵
            buildAdjacencyMatrix(allAuthors);

            // 4. 构建特征矩阵 (词袋模型)
            RealMatrix featureMatrix = buildFeatureMatrix(allAuthors);

            // 5. 运行两层GCN
            computeGCNEmbeddings(featureMatrix);

            embeddingsInitialized = true;
            System.out.println("GCN embeddings initialized successfully. Authors: " + allAuthors.size());
        } catch (Exception e) {
            System.err.println("Error initializing GCN embeddings: " + e.getMessage());
            e.printStackTrace();
        }
    }

    /**
     * 构建词汇表（从所有论文标题中提取）
     */
    private void buildVocabulary() {
        List<Paper> allPapers = paperMapper.selectList(null);
        Set<String> uniqueWords = new HashSet<>();

        for (Paper paper : allPapers) {
            if (paper.getTitle() != null) {
                String[] words = paper.getTitle().toLowerCase()
                        .replaceAll("[^a-z0-9 ]", "") // 移除非字母数字字符
                        .split("\\s+");

                // 过滤停用词和短词
                for (String word : words) {
                    if (word.length() > 3 && !isStopWord(word)) {
                        uniqueWords.add(word);
                    }
                }
            }
        }

        // 创建词汇表索引
        int index = 0;
        for (String word : uniqueWords) {
            vocabulary.put(word, index++);
        }
        System.out.println("Vocabulary built with " + vocabulary.size() + " words.");
    }

    /**
     * 简单停用词过滤
     */
    private boolean isStopWord(String word) {
        Set<String> stopWords = new HashSet<>(Arrays.asList(
                "the", "and", "of", "to", "in", "a", "an", "on", "for", "with", "by", "at",
                "is", "are", "as", "from", "that", "this", "these", "those", "be", "been", "was",
                "were", "it", "its", "which", "or", "but", "not", "have", "has", "had", "do", "does",
                "did", "will", "would", "should", "can", "could", "may", "might", "must", "about",
                "above", "below", "under", "over", "between", "among", "through", "during", "before",
                "after", "since", "while", "where", "when", "why", "how", "what", "who", "whom",
                "whose", "there", "here", "other", "others", "such", "some", "any", "all", "many",
                "few", "more", "most", "much", "little", "less", "least", "only", "just", "also",
                "very", "too", "so", "than", "then", "now", "well", "like", "into", "out", "up",
                "down", "off", "again", "further", "once", "twice", "times", "first", "second",
                "last", "next", "previous", "same", "different", "both", "each", "every", "either",
                "neither", "whether", "else", "otherwise", "however", "therefore", "thus", "hence",
                "meanwhile", "moreover", "furthermore", "nevertheless", "nonetheless", "consequently",
                "accordingly", "otherwise", "whereas", "although", "though", "unless", "until",
                "because", "since", "provided", "given", "regarding", "concerning", "including",
                "especially", "particularly", "specifically", "namely", "for example", "for instance",
                "that is", "i.e.", "e.g.", "etc.", "and so on", "and so forth", "as well as", "along with",
                "together with", "in addition to", "apart from", "except for", "besides", "more than",
                "less than", "equal to", "similar to", "different from", "compared to", "in terms of",
                "with respect to", "according to", "based on", "due to", "owing to", "because of",
                "as a result of", "in spite of", "despite", "regardless of", "instead of", "rather than",
                "such as", "like", "for", "against", "pro", "con", "per", "via", "versus", "vs.", "et al."
        ));
        return stopWords.contains(word);
    }

    /**
     * 构建邻接矩阵
     */
    private void buildAdjacencyMatrix(List<Author> allAuthors) {
        int n = allAuthors.size();
        adjacencyMatrix = MatrixUtils.createRealMatrix(n, n);

        // 获取所有合作记录
        List<PaperAuthor> allPaperAuthors = paperAuthorMapper.selectList(null);
        Map<Long, Set<Long>> authorPapersMap = new HashMap<>();

        // 构建作者-论文映射
        for (PaperAuthor pa : allPaperAuthors) {
            authorPapersMap.computeIfAbsent(pa.getAuthorId(), k -> new HashSet<>())
                    .add(pa.getPaperId());
        }

        // 填充邻接矩阵
        for (int i = 0; i < n; i++) {
            Long author1 = indexAuthorMap.get(i);
            Set<Long> papers1 = authorPapersMap.getOrDefault(author1, Collections.emptySet());

            for (int j = i + 1; j < n; j++) {
                Long author2 = indexAuthorMap.get(j);
                Set<Long> papers2 = authorPapersMap.getOrDefault(author2, Collections.emptySet());

                // 计算共同论文数
                long commonPapers = papers1.stream()
                        .filter(papers2::contains)
                        .count();

                if (commonPapers > 0) {
                    // 对称矩阵
                    adjacencyMatrix.setEntry(i, j, commonPapers);
                    adjacencyMatrix.setEntry(j, i, commonPapers);
                }
            }
        }

        // 添加自环
        for (int i = 0; i < n; i++) {
            adjacencyMatrix.addToEntry(i, i, 1.0);
        }

        // 归一化邻接矩阵
        normalizeAdjacencyMatrix();
        System.out.println("Adjacency matrix built and normalized. Size: " + n + "x" + n);
    }

    /**
     * 归一化邻接矩阵
     */
    private void normalizeAdjacencyMatrix() {
        int n = adjacencyMatrix.getRowDimension();
        RealMatrix degreeMatrix = MatrixUtils.createRealMatrix(n, n);

        // 计算度矩阵
        for (int i = 0; i < n; i++) {
            double degree = 0;
            for (int j = 0; j < n; j++) {
                degree += adjacencyMatrix.getEntry(i, j);
            }
            degreeMatrix.setEntry(i, i, 1.0 / Math.sqrt(degree));
        }

        // 归一化: D^{-1/2} A D^{-1/2}
        adjacencyMatrix = degreeMatrix.multiply(adjacencyMatrix).multiply(degreeMatrix);
    }

    /**
     * 构建特征矩阵（词袋模型）
     */
    private RealMatrix buildFeatureMatrix(List<Author> allAuthors) {
        int n = allAuthors.size();
        int d = vocabulary.size();
        RealMatrix featureMatrix = MatrixUtils.createRealMatrix(n, d);

        // 获取所有论文
        List<Paper> allPapers = paperMapper.selectList(null);
        Map<Long, List<Paper>> authorPapersMap = new HashMap<>();

        // 构建作者-论文映射
        for (Paper paper : allPapers) {
            List<PaperAuthor> authors = paperAuthorMapper.selectList(
                    new QueryWrapper<PaperAuthor>().eq("paper_id", paper.getId())
            );

            for (PaperAuthor pa : authors) {
                authorPapersMap.computeIfAbsent(pa.getAuthorId(), k -> new ArrayList<>())
                        .add(paper);
            }
        }

        // 为每个作者构建词袋向量
        for (int i = 0; i < n; i++) {
            Long authorId = indexAuthorMap.get(i);
            List<Paper> papers = authorPapersMap.getOrDefault(authorId, Collections.emptyList());
            double[] vector = new double[d];

            for (Paper paper : papers) {
                if (paper.getTitle() != null) {
                    String[] words = paper.getTitle().toLowerCase()
                            .replaceAll("[^a-z0-9 ]", "")
                            .split("\\s+");

                    for (String word : words) {
                        Integer index = vocabulary.get(word);
                        if (index != null) {
                            vector[index] += 1.0; // TF 计数
                        }
                    }
                }
            }

            // 归一化向量
            double norm = Math.sqrt(Arrays.stream(vector).map(v -> v * v).sum());
            if (norm > 0) {
                for (int j = 0; j < d; j++) {
                    vector[j] /= norm;
                }
            }

            featureMatrix.setRow(i, vector);
        }

        System.out.println("Feature matrix built. Dimensions: " + n + "x" + d);
        return featureMatrix;
    }

    /**
     * 计算两层GCN嵌入
     */
    private void computeGCNEmbeddings(RealMatrix featureMatrix) {
        // 第一层: H1 = ReLU(A * X)
        RealMatrix h1 = adjacencyMatrix.multiply(featureMatrix);
        applyReLU(h1);

        // 第二层: H2 = A * H1
        RealMatrix h2 = adjacencyMatrix.multiply(h1);

        // 归一化嵌入向量
        for (int i = 0; i < h2.getRowDimension(); i++) {
            double[] embedding = h2.getRow(i);
            double norm = Math.sqrt(Arrays.stream(embedding).map(v -> v * v).sum());
            if (norm > 0) {
                for (int j = 0; j < embedding.length; j++) {
                    embedding[j] /= norm;
                }
            }
            authorEmbeddings.put(indexAuthorMap.get(i), embedding);
        }

        System.out.println("GCN embeddings computed. Embedding size: " + h2.getColumnDimension());
    }

    /**
     * 应用ReLU激活函数
     */
    private void applyReLU(RealMatrix matrix) {
        for (int i = 0; i < matrix.getRowDimension(); i++) {
            for (int j = 0; j < matrix.getColumnDimension(); j++) {
                double value = matrix.getEntry(i, j);
                if (value < 0) {
                    matrix.setEntry(i, j, 0);
                }
            }
        }
    }

    /**
     * 计算余弦相似度
     */
    private double cosineSimilarity(double[] v1, double[] v2) {
        if (v1 == null || v2 == null || v1.length != v2.length) {
            return 0.0;
        }

        double dot = 0, norm1 = 0, norm2 = 0;
        for (int i = 0; i < v1.length; i++) {
            dot += v1[i] * v2[i];
            norm1 += v1[i] * v1[i];
            norm2 += v2[i] * v2[i];
        }

        if (norm1 == 0 || norm2 == 0) {
            return 0.0;
        }

        return dot / (Math.sqrt(norm1) * Math.sqrt(norm2));
    }

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
                if (paper != null && paper.getYear() != null) {
                    int currentYear = paper.getYear();
                    int lastYear = coauthorLastYear.getOrDefault(pa.getAuthorId(), 0);
                    if (currentYear > lastYear) {
                        coauthorLastYear.put(pa.getAuthorId(), currentYear);
                    }
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
     * 基于两层GCN嵌入算法，返回详细DTO列表。
     * 保持原始返回数据结构不变
     */
    @Override
    public List<CollaborationPredictResponse> predictCollaborators(Long authorId, List<String> directions, Integer minPapers, Integer startYear, Integer endYear) {
        // 1. 检查嵌入是否已初始化
        if (!embeddingsInitialized) {
            throw new IllegalStateException("Embeddings not initialized yet");
        }

        // 2. 获取目标作者的嵌入向量
        double[] targetEmbedding = authorEmbeddings.get(authorId);
        if (targetEmbedding == null) {
            return Collections.emptyList();
        }

        // 3. 获取该作者的所有论文（用于过滤已合作作者）
        List<PaperAuthor> myPapers = paperAuthorMapper.selectList(
                new QueryWrapper<PaperAuthor>().eq("author_id", authorId)
        );
        List<Long> myPaperIds = myPapers.stream().map(PaperAuthor::getPaperId).collect(Collectors.toList());

        // 4. 获取合作者集合（排除已合作过的作者）
        Set<Long> coauthorIds = new HashSet<>();
        if (!myPaperIds.isEmpty()) {
            QueryWrapper<Paper> paperWrapper = new QueryWrapper<>();
            paperWrapper.in("id", myPaperIds);
            if (startYear != null) paperWrapper.ge("year", startYear);
            if (endYear != null) paperWrapper.le("year", endYear);

            List<Paper> authorPapers = paperMapper.selectList(paperWrapper);

            for (Paper paper : authorPapers) {
                List<PaperAuthor> pas = paperAuthorMapper.selectList(
                        new QueryWrapper<PaperAuthor>().eq("paper_id", paper.getId())
                );
                for (PaperAuthor pa : pas) {
                    if (!pa.getAuthorId().equals(authorId)) {
                        coauthorIds.add(pa.getAuthorId());
                    }
                }
            }
        }

        // 5. 获取所有作者，过滤掉自己和已合作过的
        List<Author> allAuthors = authorMapper.selectList(null);
        List<Author> potentialCoauthors = allAuthors.stream()
                .filter(a -> !a.getId().equals(authorId) && !coauthorIds.contains(a.getId()))
                .collect(Collectors.toList());

        // 6. 计算每个潜在合作者的相似度
        List<CollaborationPredictResponse> result = new ArrayList<>();
        for (Author coauthor : potentialCoauthors) {
            // 获取合作者的嵌入
            double[] coauthorEmbedding = authorEmbeddings.get(coauthor.getId());
            if (coauthorEmbedding == null) continue;

            // 计算嵌入相似度
            double similarity = cosineSimilarity(targetEmbedding, coauthorEmbedding);

            // 获取潜在合作者的论文数量（按年份过滤）
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

            // 应用最小论文数过滤
            int minPaperThreshold = minPapers != null ? minPapers : 1;
            if (coauthorPapers.size() < minPaperThreshold) continue;

            // 计算共同合作者数（保持原始逻辑）
            Set<Long> coCoauthorIds = new HashSet<>();
            for (Paper paper : coauthorPapers) {
                List<PaperAuthor> pas = paperAuthorMapper.selectList(
                        new QueryWrapper<PaperAuthor>().eq("paper_id", paper.getId())
                );
                for (PaperAuthor pa : pas) {
                    if (!pa.getAuthorId().equals(coauthor.getId())) {
                        coCoauthorIds.add(pa.getAuthorId());
                    }
                }
            }
            int commonCoauthors = (int) coCoauthorIds.stream().filter(coauthorIds::contains).count();

            // 计算研究方向匹配度（使用原始方法）
            List<Paper> authorPapers = getAuthorPapers(authorId, startYear, endYear);
            double[] authorVec = getDirectionVector(authorPapers, directions);
            double[] coauthorVec = getDirectionVector(coauthorPapers, directions);
            double cosineScore = cosineSimilarity(authorVec, coauthorVec);
            double jsScore = 1 - Math.min(1, jsDivergence(authorVec, coauthorVec));
            double directionScore = 0.7 * cosineScore + 0.3 * jsScore;

            // 综合评分：嵌入相似度 + 研究方向匹配度
            double finalScore = 0.6 * similarity + 0.4 * directionScore;
            // 添加一些随机性以避免完全相同的分数
            finalScore = Math.max(0.1, Math.min(0.95, finalScore + (Math.random() * 0.05 - 0.025)));

            // 创建响应对象（保持原始数据结构）
            CollaborationPredictResponse resp = new CollaborationPredictResponse();
            resp.setName(coauthor.getName());
            resp.setPaperCount(coauthorPapers.size());
            resp.setCommonCoauthors(commonCoauthors);
            resp.setDirectionScore(String.format("%.1f%%", directionScore * 100));
            resp.setTotalScore(String.format("%.1f%%", finalScore * 100));
            resp.setScoreValue(finalScore);

            // 构建指标列表（保持原始结构）
            List<CollaborationPredictResponse.Indicator> indicators = new ArrayList<>();

            CollaborationPredictResponse.Indicator i1 = new CollaborationPredictResponse.Indicator();
            i1.setName("论文数量");
            i1.setValue(coauthorPapers.size());

            CollaborationPredictResponse.Indicator i2 = new CollaborationPredictResponse.Indicator();
            i2.setName("共同合作者");
            i2.setValue(commonCoauthors);

            CollaborationPredictResponse.Indicator i3 = new CollaborationPredictResponse.Indicator();
            i3.setName("研究方向匹配度");
            i3.setValue(directionScore * 10);

            CollaborationPredictResponse.Indicator i4 = new CollaborationPredictResponse.Indicator();
            i4.setName("嵌入相似度");
            i4.setValue(similarity * 10);

            indicators.add(i1);
            indicators.add(i2);
            indicators.add(i3);
            indicators.add(i4);

            resp.setIndicators(indicators);
            result.add(resp);
        }

        // 按分数排序，取前10
        result.sort((a, b) -> Double.compare(b.getScoreValue(), a.getScoreValue()));
        return result.size() > 10 ? result.subList(0, 10) : result;
    }

    // 辅助方法：获取作者论文
    private List<Paper> getAuthorPapers(Long authorId, Integer startYear, Integer endYear) {
        List<PaperAuthor> myPapers = paperAuthorMapper.selectList(
                new QueryWrapper<PaperAuthor>().eq("author_id", authorId)
        );
        List<Long> myPaperIds = myPapers.stream().map(PaperAuthor::getPaperId).collect(Collectors.toList());
        if (myPaperIds.isEmpty()) return Collections.emptyList();

        QueryWrapper<Paper> paperWrapper = new QueryWrapper<>();
        paperWrapper.in("id", myPaperIds);
        if (startYear != null) paperWrapper.ge("year", startYear);
        if (endYear != null) paperWrapper.le("year", endYear);

        return paperMapper.selectList(paperWrapper);
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
        if (directions == null || directions.isEmpty()) {
            return new double[0];
        }

        double[] vec = new double[directions.size()];
        if (papers == null || papers.isEmpty()) return vec;

        for (Paper paper : papers) {
            String title = paper.getTitle() != null ? paper.getTitle().toLowerCase() : "";
            for (int i = 0; i < directions.size(); i++) {
                String dir = directions.get(i).toLowerCase();
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
