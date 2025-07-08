package com.example.academic_collab_platform_backend.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.example.academic_collab_platform_backend.mapper.AuthorMapper;
import com.example.academic_collab_platform_backend.mapper.PaperAuthorMapper;
import com.example.academic_collab_platform_backend.model.Author;
import com.example.academic_collab_platform_backend.model.PaperAuthor;
import com.example.academic_collab_platform_backend.service.AuthorService;
import com.example.academic_collab_platform_backend.util.RedisUtil;
import org.hibernate.validator.internal.constraintvalidators.bv.size.SizeValidatorForArraysOfLong;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.cache.CacheProperties;
import org.springframework.stereotype.Service;
import com.example.academic_collab_platform_backend.dto.AuthorDetailResponse;
import com.example.academic_collab_platform_backend.model.Paper;
import com.example.academic_collab_platform_backend.mapper.PaperMapper;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.ArrayList;
import java.util.concurrent.TimeUnit;

/**
 * 作者相关业务实现类，负责作者的查询、统计、详情等具体业务逻辑。
 */
@Service
public class AuthorServiceImpl implements AuthorService {

    @Autowired
    private AuthorMapper authorMapper;

    @Autowired
    private PaperAuthorMapper paperAuthorMapper;

    @Autowired
    private PaperMapper paperMapper;

    @Autowired
    private RedisUtil redisUtil;

    // Redis缓存相关常量
    private static final String ALL_AUTHORS_CACHE_KEY = "all_authors";
    private static final String AUTHOR_BY_ID_CACHE_KEY_PREFIX = "author:";
    private static final String AUTHOR_BY_NAME_CACHE_KEY_PREFIX = "author_name:";
    private static final long CACHE_EXPIRE_TIME = 24; // 缓存过期时间（小时）
    private SizeValidatorForArraysOfLong sizeValidatorForArraysOfLong;

    /**
     * 从Redis缓存获取所有作者
     */
    private List<Author> getAllAuthorsFromCache() {
        try {
            String jsonValue = redisUtil.get(ALL_AUTHORS_CACHE_KEY);
            if (jsonValue == null) {
                return null;
            }
            return redisUtil.getObjectMapper().readValue(jsonValue, 
                redisUtil.getObjectMapper().getTypeFactory().constructCollectionType(List.class, Author.class));
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * 将所有作者加载到Redis缓存
     */
    private void loadAllAuthorsToCache() {
        List<Author> allAuthors=authorMapper.selectList(null);
        if(allAuthors!=null && !allAuthors.isEmpty()){
            redisUtil.setObject(ALL_AUTHORS_CACHE_KEY,allAuthors,CACHE_EXPIRE_TIME,TimeUnit.HOURS);
            for(Author author:allAuthors){
                String authorByIdKey=AUTHOR_BY_ID_CACHE_KEY_PREFIX+author.getId();
                String authorByNameKey=AUTHOR_BY_NAME_CACHE_KEY_PREFIX+author.getName();
                redisUtil.setObject(authorByIdKey,author,CACHE_EXPIRE_TIME,TimeUnit.HOURS);
                redisUtil.setObject(authorByNameKey,author,CACHE_EXPIRE_TIME,TimeUnit.HOURS);
            }
        }
    }

    /**
     * 获取所有作者（优先从缓存获取）
     */
    private List<Author> getAllAuthors(){
        List<Author> cacheAuthor=getAllAuthorsFromCache();
        if(cacheAuthor!=null && !cacheAuthor.isEmpty()){
            return cacheAuthor;
        }
        loadAllAuthorsToCache();
        return getAllAuthorsFromCache();
    }


    /**
     * 分页搜索作者，根据关键词模糊匹配作者名。
     */
    @Override
    public IPage<Author> searchAuthors(String keyword, int page, int size) {
        // 从缓存获取所有作者
        List<Author> allAuthors = getAllAuthors();
        
        // 如果有关键词，进行过滤
        List<Author> filteredAuthors = allAuthors;
        if (keyword != null && !keyword.isEmpty()) {
            filteredAuthors = allAuthors.stream()
                .filter(author -> author.getName() != null && author.getName().toLowerCase().contains(keyword.toLowerCase()))
                .collect(Collectors.toList());
        }
        
        // 手动实现分页
        int start = (page - 1) * size;
        int end = Math.min(start + size, filteredAuthors.size());
        List<Author> pageAuthors = start < filteredAuthors.size() ? 
            filteredAuthors.subList(start, end) : new ArrayList<>();
        
        // 创建分页结果
        Page<Author> resultPage = new Page<>(page, size);
        resultPage.setRecords(pageAuthors);
        resultPage.setTotal(filteredAuthors.size());
        resultPage.setCurrent(page);
        resultPage.setSize(size);
        
        return resultPage;
    }


    /**
     * 根据论文ID获取作者列表。
     */
    @Override
    public List<Author> getAuthorsByPaperId(Long paperId) {
        List<PaperAuthor> relations = paperAuthorMapper.selectList(
                new QueryWrapper<PaperAuthor>().eq("paper_id", paperId)
        );
        List<Long> authorIds = relations.stream().map(PaperAuthor::getAuthorId).collect(Collectors.toList());
        
        if (authorIds.isEmpty()) {
            return Collections.emptyList();
        }
        
        // 从缓存获取作者信息
        List<Author> allAuthors = getAllAuthors();
        return allAuthors.stream()
            .filter(author -> authorIds.contains(author.getId()))
            .collect(Collectors.toList());
    }

    /**
     * 根据关键词模糊搜索作者（无分页）。
     */
    @Override
    public List<Author> searchAuthorsByKeyword(String keyword) {
        // 从缓存获取所有作者
        List<Author> allAuthors = getAllAuthors();
        
        if (keyword == null || keyword.isEmpty()) {
            return allAuthors;
        }
        
        return allAuthors.stream()
            .filter(author -> author.getName() != null && author.getName().toLowerCase().contains(keyword.toLowerCase()))
            .collect(Collectors.toList());
    }

    /**
     * 根据作者ID获取作者对象。
     */
    @Override
    public Author getAuthorById(Long id) {
        // 首先尝试从缓存获取
        String cacheKey = AUTHOR_BY_ID_CACHE_KEY_PREFIX + id;
        Author cachedAuthor = redisUtil.getObject(cacheKey, Author.class);
        if (cachedAuthor != null) {
            return cachedAuthor;
        }
        
        // 缓存中没有，从数据库获取
        Author author = authorMapper.selectById(id);
        if (author != null) {
            redisUtil.setObject(cacheKey, author, CACHE_EXPIRE_TIME, TimeUnit.HOURS);
        }
        return author;
    }

    /**
     * 获取论文数最多的前N位作者，可选按年份过滤。
     */
    public List<Author> getTopAuthors(int limit, Integer year) {
        // 从缓存获取所有作者
        List<Author> allAuthors = getAllAuthors();
        Map<Long, Integer> authorPaperCount = new HashMap<>();
        
        for (Author author : allAuthors) {
            int count = paperAuthorMapper.selectCount(new QueryWrapper<PaperAuthor>().eq("author_id", author.getId())).intValue();
            authorPaperCount.put(author.getId(), count);
            author.setPaperCount(count);
        }
        
        List<Author> sortedAuthors = allAuthors.stream()
            .sorted((a, b) -> authorPaperCount.getOrDefault(b.getId(), 0) - authorPaperCount.getOrDefault(a.getId(), 0))
            .limit(limit)
            .collect(Collectors.toList());
        return sortedAuthors;
    }

    /**
     * 根据机构分页获取作者。
     */
    @Override
    public List<Author> getAuthorsByInstitution(String institution, int page, int size) {
        // 从缓存获取所有作者
        List<Author> allAuthors = getAllAuthors();
        
        // 手动实现分页
        int start = (page - 1) * size;
        int end = Math.min(start + size, allAuthors.size());
        return start < allAuthors.size() ? allAuthors.subList(start, end) : new ArrayList<>();
    }

    /**
     * 获取作者统计信息。
     */
    @Override
    public Map<String, Object> getAuthorStatistics() {
        Map<String, Object> stats = new HashMap<>();
        List<Author> allAuthors = getAllAuthors();
        stats.put("totalAuthors", allAuthors.size());
        stats.put("message", "作者统计信息");
        return stats;
    }

    /**
     * 根据作者名获取作者详情（含论文、合作者、统计等）。
     */
    @Override
    public AuthorDetailResponse getAuthorDetailByName(String authorName) {
        AuthorDetailResponse resp = new AuthorDetailResponse();
        resp.setAuthor(authorName);
        
        // 首先尝试从缓存获取作者
        String cacheKey = AUTHOR_BY_NAME_CACHE_KEY_PREFIX + authorName;
        Author author = redisUtil.getObject(cacheKey, Author.class);
        
        if (author == null) {
            // 缓存中没有，从数据库获取
            author = authorMapper.selectOne(new QueryWrapper<Author>().eq("name", authorName));
            if (author != null) {
                redisUtil.setObject(cacheKey, author, CACHE_EXPIRE_TIME, TimeUnit.HOURS);
            }
        }
        
        if (author == null) return null;
        
        // 保存author的ID为final变量，以便在lambda中使用
        final Long authorId = author.getId();
        
        List<Long> authorIds = Collections.singletonList(authorId);
        // 查找该作者的所有论文
        List<PaperAuthor> relations = paperAuthorMapper.selectList(
            new QueryWrapper<PaperAuthor>().in("author_id", authorIds)
        );
        List<Long> paperIds = relations.stream().map(PaperAuthor::getPaperId).distinct().collect(Collectors.toList());
        List<Paper> papers = paperIds.isEmpty() ? Collections.emptyList() : paperMapper.selectBatchIds(paperIds);

        // 查询所有涉及到的作者
        List<PaperAuthor> allPaperAuthors = paperIds.isEmpty() ? Collections.emptyList() :
            paperAuthorMapper.selectList(new QueryWrapper<PaperAuthor>().in("paper_id", paperIds));
        Map<Long, List<Long>> paperIdToAuthorIds = allPaperAuthors.stream()
            .collect(Collectors.groupingBy(PaperAuthor::getPaperId,
                Collectors.mapping(PaperAuthor::getAuthorId, Collectors.toList())));
        
        // 从缓存获取作者信息
        List<Author> allAuthors = getAllAuthors();
        Map<Long, String> authorIdToName = allAuthors.stream()
            .collect(Collectors.toMap(Author::getId, Author::getName));

        // 给每篇论文加上 authorsList 字段
        for (Paper paper : papers) {
            List<Long> pauthorIds = paperIdToAuthorIds.getOrDefault(paper.getId(), new ArrayList<>());
            List<String> authorNames = pauthorIds.stream().map(authorIdToName::get).collect(Collectors.toList());
            paper.setAuthorsList(authorNames);
        }
        resp.setPapers(papers);

        // 相关作者（去除自己）
        List<Long> relatedAuthorIds = allPaperAuthors.stream()
            .map(PaperAuthor::getAuthorId)
            .filter(id -> !id.equals(authorId))
            .distinct()
            .collect(Collectors.toList());
        List<Author> relatedAuthors = allAuthors.stream()
            .filter(a -> relatedAuthorIds.contains(a.getId()))
            .collect(Collectors.toList());
        List<String> relatedAuthorNames = relatedAuthors.stream().map(Author::getName).collect(Collectors.toList());
        resp.setRelatedAuthors(relatedAuthorNames);

        // 统计数据
        Map<String, Integer> stats = new HashMap<>();
        for (Paper paper : papers) {
            String year = String.valueOf(paper.getYear());
            stats.put(year, stats.getOrDefault(year, 0) + 1);
        }
        resp.setStats(stats);
        return resp;
    }

    /**
     * 清除作者相关缓存
     */
    public void clearAuthorCache() {
        redisUtil.delete(ALL_AUTHORS_CACHE_KEY);
        // 这里可以添加清除其他相关缓存的逻辑
    }
} 