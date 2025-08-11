package com.example.academic_collab_platform_backend.util;

import com.example.academic_collab_platform_backend.mapper.AuthorMapper;
import com.example.academic_collab_platform_backend.mapper.PaperAuthorMapper;
import com.example.academic_collab_platform_backend.mapper.PaperMapper;
import com.example.academic_collab_platform_backend.model.Author;
import com.example.academic_collab_platform_backend.model.Paper;
import com.example.academic_collab_platform_backend.model.PaperAuthor;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.io.InputStream;
import java.util.*;

@Component
public class DataImporter implements CommandLineRunner {

    @Autowired
    private PaperMapper paperMapper;

    @Autowired
    private AuthorMapper authorMapper;

    @Autowired
    private PaperAuthorMapper paperAuthorMapper;

    @Override
    public void run(String... args) throws Exception {
        // 检查是否已有数据
        if (paperMapper.selectCount(null) > 0) {
            System.out.println("数据库中已有数据，跳过导入");
            return;
        }

        System.out.println("开始导入数据...");
        
        try {
            ObjectMapper mapper = new ObjectMapper();
            InputStream is = getClass().getResourceAsStream("/dblp.json");
            
            if (is == null) {
                System.out.println("未找到dblp.json文件，跳过数据导入");
                return;
            }

            // 读取JSON数据
            List<Map<String, Object>> rawData = mapper.readValue(is, new TypeReference<List<Map<String, Object>>>() {});
            
            // 用于存储作者名到ID的映射
            Map<String, Long> authorNameToId = new HashMap<>();
            
            // 预先加载数据库中已存在的作者到内存映射中
            List<Author> existingAuthors = authorMapper.selectList(null);
            for (Author author : existingAuthors) {
                authorNameToId.put(author.getName(), author.getId());
            }
            System.out.println("已加载 " + existingAuthors.size() + " 个现有作者到内存缓存");
            
            for (Map<String, Object> item : rawData) {
                // 创建Paper对象
                Paper paper = new Paper();
                paper.setTitle((String) item.get("title"));
                paper.setAbstractText((String) item.get("abstract"));
                paper.setUrl((String) item.get("url"));
                
                Object yearObj = item.get("year");
                if (yearObj != null) {
                    if (yearObj instanceof Integer) {
                        paper.setYear((Integer) yearObj);
                    } else if (yearObj instanceof String) {
                        try {
                            paper.setYear(Integer.parseInt((String) yearObj));
                        } catch (NumberFormatException e) {
                            paper.setYear(null);
                        }
                    }
                }
                
                // 保存Paper
                paperMapper.insert(paper);
                Long paperId = paper.getId();
                
                // 处理作者
                Object authorsObj = item.get("authors");
                if (authorsObj instanceof List) {
                    List<String> authors = (List<String>) authorsObj;
                    for (String authorName : authors) {
                        if (authorName != null && !authorName.trim().isEmpty()) {
                            // 检查作者是否已存在（先查内存缓存，再查数据库）
                            String trimmedName = authorName.trim();
                            Long authorId = authorNameToId.get(trimmedName);
                            if (authorId == null) {
                                // 先从数据库查询是否已存在该作者
                                Author existingAuthor = authorMapper.selectOne(
                                    new com.baomidou.mybatisplus.core.conditions.query.QueryWrapper<Author>()
                                        .eq("name", trimmedName)
                                );
                                
                                if (existingAuthor != null) {
                                    // 数据库中已存在，使用现有ID
                                    authorId = existingAuthor.getId();
                                    authorNameToId.put(trimmedName, authorId);
                                } else {
                                    // 数据库中不存在，创建新作者
                                    try {
                                        Author author = new Author();
                                        author.setName(trimmedName);
                                        authorMapper.insert(author);
                                        authorId = author.getId();
                                        authorNameToId.put(trimmedName, authorId);
                                    } catch (Exception e) {
                                        // 如果插入失败（可能是并发插入导致重复），再次查询数据库
                                        Author retryAuthor = authorMapper.selectOne(
                                            new com.baomidou.mybatisplus.core.conditions.query.QueryWrapper<Author>()
                                                .eq("name", trimmedName)
                                        );
                                        if (retryAuthor != null) {
                                            authorId = retryAuthor.getId();
                                            authorNameToId.put(trimmedName, authorId);
                                        } else {
                                            throw e; // 如果还是失败，抛出异常
                                        }
                                    }
                                }
                            }
                            
                            // 创建Paper-Author关系（检查是否已存在）
                            PaperAuthor existingRelation = paperAuthorMapper.selectOne(
                                new com.baomidou.mybatisplus.core.conditions.query.QueryWrapper<PaperAuthor>()
                                    .eq("paper_id", paperId)
                                    .eq("author_id", authorId)
                            );
                            
                            if (existingRelation == null) {
                                PaperAuthor paperAuthor = new PaperAuthor();
                                paperAuthor.setPaperId(paperId);
                                paperAuthor.setAuthorId(authorId);
                                try {
                                    paperAuthorMapper.insert(paperAuthor);
                                } catch (Exception e) {
                                    // 忽略重复插入错误，可能是并发导致的
                                    System.out.println("跳过重复的Paper-Author关系: paperId=" + paperId + ", authorId=" + authorId);
                                }
                            }
                        }
                    }
                }
            }
            
            System.out.println("数据导入完成！");
            System.out.println("文献数量: " + paperMapper.selectCount(null));
            System.out.println("作者数量: " + authorMapper.selectCount(null));
            System.out.println("关系数量: " + paperAuthorMapper.selectCount(null));
            
        } catch (Exception e) {
            System.err.println("数据导入失败: " + e.getMessage());
            e.printStackTrace();
        }
    }
} 