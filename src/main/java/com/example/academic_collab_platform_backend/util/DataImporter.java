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
                            // 检查作者是否已存在
                            Long authorId = authorNameToId.get(authorName);
                            if (authorId == null) {
                                // 创建新作者
                                Author author = new Author();
                                author.setName(authorName.trim());
                                authorMapper.insert(author);
                                authorId = author.getId();
                                authorNameToId.put(authorName.trim(), authorId);
                            }
                            
                            // 创建Paper-Author关系
                            PaperAuthor paperAuthor = new PaperAuthor();
                            paperAuthor.setPaperId(paperId);
                            paperAuthor.setAuthorId(authorId);
                            paperAuthorMapper.insert(paperAuthor);
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