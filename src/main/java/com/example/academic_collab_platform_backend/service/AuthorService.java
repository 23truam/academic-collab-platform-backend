package com.example.academic_collab_platform_backend.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.example.academic_collab_platform_backend.model.Author;
import com.example.academic_collab_platform_backend.dto.AuthorDetailResponse;

import java.util.List;
import java.util.Map;

/**
 * 作者相关业务接口，定义作者的查询、统计、详情等操作。
 */
public interface AuthorService {
    /**
     * 分页搜索作者
     * @param keyword 关键词
     * @param page 页码
     * @param size 每页数量
     * @return 分页作者列表
     */
    IPage<Author> searchAuthors(String keyword, int page, int size);

    /**
     * 根据论文ID获取作者列表
     * @param paperId 论文ID
     * @return 作者列表
     */
    List<Author> getAuthorsByPaperId(Long paperId);
    
    /**
     * 根据关键词模糊搜索作者（无分页）
     * @param keyword 关键词
     * @return 作者列表
     */
    List<Author> searchAuthorsByKeyword(String keyword);

    /**
     * 根据作者ID获取作者对象
     * @param id 作者ID
     * @return 作者对象
     */
    Author getAuthorById(Long id);

    /**
     * 获取论文数最多的前N位作者
     * @param limit 数量
     * @param year 可选，按年份过滤
     * @return 作者列表
     */
    List<Author> getTopAuthors(int limit, Integer year);

    /**
     * 根据机构分页获取作者
     * @param institution 机构名
     * @param page 页码
     * @param size 每页数量
     * @return 作者列表
     */
    List<Author> getAuthorsByInstitution(String institution, int page, int size);

    /**
     * 获取作者统计信息
     * @return 统计数据
     */
    Map<String, Object> getAuthorStatistics();

    /**
     * 根据作者名获取作者详情（含论文、合作者、统计等）
     * @param authorName 作者名
     * @return 详情数据
     */
    AuthorDetailResponse getAuthorDetailByName(String authorName);
}