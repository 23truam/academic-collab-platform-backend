package com.example.academic_collab_platform_backend.service.impl;

import com.example.academic_collab_platform_backend.model.Paper;
import com.example.academic_collab_platform_backend.service.LiteratureService;
import com.example.academic_collab_platform_backend.service.PaperService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 文献检索业务实现类，负责文献的关键词检索等具体业务逻辑。
 */
@Service
public class LiteratureServiceImpl implements LiteratureService {
    @Autowired
    private PaperService paperService;

    /**
     * 根据关键词检索文献。
     */
    @Override
    public List<Paper> searchPapersByKeyword(String keyword) {
        return paperService.searchPapersByKeyword(keyword);
    }
} 