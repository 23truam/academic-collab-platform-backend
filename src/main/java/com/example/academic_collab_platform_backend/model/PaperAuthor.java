package com.example.academic_collab_platform_backend.model;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.*;

@Data
@TableName("paper_authors")
public class PaperAuthor {
    public Long getPaperId() {
        return paperId;
    }

    public void setPaperId(Long paperId) {
        this.paperId = paperId;
    }

    public Long getAuthorId() {
        return authorId;
    }

    public void setAuthorId(Long authorId) {
        this.authorId = authorId;
    }

    private Long paperId;
    private Long authorId;
} 