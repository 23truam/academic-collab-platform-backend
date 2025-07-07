package com.example.academic_collab_platform_backend.model;

import lombok.Data;

@Data
public class CollaborationResult {
    public Long getAuthor1Id() {
        return author1Id;
    }

    public void setAuthor1Id(Long author1Id) {
        this.author1Id = author1Id;
    }

    public Long getAuthor2Id() {
        return author2Id;
    }

    public void setAuthor2Id(Long author2Id) {
        this.author2Id = author2Id;
    }

    public Integer getLastYear() {
        return lastYear;
    }

    public void setLastYear(Integer lastYear) {
        this.lastYear = lastYear;
    }

    public Integer getPaperCount() {
        return paperCount;
    }

    public void setPaperCount(Integer paperCount) {
        this.paperCount = paperCount;
    }

    public String getAuthor2Name() {
        return author2Name;
    }

    public void setAuthor2Name(String author2Name) {
        this.author2Name = author2Name;
    }

    public String getAuthor1Name() {
        return author1Name;
    }

    public void setAuthor1Name(String author1Name) {
        this.author1Name = author1Name;
    }

    private Long author1Id;
    private Long author2Id;
    private String author1Name;
    private String author2Name;
    private Integer paperCount;
    private Integer lastYear;
} 