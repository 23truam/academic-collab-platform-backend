package com.example.academic_collab_platform_backend.dto;

import java.util.List;

public class CollaborationPredictRequest {
    private Long authorId;
    private List<String> directions;
    private Integer minPapers;
    private Integer startYear;
    private Integer endYear;

    public Long getAuthorId() {
        return authorId;
    }

    public void setAuthorId(Long authorId) {
        this.authorId = authorId;
    }

    public List<String> getDirections() {
        return directions;
    }

    public void setDirections(List<String> directions) {
        this.directions = directions;
    }

    public Integer getMinPapers() {
        return minPapers;
    }

    public void setMinPapers(Integer minPapers) {
        this.minPapers = minPapers;
    }

    public Integer getStartYear() {
        return startYear;
    }

    public void setStartYear(Integer startYear) {
        this.startYear = startYear;
    }

    public Integer getEndYear() {
        return endYear;
    }

    public void setEndYear(Integer endYear) {
        this.endYear = endYear;
    }
} 