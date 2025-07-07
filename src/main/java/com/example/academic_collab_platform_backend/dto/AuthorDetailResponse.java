package com.example.academic_collab_platform_backend.dto;

import com.example.academic_collab_platform_backend.model.Paper;
import java.util.List;
import java.util.Map;

public class AuthorDetailResponse {
    private String author;
    private List<Paper> papers;
    private List<String> relatedAuthors;
    private Map<String, Integer> stats;

    public String getAuthor() { return author; }
    public void setAuthor(String author) { this.author = author; }
    public List<Paper> getPapers() { return papers; }
    public void setPapers(List<Paper> papers) { this.papers = papers; }
    public List<String> getRelatedAuthors() { return relatedAuthors; }
    public void setRelatedAuthors(List<String> relatedAuthors) { this.relatedAuthors = relatedAuthors; }
    public Map<String, Integer> getStats() { return stats; }
    public void setStats(Map<String, Integer> stats) { this.stats = stats; }
    @Override
    public String toString() {
        return "AuthorDetailResponse{" +
                "author='" + author + '\'' +
                ", papers=" + papers +
                ", relatedAuthors=" + relatedAuthors +
                ", stats=" + stats +
                '}';
    }
} 