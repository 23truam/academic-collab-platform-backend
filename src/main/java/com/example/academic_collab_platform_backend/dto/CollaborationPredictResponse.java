package com.example.academic_collab_platform_backend.dto;

import java.util.List;

public class CollaborationPredictResponse {
    private String name;
    private int paperCount;
    private int commonCoauthors;
    private String directionScore;
    private String totalScore;
    private double scoreValue;
    private List<Indicator> indicators;

    public static class Indicator {
        private String name;
        private double value;

        public String getName() { return name; }
        public void setName(String name) { this.name = name; }
        public double getValue() { return value; }
        public void setValue(double value) { this.value = value; }
    }

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    public int getPaperCount() { return paperCount; }
    public void setPaperCount(int paperCount) { this.paperCount = paperCount; }
    public int getCommonCoauthors() { return commonCoauthors; }
    public void setCommonCoauthors(int commonCoauthors) { this.commonCoauthors = commonCoauthors; }
    public String getDirectionScore() { return directionScore; }
    public void setDirectionScore(String directionScore) { this.directionScore = directionScore; }
    public String getTotalScore() { return totalScore; }
    public void setTotalScore(String totalScore) { this.totalScore = totalScore; }
    public double getScoreValue() { return scoreValue; }
    public void setScoreValue(double scoreValue) { this.scoreValue = scoreValue; }
    public List<Indicator> getIndicators() { return indicators; }
    public void setIndicators(List<Indicator> indicators) { this.indicators = indicators; }
} 