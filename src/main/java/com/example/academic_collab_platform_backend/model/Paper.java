package com.example.academic_collab_platform_backend.model;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;

import java.util.Arrays;
import java.util.List;

@Data
@TableName("papers")
public class Paper {
    @TableId(type = IdType.AUTO)
    private Long id;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getAuthors() {
        return authors;
    }

    public void setAuthors(String authors) {
        this.authors = authors;
    }

    public String getAbstractText() {
        return abstractText;
    }

    public void setAbstractText(String abstractText) {
        this.abstractText = abstractText;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public Integer getYear() {
        return year;
    }

    public void setYear(Integer year) {
        this.year = year;
    }

    private String title;

    // 数据库中存储的authors字符串
    @JsonIgnore
    private String authors;

    private String abstractText;

    private String url;

    private Integer year;

    // 前端期望的authors数组
    @TableField(exist = false)
    private List<String> authorsList;

    // 获取authors数组
    public List<String> getAuthorsList() {
        if (authorsList == null && authors != null) {
            authorsList = Arrays.asList(authors.split(","));
        }
        return authorsList;
    }

    // 设置authors数组
    public void setAuthorsList(List<String> authorsList) {
        this.authorsList = authorsList;
        if (authorsList != null) {
            this.authors = String.join(",", authorsList);
        }
    }
}

