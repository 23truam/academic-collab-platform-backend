package com.example.academic_collab_platform_backend.model;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.annotation.TableField;
import lombok.*;

@Data
@TableName("authors")
public class Author {
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @TableId(type = IdType.AUTO)
    private Long id;
    private String name;

    @TableField(exist = false)
    private Integer paperCount;
    public Integer getPaperCount() { return paperCount; }
    public void setPaperCount(Integer paperCount) { this.paperCount = paperCount; }
}