package com.example.academic_collab_platform_backend.util;

import java.util.HashMap;
import java.util.Map;

public class ResponseUtil {
    public static Map<String, Object> success(Object data) {
        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("data", data);
        return result;
    }

    public static Map<String, Object> successMsg(String msg) {
        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("message", msg);
        return result;
    }

    public static Map<String, Object> error(String msg) {
        Map<String, Object> result = new HashMap<>();
        result.put("success", false);
        result.put("message", msg);
        return result;
    }
} 