package com.example.academic_collab_platform_backend.util;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.stereotype.Component;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.List;
import java.util.Arrays;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

/**
 * Redis工具类，提供高并发支持
 */
@Component
public class RedisUtil {

    @Autowired
    private RedisTemplate<String, String> redisTemplate;

    private final ObjectMapper objectMapper = new ObjectMapper();

    public RedisUtil() {
        // 注册 Java 8 时间模块
        objectMapper.registerModule(new JavaTimeModule());
        objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        // 🕰️ 统一设置时区为北京时间
        objectMapper.setTimeZone(java.util.TimeZone.getTimeZone("Asia/Shanghai"));
    }

    /**
     * 获取ObjectMapper实例
     * @return ObjectMapper实例
     */
    public ObjectMapper getObjectMapper() {
        return objectMapper;
    }

    // 分布式锁释放脚本
    private static final String UNLOCK_SCRIPT = 
        "if redis.call('get', KEYS[1]) == ARGV[1] then " +
        "return redis.call('del', KEYS[1]) " +
        "else " +
        "return 0 " +
        "end";

    /**
     * 获取分布式锁
     * @param lockKey 锁的key
     * @param lockValue 锁的值（通常是UUID）
     * @param timeout 超时时间
     * @param unit 时间单位
     * @return 是否获取成功
     */
    public boolean tryLock(String lockKey, String lockValue, long timeout, TimeUnit unit) {
        return Boolean.TRUE.equals(redisTemplate.opsForValue().setIfAbsent(lockKey, lockValue, timeout, unit));
    }

    /**
     * 释放分布式锁
     * @param lockKey 锁的key
     * @param lockValue 锁的值
     * @return 是否释放成功
     */
    public boolean releaseLock(String lockKey,String lockValue){
        DefaultRedisScript<Long> script=new DefaultRedisScript<>();
        script.setScriptText(UNLOCK_SCRIPT);
        script.setResultType(Long.class);
        Long result=redisTemplate.execute(script,Arrays.asList(lockKey),lockValue);
        return result!=null && result==1L;
    }

    /**
     * 安全地执行需要分布式锁保护的操作
     * @param lockKey 锁的key
     * @param lockValue 锁的值
     * @param timeout 锁超时时间
     * @param unit 时间单位
     * @param operation 要执行的操作
     * @return 操作结果
     */
    public <T> T executeWithLock(String lockKey, String lockValue, long timeout, TimeUnit unit, Supplier<T> operation) {
        if (tryLock(lockKey, lockValue, timeout, unit)) {
            try {
                return operation.get();
            } finally {
                releaseLock(lockKey, lockValue);
            }
        }
        throw new RuntimeException("无法获取分布式锁: " + lockKey);
    }

    /**
     * 设置缓存，带过期时间
     * @param key 缓存key
     * @param value 缓存值
     * @param timeout 过期时间
     * @param unit 时间单位
     */
    public void set(String key, String value, long timeout, TimeUnit unit) {
        redisTemplate.opsForValue().set(key, value, timeout, unit);
    }

    /**
     * 设置缓存，带过期时间（对象版本）
     * @param key 缓存key
     * @param value 缓存对象
     * @param timeout 过期时间
     * @param unit 时间单位
     */
    public void setObject(String key, Object value, long timeout, TimeUnit unit) {
        try {
            String jsonValue = objectMapper.writeValueAsString(value);
            redisTemplate.opsForValue().set(key, jsonValue, timeout, unit);
        } catch (Exception e) {
            throw new RuntimeException("序列化对象失败", e);
        }
    }

    /**
     * 获取缓存
     * @param key 缓存key
     * @return 缓存值
     */
    public String get(String key) {
        return redisTemplate.opsForValue().get(key);
    }

    /**
     * 获取缓存对象
     * @param key 缓存key
     * @param clazz 对象类型
     * @return 缓存对象
     */
    public <T> T getObject(String key, Class<T> clazz) {
        String value = get(key);
        if (value == null) {
            return null;
        }
        try {
            return objectMapper.readValue(value, clazz);
        } catch (Exception e) {
            throw new RuntimeException("反序列化对象失败", e);
        }
    }

    /**
     * 删除缓存
     * @param key 缓存key
     */
    public void delete(String key) {
        redisTemplate.delete(key);
    }

    /**
     * 批量删除缓存
     * @param keys 缓存key列表
     */
    public void deleteBatch(List<String> keys) {
        redisTemplate.delete(keys);
    }

    /**
     * 检查key是否存在
     * @param key 缓存key
     * @return 是否存在
     */
    public boolean exists(String key) {
        return Boolean.TRUE.equals(redisTemplate.hasKey(key));
    }

    /**
     * 设置过期时间
     * @param key 缓存key
     * @param timeout 过期时间
     * @param unit 时间单位
     * @return 是否设置成功
     */
    public boolean expire(String key, long timeout, TimeUnit unit) {
        return Boolean.TRUE.equals(redisTemplate.expire(key, timeout, unit));
    }

    /**
     * 获取过期时间
     * @param key 缓存key
     * @return 过期时间（秒）
     */
    public long getExpire(String key) {
        Long expire = redisTemplate.getExpire(key);
        return expire != null ? expire : -1;
    }

    /**
     * 原子性递增
     * @param key 缓存key
     * @param delta 增量
     * @return 递增后的值
     */
    public long increment(String key, long delta) {
        return redisTemplate.opsForValue().increment(key, delta);
    }

    /**
     * 原子性递减
     * @param key 缓存key
     * @param delta 减量
     * @return 递减后的值
     */
    public long decrement(String key, long delta) {
        return redisTemplate.opsForValue().decrement(key, delta);
    }

    /**
     * 注册 ObjectMapper 支持 Java 8 时间模块
     */
    public void registerJavaTimeModule() {
        objectMapper.registerModule(new JavaTimeModule());
        objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    }

    /**
     * 向列表左侧添加元素（LPUSH）
     * @param key 列表key
     * @param value 要添加的值
     * @return 列表长度
     */
    public long lpush(String key, String value) {
        return redisTemplate.opsForList().leftPush(key, value);
    }

    /**
     * 从列表右侧弹出元素（RPOP）
     * @param key 列表key
     * @return 弹出的值，如果列表为空返回null
     */
    public String rpop(String key) {
        return redisTemplate.opsForList().rightPop(key);
    }

    /**
     * 获取列表长度
     * @param key 列表key
     * @return 列表长度
     */
    public long llen(String key) {
        Long size = redisTemplate.opsForList().size(key);
        return size != null ? size : 0;
    }

    /**
     * 仅当key不存在时设置值，带过期时间
     * @param key 缓存key
     * @param value 缓存值
     * @param timeout 过期时间
     * @param unit 时间单位
     * @return 是否设置成功（true=首次设置，false=key已存在）
     */
    public Boolean setIfAbsent(String key, String value, long timeout, TimeUnit unit) {
        return redisTemplate.opsForValue().setIfAbsent(key, value, timeout, unit);
    }

    /**
     * 仅当key不存在时设置值（不带过期时间）
     * @param key 缓存key
     * @param value 缓存值
     * @return 是否设置成功（true=首次设置，false=key已存在）
     */
    public Boolean setIfAbsent(String key, String value) {
        return redisTemplate.opsForValue().setIfAbsent(key, value);
    }

}