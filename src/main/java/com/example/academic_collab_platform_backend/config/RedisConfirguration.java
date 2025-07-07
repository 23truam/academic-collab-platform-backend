package com.example.academic_collab_platform_backend.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.data.redis.serializer.GenericJackson2JsonRedisSerializer;
import org.springframework.data.redis.connection.lettuce.LettuceClientConfiguration;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import io.lettuce.core.ClientOptions;
import io.lettuce.core.ReadFrom;
import io.lettuce.core.cluster.ClusterClientOptions;
import java.time.Duration;

/**
 * Redis配置类，优化高并发性能
 */
@Configuration
@Slf4j
public class RedisConfirguration {

    @Bean
    public RedisTemplate<String, String> redisTemplate(RedisConnectionFactory redisConnectionFactory) {
        log.info("开始创建redis模板对象...");
        RedisTemplate<String, String> redisTemplate = new RedisTemplate<>();
        
        // 设置redis的连接工厂对象
        redisTemplate.setConnectionFactory(redisConnectionFactory);
        
        // 设置key的序列化器 - 使用String序列化器提高性能
        StringRedisSerializer stringRedisSerializer = new StringRedisSerializer();
        redisTemplate.setKeySerializer(stringRedisSerializer);
        redisTemplate.setHashKeySerializer(stringRedisSerializer);
        
        // 设置value的序列化器 - 使用String序列化器提高性能
        redisTemplate.setValueSerializer(stringRedisSerializer);
        redisTemplate.setHashValueSerializer(stringRedisSerializer);
        
        // 初始化RedisTemplate
        redisTemplate.afterPropertiesSet();
        
        log.info("Redis模板对象创建完成");
        return redisTemplate;
    }

    /**
     * 配置Lettuce连接工厂，优化高并发性能
     */
    @Bean
    public LettuceConnectionFactory lettuceConnectionFactory() {
        // Redis连接配置
        RedisStandaloneConfiguration redisConfig = new RedisStandaloneConfiguration();
        redisConfig.setHostName("localhost"); // 根据实际配置修改
        redisConfig.setPort(6379);
        redisConfig.setDatabase(1);
         redisConfig.setPassword("zxh68080656"); // 如果有密码，取消注释
        
        // Lettuce客户端配置，优化高并发
        LettuceClientConfiguration clientConfig = LettuceClientConfiguration.builder()
            .clientOptions(ClientOptions.builder()
                .timeoutOptions(io.lettuce.core.TimeoutOptions.enabled(Duration.ofSeconds(5)))
                .build())
            .commandTimeout(Duration.ofSeconds(5))
            .shutdownTimeout(Duration.ofSeconds(5))
            .build();
        
        return new LettuceConnectionFactory(redisConfig, clientConfig);
    }

    /**
     * 配置RedisTemplate用于对象序列化（备用）
     */
    @Bean
    public RedisTemplate<String, Object> objectRedisTemplate(RedisConnectionFactory redisConnectionFactory) {
        RedisTemplate<String, Object> template = new RedisTemplate<>();
        template.setConnectionFactory(redisConnectionFactory);
        
        // 使用Jackson2JsonRedisSerializer进行对象序列化
        GenericJackson2JsonRedisSerializer jsonSerializer = new GenericJackson2JsonRedisSerializer();
        StringRedisSerializer stringSerializer = new StringRedisSerializer();
        
        template.setKeySerializer(stringSerializer);
        template.setHashKeySerializer(stringSerializer);
        template.setValueSerializer(jsonSerializer);
        template.setHashValueSerializer(jsonSerializer);
        
        template.afterPropertiesSet();
        return template;
    }
}
