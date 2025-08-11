package com.example.academic_collab_platform_backend.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.core.task.AsyncTaskExecutor;

import java.util.concurrent.ThreadPoolExecutor;

/**
 * 异步任务配置类
 * 用于配置搜索历史等异步处理任务
 */

@Configuration
@EnableAsync
public class AsyncConfig {
/**
     * 配置异步任务执行器
     * 专门用于处理搜索历史缓存更新等异步任务
     */

    @Bean("searchHistoryTaskExecutor")
    public AsyncTaskExecutor searchHistoryTaskExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        
        // 核心线程数：10个线程
        executor.setCorePoolSize(10);
        
        // 最大线程数：50个线程
        executor.setMaxPoolSize(50);
        
        // 队列容量：1000个任务
        executor.setQueueCapacity(1000);
        
        // 线程名前缀
        executor.setThreadNamePrefix("search-history-");
        
        // 线程空闲时间：60秒
        executor.setKeepAliveSeconds(60);
        
        // 拒绝策略：当队列满时，由调用线程处理
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        
        // 等待所有任务结束后再关闭线程池
        executor.setWaitForTasksToCompleteOnShutdown(true);
        
        // 等待时间：30秒
        executor.setAwaitTerminationSeconds(30);
        
        executor.initialize();
        return executor;
    }
}
