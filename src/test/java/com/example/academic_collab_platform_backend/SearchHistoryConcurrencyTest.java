package com.example.academic_collab_platform_backend;

import com.example.academic_collab_platform_backend.model.SearchHistory;
import com.example.academic_collab_platform_backend.service.SearchHistoryService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * 搜索历史高并发测试类
 */
@SpringBootTest
@ActiveProfiles("test")
public class SearchHistoryConcurrencyTest {

    @Autowired
    private SearchHistoryService searchHistoryService;

    private static final int THREAD_COUNT = 100;
    private static final int OPERATIONS_PER_THREAD = 50;
    private static final int USER_COUNT = 10;

    @Test
    public void testConcurrentSearchHistoryOperations() throws InterruptedException {
        System.out.println("开始高并发测试...");
        
        ExecutorService executor = Executors.newFixedThreadPool(THREAD_COUNT);
        CountDownLatch latch = new CountDownLatch(THREAD_COUNT);
        AtomicInteger successCount = new AtomicInteger(0);
        AtomicInteger failCount = new AtomicInteger(0);
        AtomicLong totalTime = new AtomicLong(0);

        // 创建测试数据
        List<SearchHistory> testData = createTestData();

        long startTime = System.currentTimeMillis();

        // 提交并发任务
        for (int i = 0; i < THREAD_COUNT; i++) {
            final int threadId = i;
            executor.submit(() -> {
                try {
                    for (int j = 0; j < OPERATIONS_PER_THREAD; j++) {
                        long operationStart = System.currentTimeMillis();
                        
                        try {
                            // 随机选择一个用户和搜索历史
                            SearchHistory history = testData.get(threadId % testData.size());
                            String userId = String.valueOf(threadId % USER_COUNT + 1);
                            
                            // 创建新的搜索历史
                            SearchHistory newHistory = new SearchHistory();
                            newHistory.setUserId(userId);
                            newHistory.setKeyword("test_keyword_" + threadId + "_" + j);
                            newHistory.setType("test_type_" + threadId);
                            newHistory.setTimestamp(System.currentTimeMillis());
                            
                            // 执行添加操作
                            searchHistoryService.addSearchHistory(newHistory);
                            
                            // 执行查询操作
                            searchHistoryService.getRecentSearchHistory(userId, 5);
                            
                            successCount.incrementAndGet();
                            totalTime.addAndGet(System.currentTimeMillis() - operationStart);
                            
                        } catch (Exception e) {
                            failCount.incrementAndGet();
                            System.err.println("线程 " + threadId + " 操作失败: " + e.getMessage());
                        }
                    }
                } finally {
                    latch.countDown();
                }
            });
        }

        // 等待所有任务完成
        latch.await();
        long endTime = System.currentTimeMillis();
        
        executor.shutdown();
        executor.awaitTermination(10, TimeUnit.SECONDS);

        // 输出测试结果
        printTestResults(startTime, endTime, successCount.get(), failCount.get(), totalTime.get());
    }

    @Test
    public void testConcurrentReadOperations() throws InterruptedException {
        System.out.println("开始并发读取测试...");
        
        ExecutorService executor = Executors.newFixedThreadPool(THREAD_COUNT);
        CountDownLatch latch = new CountDownLatch(THREAD_COUNT);
        AtomicInteger successCount = new AtomicInteger(0);
        AtomicInteger failCount = new AtomicInteger(0);

        long startTime = System.currentTimeMillis();

        // 提交并发读取任务
        for (int i = 0; i < THREAD_COUNT; i++) {
            final int threadId = i;
            executor.submit(() -> {
                try {
                    for (int j = 0; j < OPERATIONS_PER_THREAD; j++) {
                        try {
                            String userId = String.valueOf(threadId % USER_COUNT + 1);
                            searchHistoryService.getRecentSearchHistory(userId, 5);
                            successCount.incrementAndGet();
                        } catch (Exception e) {
                            failCount.incrementAndGet();
                            System.err.println("读取操作失败: " + e.getMessage());
                        }
                    }
                } finally {
                    latch.countDown();
                }
            });
        }

        latch.await();
        long endTime = System.currentTimeMillis();
        
        executor.shutdown();
        executor.awaitTermination(10, TimeUnit.SECONDS);

        printTestResults(startTime, endTime, successCount.get(), failCount.get(), 0);
    }

    @Test
    public void testConcurrentWriteOperations() throws InterruptedException {
        System.out.println("开始并发写入测试...");
        
        ExecutorService executor = Executors.newFixedThreadPool(THREAD_COUNT);
        CountDownLatch latch = new CountDownLatch(THREAD_COUNT);
        AtomicInteger successCount = new AtomicInteger(0);
        AtomicInteger failCount = new AtomicInteger(0);

        long startTime = System.currentTimeMillis();

        // 提交并发写入任务
        for (int i = 0; i < THREAD_COUNT; i++) {
            final int threadId = i;
            executor.submit(() -> {
                try {
                    for (int j = 0; j < OPERATIONS_PER_THREAD; j++) {
                        try {
                            String userId = String.valueOf(threadId % USER_COUNT + 1);
                            
                            SearchHistory history = new SearchHistory();
                            history.setUserId(userId);
                            history.setKeyword("concurrent_keyword_" + threadId + "_" + j);
                            history.setType("concurrent_type");
                            history.setTimestamp(System.currentTimeMillis());
                            
                            searchHistoryService.addSearchHistory(history);
                            successCount.incrementAndGet();
                        } catch (Exception e) {
                            failCount.incrementAndGet();
                            System.err.println("写入操作失败: " + e.getMessage());
                        }
                    }
                } finally {
                    latch.countDown();
                }
            });
        }

        latch.await();
        long endTime = System.currentTimeMillis();
        
        executor.shutdown();
        executor.awaitTermination(10, TimeUnit.SECONDS);

        printTestResults(startTime, endTime, successCount.get(), failCount.get(), 0);
    }

    private List<SearchHistory> createTestData() {
        List<SearchHistory> testData = new ArrayList<>();
        for (int i = 0; i < USER_COUNT; i++) {
            SearchHistory history = new SearchHistory();
            history.setUserId(String.valueOf(i + 1));
            history.setKeyword("test_keyword_" + i);
            history.setType("test_type");
            history.setTimestamp(System.currentTimeMillis());
            testData.add(history);
        }
        return testData;
    }

    private void printTestResults(long startTime, long endTime, int successCount, int failCount, long totalTime) {
        long duration = endTime - startTime;
        int totalOperations = successCount + failCount;
        double successRate = totalOperations > 0 ? (double) successCount / totalOperations * 100 : 0;
        double avgOperationTime = successCount > 0 ? (double) totalTime / successCount : 0;
        double operationsPerSecond = duration > 0 ? (double) totalOperations / duration * 1000 : 0;

        System.out.println("=== 高并发测试结果 ===");
        System.out.println("总执行时间: " + duration + "ms");
        System.out.println("总操作数: " + totalOperations);
        System.out.println("成功操作数: " + successCount);
        System.out.println("失败操作数: " + failCount);
        System.out.println("成功率: " + String.format("%.2f", successRate) + "%");
        System.out.println("平均操作时间: " + String.format("%.2f", avgOperationTime) + "ms");
        System.out.println("每秒操作数: " + String.format("%.2f", operationsPerSecond));
        System.out.println("=====================");
    }
} 