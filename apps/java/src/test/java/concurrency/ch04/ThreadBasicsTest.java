package concurrency.ch04;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Timeout;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.Assertions.*;

/**
 * スレッド基本操作のテスト
 */
class ThreadBasicsTest {

    @Test
    @DisplayName("スレッドを作成して実行できる")
    @Timeout(5)
    void shouldCreateAndRunThread() throws InterruptedException {
        List<String> results = new CopyOnWriteArrayList<>();
        CountDownLatch latch = new CountDownLatch(1);

        Thread thread = ThreadBasics.createWorkerThread(() -> {
            results.add("task executed");
            latch.countDown();
        });

        thread.start();
        latch.await();

        assertThat(results).containsExactly("task executed");
    }

    @Test
    @DisplayName("複数のスレッドを並行実行できる")
    @Timeout(5)
    void shouldRunMultipleThreadsConcurrently() throws InterruptedException {
        List<Integer> results = new CopyOnWriteArrayList<>();
        int threadCount = 5;
        CountDownLatch latch = new CountDownLatch(threadCount);

        List<Thread> threads = ThreadBasics.createMultipleWorkers(threadCount, i -> {
            results.add(i);
            latch.countDown();
        });

        for (Thread thread : threads) {
            thread.start();
        }

        latch.await();

        assertThat(results).hasSize(threadCount);
        assertThat(results).containsExactlyInAnyOrder(0, 1, 2, 3, 4);
    }

    @Test
    @DisplayName("スレッド名を設定できる")
    void shouldSetThreadName() {
        Thread thread = ThreadBasics.createNamedThread("TestWorker", () -> {});

        assertThat(thread.getName()).isEqualTo("TestWorker");
    }

    @Test
    @DisplayName("joinでスレッドの完了を待機できる")
    @Timeout(5)
    void shouldWaitForThreadCompletion() throws InterruptedException {
        List<String> results = new CopyOnWriteArrayList<>();

        Thread thread = ThreadBasics.createWorkerThread(() -> {
            try {
                Thread.sleep(100);
                results.add("completed");
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        thread.start();
        assertThat(results).isEmpty();

        thread.join();
        assertThat(results).containsExactly("completed");
    }

    @Test
    @DisplayName("現在のスレッド数を取得できる")
    void shouldGetActiveThreadCount() {
        int count = ThreadBasics.getActiveThreadCount();

        assertThat(count).isGreaterThanOrEqualTo(1);
    }
}
