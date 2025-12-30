package concurrency.ch06;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Timeout;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.*;

/**
 * ゲームループのテスト
 */
class GameLoopTest {

    @Test
    @DisplayName("タスクを作成できる")
    void shouldCreateTask() {
        AtomicInteger counter = new AtomicInteger(0);

        GameLoop.Task task = new GameLoop.Task("TestTask", counter::incrementAndGet);

        assertThat(task.name()).isEqualTo("TestTask");
    }

    @Test
    @DisplayName("タスクを実行できる")
    @Timeout(5)
    void shouldExecuteTask() throws InterruptedException {
        AtomicInteger counter = new AtomicInteger(0);
        CountDownLatch latch = new CountDownLatch(1);

        GameLoop.Task task = new GameLoop.Task("TestTask", () -> {
            counter.incrementAndGet();
            latch.countDown();
        });

        Thread thread = new Thread(task);
        thread.start();
        latch.await();

        assertThat(counter.get()).isEqualTo(1);
    }

    @Test
    @DisplayName("イベントを使ってタスクを調整できる")
    @Timeout(5)
    void shouldCoordinateTasksWithEvent() throws InterruptedException {
        List<String> executionOrder = new CopyOnWriteArrayList<>();
        GameLoop.ProcessorFreeEvent event = new GameLoop.ProcessorFreeEvent();
        CountDownLatch allDone = new CountDownLatch(3);

        // 3つのタスクを作成
        Thread t1 = new Thread(() -> {
            event.waitForSignal();
            executionOrder.add("task1");
            event.signal();
            allDone.countDown();
        });

        Thread t2 = new Thread(() -> {
            event.waitForSignal();
            executionOrder.add("task2");
            event.signal();
            allDone.countDown();
        });

        Thread t3 = new Thread(() -> {
            event.waitForSignal();
            executionOrder.add("task3");
            event.signal();
            allDone.countDown();
        });

        t1.start();
        t2.start();
        t3.start();

        // 最初のシグナルを送信
        Thread.sleep(50);  // スレッドが待機状態になるのを待つ
        event.signal();

        allDone.await();

        assertThat(executionOrder).hasSize(3);
    }

    @Test
    @DisplayName("ゲームループが複数タスクを順番に実行できる")
    @Timeout(10)
    void shouldRunGameLoopWithMultipleTasks() throws InterruptedException {
        List<String> results = new CopyOnWriteArrayList<>();
        AtomicInteger cycleCount = new AtomicInteger(0);
        int maxCycles = 3;

        GameLoop gameLoop = new GameLoop(
            () -> results.add("input"),
            () -> results.add("compute"),
            () -> results.add("render"),
            () -> cycleCount.incrementAndGet() <= maxCycles
        );

        gameLoop.run();

        // 3サイクル × 3タスク = 9回
        assertThat(results).hasSize(9);
        assertThat(results.subList(0, 3)).containsExactly("input", "compute", "render");
    }
}
