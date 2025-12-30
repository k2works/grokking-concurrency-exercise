package concurrency.ch06;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.BooleanSupplier;

/**
 * ゲームループ - マルチタスキングの例
 * 入力処理、ゲーム計算、描画処理を順番に実行
 */
public class GameLoop {

    /**
     * タスクを表すレコード
     */
    public record Task(String name, Runnable action) implements Runnable {
        @Override
        public void run() {
            action.run();
        }
    }

    /**
     * プロセッサ空き状態を示すイベント
     * threading.Event の Java 版
     */
    public static class ProcessorFreeEvent {
        private final Lock lock = new ReentrantLock();
        private final Condition condition = lock.newCondition();
        private boolean signaled = false;

        /**
         * シグナルを待機
         */
        public void waitForSignal() {
            lock.lock();
            try {
                while (!signaled) {
                    condition.await();
                }
                signaled = false;  // シグナルを消費
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            } finally {
                lock.unlock();
            }
        }

        /**
         * シグナルを送信
         */
        public void signal() {
            lock.lock();
            try {
                signaled = true;
                condition.signal();
            } finally {
                lock.unlock();
            }
        }
    }

    private final Runnable inputTask;
    private final Runnable computeTask;
    private final Runnable renderTask;
    private final BooleanSupplier continueCondition;

    public GameLoop(Runnable inputTask, Runnable computeTask,
                    Runnable renderTask, BooleanSupplier continueCondition) {
        this.inputTask = inputTask;
        this.computeTask = computeTask;
        this.renderTask = renderTask;
        this.continueCondition = continueCondition;
    }

    /**
     * ゲームループを実行
     */
    public void run() {
        while (continueCondition.getAsBoolean()) {
            inputTask.run();
            computeTask.run();
            renderTask.run();
        }
    }

    public static void main(String[] args) {
        System.out.println("Starting game loop demo...");

        GameLoop gameLoop = new GameLoop(
            () -> {
                System.out.println("Getting user input...");
                sleep(100);
            },
            () -> {
                System.out.println("Computing game world...");
                sleep(100);
            },
            () -> {
                System.out.println("Rendering next screen...");
                sleep(100);
            },
            new BooleanSupplier() {
                private int cycles = 0;
                @Override
                public boolean getAsBoolean() {
                    return cycles++ < 5;
                }
            }
        );

        gameLoop.run();
        System.out.println("Game loop done!");
    }

    private static void sleep(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
