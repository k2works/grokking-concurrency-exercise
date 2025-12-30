package concurrency.ch04;

import java.util.ArrayList;
import java.util.List;
import java.util.function.IntConsumer;

/**
 * スレッドの基本操作
 */
public class ThreadBasics {

    /**
     * ワーカースレッドを作成
     *
     * @param task 実行するタスク
     * @return 作成されたスレッド
     */
    public static Thread createWorkerThread(Runnable task) {
        return new Thread(task);
    }

    /**
     * 複数のワーカースレッドを作成
     *
     * @param count 作成するスレッド数
     * @param task  各スレッドで実行するタスク（インデックスを受け取る）
     * @return 作成されたスレッドのリスト
     */
    public static List<Thread> createMultipleWorkers(int count, IntConsumer task) {
        List<Thread> threads = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            final int index = i;
            Thread thread = new Thread(() -> task.accept(index), "Worker-" + i);
            threads.add(thread);
        }
        return threads;
    }

    /**
     * 名前付きスレッドを作成
     *
     * @param name スレッド名
     * @param task 実行するタスク
     * @return 作成されたスレッド
     */
    public static Thread createNamedThread(String name, Runnable task) {
        return new Thread(task, name);
    }

    /**
     * アクティブスレッド数を取得
     *
     * @return アクティブなスレッド数
     */
    public static int getActiveThreadCount() {
        return Thread.activeCount();
    }

    public static void main(String[] args) throws InterruptedException {
        System.out.println("Starting thread demo...");
        System.out.println("Current process: " + ProcessHandle.current().pid());
        System.out.println("Thread count: " + getActiveThreadCount());

        int numThreads = 5;
        System.out.println("Starting " + numThreads + " workers...");

        List<Thread> threads = createMultipleWorkers(numThreads, i -> {
            String name = Thread.currentThread().getName();
            System.out.println(name + " doing " + i + " work");
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        for (Thread thread : threads) {
            thread.start();
        }

        System.out.println("Thread count: " + getActiveThreadCount());

        for (Thread thread : threads) {
            thread.join();
        }

        System.out.println("All workers done!");
    }
}
