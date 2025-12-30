package concurrency.ch07;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

/**
 * パイプラインパターン
 * 処理をステージに分割し、各ステージを並列に実行
 */
public class Pipeline<T> {

    /**
     * パイプラインのステージ
     */
    public record Stage<I, O>(String name, Function<I, O> processor) {}

    private final List<StageWorker> workers = new ArrayList<>();
    private final List<BlockingQueue<Object>> queues = new ArrayList<>();
    private volatile boolean running = false;

    /**
     * ステージを追加
     */
    @SuppressWarnings("unchecked")
    public <I, O> void addStage(String name, Function<I, O> processor) {
        BlockingQueue<Object> inputQueue;
        if (queues.isEmpty()) {
            inputQueue = new LinkedBlockingQueue<>();
            queues.add(inputQueue);
        } else {
            inputQueue = queues.get(queues.size() - 1);
        }

        BlockingQueue<Object> outputQueue = new LinkedBlockingQueue<>();
        queues.add(outputQueue);

        StageWorker worker = new StageWorker(name, inputQueue, outputQueue, (Function<Object, Object>) processor);
        workers.add(worker);
    }

    /**
     * パイプラインを開始
     */
    public void start() {
        running = true;
        for (StageWorker worker : workers) {
            worker.start();
        }
    }

    /**
     * データを送信
     */
    public void submit(T item) {
        if (!queues.isEmpty()) {
            queues.get(0).offer(item);
        }
    }

    /**
     * パイプラインを停止
     */
    public void stop() {
        running = false;
        for (StageWorker worker : workers) {
            worker.interrupt();
        }
    }

    /**
     * ステージワーカー
     */
    private class StageWorker extends Thread {
        private final BlockingQueue<Object> inputQueue;
        private final BlockingQueue<Object> outputQueue;
        private final Function<Object, Object> processor;

        StageWorker(String name, BlockingQueue<Object> inputQueue,
                    BlockingQueue<Object> outputQueue, Function<Object, Object> processor) {
            super(name);
            this.inputQueue = inputQueue;
            this.outputQueue = outputQueue;
            this.processor = processor;
            setDaemon(true);
        }

        @Override
        public void run() {
            while (running || !inputQueue.isEmpty()) {
                try {
                    Object item = inputQueue.poll(100, TimeUnit.MILLISECONDS);
                    if (item != null) {
                        System.out.println(getName() + ": processing " + item);
                        Object result = processor.apply(item);
                        outputQueue.offer(result);
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }
    }

    public static void main(String[] args) throws InterruptedException {
        System.out.println("Starting laundry pipeline...");

        Pipeline<String> pipeline = new Pipeline<>();
        pipeline.addStage("Washer", item -> {
            sleep(200);
            return "washed:" + item;
        });
        pipeline.addStage("Dryer", item -> {
            sleep(100);
            return "dried:" + item;
        });
        pipeline.addStage("Folder", item -> {
            sleep(50);
            System.out.println("Done: " + item);
            return "folded:" + item;
        });

        pipeline.start();

        for (int i = 1; i <= 4; i++) {
            pipeline.submit("Washload #" + i);
        }

        Thread.sleep(2000);
        pipeline.stop();
        System.out.println("All done!");
    }

    private static void sleep(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
