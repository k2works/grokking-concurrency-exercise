package concurrency.ch07;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Timeout;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;

import static org.assertj.core.api.Assertions.*;

/**
 * パイプラインパターンのテスト
 */
class PipelineTest {

    @Test
    @DisplayName("パイプラインステージを作成できる")
    void shouldCreatePipelineStage() {
        Pipeline.Stage<String, String> stage = new Pipeline.Stage<>(
            "TestStage",
            input -> input.toUpperCase()
        );

        assertThat(stage.name()).isEqualTo("TestStage");
    }

    @Test
    @DisplayName("パイプラインを通じてデータを処理できる")
    @Timeout(10)
    void shouldProcessDataThroughPipeline() throws InterruptedException {
        List<String> results = new CopyOnWriteArrayList<>();
        CountDownLatch latch = new CountDownLatch(3);

        Pipeline<String> pipeline = new Pipeline<>();
        pipeline.addStage("wash", item -> {
            results.add("washed:" + item);
            return "washed:" + item;
        });
        pipeline.addStage("dry", item -> {
            results.add("dried:" + item);
            return "dried:" + item;
        });
        pipeline.addStage("fold", item -> {
            results.add("folded:" + item);
            latch.countDown();
            return "folded:" + item;
        });

        pipeline.start();
        pipeline.submit("load1");
        pipeline.submit("load2");
        pipeline.submit("load3");

        latch.await();
        pipeline.stop();

        assertThat(results).contains(
            "washed:load1", "dried:washed:load1", "folded:dried:washed:load1"
        );
    }

    @Test
    @DisplayName("空のパイプラインを処理できる")
    void shouldHandleEmptyPipeline() {
        Pipeline<String> pipeline = new Pipeline<>();
        pipeline.start();
        pipeline.stop();
        // エラーなく終了すればOK
    }
}
