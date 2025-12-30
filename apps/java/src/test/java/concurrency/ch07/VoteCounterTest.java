package concurrency.ch07;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Timeout;

import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;

import static org.assertj.core.api.Assertions.*;

/**
 * Fork/Join パターンによる投票集計のテスト
 */
class VoteCounterTest {

    @Test
    @DisplayName("投票を集計できる")
    void shouldCountVotes() {
        List<Integer> votes = List.of(1, 2, 1, 3, 2, 1, 1, 2, 3, 1);

        Map<Integer, Integer> result = VoteCounter.countVotes(votes);

        assertThat(result).containsEntry(1, 5);
        assertThat(result).containsEntry(2, 3);
        assertThat(result).containsEntry(3, 2);
    }

    @Test
    @DisplayName("空のリストを集計できる")
    void shouldHandleEmptyList() {
        List<Integer> votes = List.of();

        Map<Integer, Integer> result = VoteCounter.countVotes(votes);

        assertThat(result).isEmpty();
    }

    @Test
    @DisplayName("並列で投票を集計できる")
    @Timeout(10)
    void shouldCountVotesInParallel() {
        // 10万票を生成
        List<Integer> votes = IntStream.range(0, 100000)
            .map(i -> i % 10 + 1)  // 1-10 の候補者
            .boxed()
            .toList();

        Map<Integer, Integer> result = VoteCounter.countVotesParallel(votes, 4);

        // 各候補者に1万票ずつ
        for (int i = 1; i <= 10; i++) {
            assertThat(result).containsEntry(i, 10000);
        }
    }

    @Test
    @DisplayName("結果を統合できる")
    void shouldMergeResults() {
        Map<Integer, Integer> result1 = Map.of(1, 5, 2, 3);
        Map<Integer, Integer> result2 = Map.of(1, 3, 3, 4);

        Map<Integer, Integer> merged = VoteCounter.mergeResults(result1, result2);

        assertThat(merged).containsEntry(1, 8);
        assertThat(merged).containsEntry(2, 3);
        assertThat(merged).containsEntry(3, 4);
    }
}
