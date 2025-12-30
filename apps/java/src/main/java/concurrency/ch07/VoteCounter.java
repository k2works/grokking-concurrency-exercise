package concurrency.ch07;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.stream.IntStream;

/**
 * Fork/Join パターンによる投票集計
 */
public class VoteCounter {

    /**
     * 投票を集計（逐次処理）
     */
    public static Map<Integer, Integer> countVotes(List<Integer> votes) {
        Map<Integer, Integer> result = new HashMap<>();
        for (Integer vote : votes) {
            result.merge(vote, 1, Integer::sum);
        }
        return result;
    }

    /**
     * 投票を並列で集計（Fork/Join パターン）
     */
    public static Map<Integer, Integer> countVotesParallel(List<Integer> votes, int numWorkers) {
        if (votes.isEmpty()) {
            return new HashMap<>();
        }

        int chunkSize = (int) Math.ceil((double) votes.size() / numWorkers);
        List<List<Integer>> chunks = new ArrayList<>();

        for (int i = 0; i < votes.size(); i += chunkSize) {
            chunks.add(votes.subList(i, Math.min(i + chunkSize, votes.size())));
        }

        try (ExecutorService executor = Executors.newFixedThreadPool(numWorkers)) {
            // Fork: 各ワーカーにチャンクを割り当て
            List<Future<Map<Integer, Integer>>> futures = new ArrayList<>();
            for (List<Integer> chunk : chunks) {
                futures.add(executor.submit(() -> countVotes(chunk)));
            }

            // Join: 結果を統合
            Map<Integer, Integer> total = new HashMap<>();
            for (Future<Map<Integer, Integer>> future : futures) {
                Map<Integer, Integer> partialResult = future.get();
                total = mergeResults(total, partialResult);
            }
            return total;
        } catch (InterruptedException | ExecutionException e) {
            Thread.currentThread().interrupt();
            throw new RuntimeException(e);
        }
    }

    /**
     * 2つの集計結果を統合
     */
    public static Map<Integer, Integer> mergeResults(Map<Integer, Integer> result1,
                                                      Map<Integer, Integer> result2) {
        Map<Integer, Integer> merged = new HashMap<>(result1);
        result2.forEach((key, value) -> merged.merge(key, value, Integer::sum));
        return merged;
    }

    public static void main(String[] args) {
        // 100万票を生成
        List<Integer> votes = IntStream.range(0, 1_000_000)
            .map(i -> (int) (Math.random() * 10) + 1)
            .boxed()
            .toList();

        System.out.println("Sequential counting...");
        long start = System.nanoTime();
        Map<Integer, Integer> result1 = countVotes(votes);
        long seqTime = System.nanoTime() - start;
        System.out.printf("Sequential time: %.3f ms%n", seqTime / 1_000_000.0);

        System.out.println("\nParallel counting (4 workers)...");
        start = System.nanoTime();
        Map<Integer, Integer> result2 = countVotesParallel(votes, 4);
        long parTime = System.nanoTime() - start;
        System.out.printf("Parallel time: %.3f ms%n", parTime / 1_000_000.0);

        System.out.println("\nResults:");
        result1.forEach((k, v) -> System.out.printf("Candidate %d: %d votes%n", k, v));
    }
}
