package concurrency.ch05;

import concurrency.ch02.PasswordCracker;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * パスワードクラッカー（並列処理版）
 * 複数スレッドで並列にパスワードを解読する
 */
public class PasswordCrackerParallel {

    /**
     * チャンク範囲を表すレコード
     */
    public record ChunkRange(int start, int end) {}

    /**
     * パスワード範囲をチャンクに分割
     *
     * @param numChunks チャンク数
     * @param length    パスワードの桁数
     * @return チャンク範囲のリスト
     */
    public static List<ChunkRange> getChunks(int numChunks, int length) {
        int maxNumber = (int) Math.pow(10, length) - 1;
        int chunkSize = (maxNumber + 1) / numChunks;

        List<ChunkRange> chunks = new ArrayList<>();
        for (int i = 0; i < numChunks; i++) {
            int start = i * chunkSize;
            int end = (i == numChunks - 1) ? maxNumber : start + chunkSize - 1;
            chunks.add(new ChunkRange(start, end));
        }
        return chunks;
    }

    /**
     * チャンク内でパスワードを探索
     *
     * @param cryptoHash パスワードのハッシュ
     * @param length     パスワードの桁数
     * @param chunkStart チャンクの開始位置
     * @param chunkEnd   チャンクの終了位置
     * @return 見つかったパスワード、見つからなければnull
     */
    public static String crackChunk(String cryptoHash, int length, int chunkStart, int chunkEnd) {
        System.out.printf("Processing %d to %d%n", chunkStart, chunkEnd);
        List<String> combinations = PasswordCracker.getCombinations(length, chunkStart, chunkEnd);

        for (String combination : combinations) {
            if (PasswordCracker.checkPassword(cryptoHash, combination)) {
                return combination;
            }
        }
        return null;
    }

    /**
     * 複数スレッドでパスワードを並列解読
     *
     * @param cryptoHash パスワードのハッシュ
     * @param length     パスワードの桁数
     * @return 解読されたパスワード、見つからなければnull
     */
    public static String crackPasswordParallel(String cryptoHash, int length) {
        int numCores = Runtime.getRuntime().availableProcessors();
        List<ChunkRange> chunks = getChunks(numCores, length);

        try (ExecutorService executor = Executors.newFixedThreadPool(numCores)) {
            List<Future<String>> futures = new ArrayList<>();

            for (ChunkRange chunk : chunks) {
                Future<String> future = executor.submit(() ->
                    crackChunk(cryptoHash, length, chunk.start(), chunk.end())
                );
                futures.add(future);
            }

            for (Future<String> future : futures) {
                try {
                    String result = future.get();
                    if (result != null) {
                        executor.shutdownNow();
                        return result;
                    }
                } catch (InterruptedException | ExecutionException e) {
                    Thread.currentThread().interrupt();
                }
            }
        }

        return null;
    }

    public static void main(String[] args) {
        // "12345678" の SHA-256 ハッシュ
        String cryptoHash = "ef797c8118f02dfb649607dd5d3f8c7623048c9c063d532cc95c5ed7a898a64f";
        int length = 8;

        System.out.println("Processing number combinations concurrently");
        System.out.println("Available processors: " + Runtime.getRuntime().availableProcessors());
        long startTime = System.nanoTime();

        String result = crackPasswordParallel(cryptoHash, length);

        long processTime = System.nanoTime() - startTime;
        if (result != null) {
            System.out.println("PASSWORD CRACKED: " + result);
        }
        System.out.printf("PROCESS TIME: %.3f seconds%n", processTime / 1_000_000_000.0);
    }
}
