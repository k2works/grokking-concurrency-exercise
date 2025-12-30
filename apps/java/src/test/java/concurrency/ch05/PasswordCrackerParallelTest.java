package concurrency.ch05;

import concurrency.ch02.PasswordCracker;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Timeout;

import java.util.List;

import static org.assertj.core.api.Assertions.*;

/**
 * 並列パスワードクラッカーのテスト
 */
class PasswordCrackerParallelTest {

    @Test
    @DisplayName("範囲をチャンクに分割できる")
    void shouldSplitRangeIntoChunks() {
        List<PasswordCrackerParallel.ChunkRange> chunks =
            PasswordCrackerParallel.getChunks(4, 4);

        assertThat(chunks).hasSize(4);
        assertThat(chunks.get(0)).isEqualTo(new PasswordCrackerParallel.ChunkRange(0, 2499));
        assertThat(chunks.get(1)).isEqualTo(new PasswordCrackerParallel.ChunkRange(2500, 4999));
        assertThat(chunks.get(2)).isEqualTo(new PasswordCrackerParallel.ChunkRange(5000, 7499));
        assertThat(chunks.get(3)).isEqualTo(new PasswordCrackerParallel.ChunkRange(7500, 9999));
    }

    @Test
    @DisplayName("チャンク内でパスワードをクラックできる")
    @Timeout(10)
    void shouldCrackPasswordInChunk() {
        String cryptoHash = PasswordCracker.getCryptoHash("1234");

        String result = PasswordCrackerParallel.crackChunk(cryptoHash, 4, 0, 9999);

        assertThat(result).isEqualTo("1234");
    }

    @Test
    @DisplayName("チャンク内にパスワードがない場合はnullを返す")
    void shouldReturnNullWhenNotInChunk() {
        String cryptoHash = PasswordCracker.getCryptoHash("5000");

        String result = PasswordCrackerParallel.crackChunk(cryptoHash, 4, 0, 4999);

        assertThat(result).isNull();
    }

    @Test
    @DisplayName("パスワードを並列でクラックできる")
    @Timeout(30)
    void shouldCrackPasswordInParallel() {
        String cryptoHash = PasswordCracker.getCryptoHash("1234");

        String result = PasswordCrackerParallel.crackPasswordParallel(cryptoHash, 4);

        assertThat(result).isEqualTo("1234");
    }

    @Test
    @DisplayName("大きな範囲のパスワードも並列でクラックできる")
    @Timeout(60)
    void shouldCrackLargerPasswordInParallel() {
        String cryptoHash = PasswordCracker.getCryptoHash("54321");

        String result = PasswordCrackerParallel.crackPasswordParallel(cryptoHash, 5);

        assertThat(result).isEqualTo("54321");
    }
}
