package concurrency.ch02;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import java.util.List;

import static org.assertj.core.api.Assertions.*;

/**
 * パスワードクラッカーのテスト（逐次処理版）
 */
class PasswordCrackerTest {

    @Test
    @DisplayName("4桁の組み合わせを生成できる")
    void shouldGenerateFourDigitCombinations() {
        List<String> combinations = PasswordCracker.getCombinations(4, 0, 9);

        assertThat(combinations)
            .hasSize(10)
            .containsExactly(
                "0000", "0001", "0002", "0003", "0004",
                "0005", "0006", "0007", "0008", "0009"
            );
    }

    @Test
    @DisplayName("範囲指定で組み合わせを生成できる")
    void shouldGenerateCombinationsInRange() {
        List<String> combinations = PasswordCracker.getCombinations(4, 98, 102);

        assertThat(combinations)
            .hasSize(5)
            .containsExactly("0098", "0099", "0100", "0101", "0102");
    }

    @Test
    @DisplayName("SHA-256ハッシュを計算できる")
    void shouldCalculateSha256Hash() {
        String hash = PasswordCracker.getCryptoHash("12345678");

        assertThat(hash)
            .isEqualTo("ef797c8118f02dfb649607dd5d3f8c7623048c9c063d532cc95c5ed7a898a64f");
    }

    @Test
    @DisplayName("パスワードが一致するか確認できる")
    void shouldCheckPasswordMatch() {
        String expectedHash = "ef797c8118f02dfb649607dd5d3f8c7623048c9c063d532cc95c5ed7a898a64f";

        assertThat(PasswordCracker.checkPassword(expectedHash, "12345678")).isTrue();
        assertThat(PasswordCracker.checkPassword(expectedHash, "87654321")).isFalse();
    }

    @Test
    @DisplayName("パスワードをクラックできる")
    void shouldCrackPassword() {
        // "1234" の SHA-256 ハッシュ
        String cryptoHash = PasswordCracker.getCryptoHash("1234");

        String result = PasswordCracker.crackPassword(cryptoHash, 4);

        assertThat(result).isEqualTo("1234");
    }

    @Test
    @DisplayName("見つからない場合はnullを返す")
    void shouldReturnNullWhenNotFound() {
        // 存在しないハッシュ
        String cryptoHash = "invalid_hash";

        String result = PasswordCracker.crackPassword(cryptoHash, 2);

        assertThat(result).isNull();
    }
}
