package concurrency.ch02;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.List;

/**
 * パスワードクラッカー（逐次処理版）
 * ブルートフォース（総当たり）でパスワードを解読する
 */
public class PasswordCracker {

    /**
     * 指定された桁数のパスワード組み合わせを生成
     *
     * @param length    パスワードの桁数
     * @param minNumber 最小値
     * @param maxNumber 最大値
     * @return パスワード候補のリスト
     */
    public static List<String> getCombinations(int length, int minNumber, int maxNumber) {
        List<String> combinations = new ArrayList<>();
        String format = "%0" + length + "d";

        for (int i = minNumber; i <= maxNumber; i++) {
            combinations.add(String.format(format, i));
        }

        return combinations;
    }

    /**
     * パスワードのSHA-256ハッシュを計算
     *
     * @param password パスワード
     * @return SHA-256ハッシュ（16進数文字列）
     */
    public static String getCryptoHash(String password) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest(password.getBytes(StandardCharsets.UTF_8));
            return HexFormat.of().formatHex(hash);
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("SHA-256 algorithm not available", e);
        }
    }

    /**
     * パスワードが一致するか確認
     *
     * @param expectedCryptoHash 期待されるハッシュ
     * @param possiblePassword   候補パスワード
     * @return 一致すればtrue
     */
    public static boolean checkPassword(String expectedCryptoHash, String possiblePassword) {
        String actualCryptoHash = getCryptoHash(possiblePassword);
        return expectedCryptoHash.equals(actualCryptoHash);
    }

    /**
     * ブルートフォースでパスワードを解読
     *
     * @param cryptoHash パスワードのハッシュ
     * @param length     パスワードの桁数
     * @return 解読されたパスワード、見つからなければnull
     */
    public static String crackPassword(String cryptoHash, int length) {
        int maxNumber = (int) Math.pow(10, length) - 1;
        List<String> combinations = getCombinations(length, 0, maxNumber);

        for (String combination : combinations) {
            if (checkPassword(cryptoHash, combination)) {
                return combination;
            }
        }

        return null;
    }

    public static void main(String[] args) {
        // "12345678" の SHA-256 ハッシュ
        String cryptoHash = "ef797c8118f02dfb649607dd5d3f8c7623048c9c063d532cc95c5ed7a898a64f";
        int length = 8;

        System.out.println("Processing number combinations sequentially");
        long startTime = System.nanoTime();

        String result = crackPassword(cryptoHash, length);

        long processTime = System.nanoTime() - startTime;
        if (result != null) {
            System.out.println("PASSWORD CRACKED: " + result);
        }
        System.out.printf("PROCESS TIME: %.3f seconds%n", processTime / 1_000_000_000.0);
    }
}
