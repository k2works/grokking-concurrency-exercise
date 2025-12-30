using GrokkingConcurrency.CSharp.Ch02;

namespace GrokkingConcurrency.CSharp.Tests.Ch02;

/// <summary>
/// パスワードクラッカーのテスト（逐次処理版）
/// </summary>
public class PasswordCrackerTests
{
    [Fact]
    public void ShouldGenerateFourDigitCombinations()
    {
        var combinations = PasswordCracker.GetCombinations(4, 0, 9);

        Assert.Equal(10, combinations.Count);
        Assert.Equal(
            new[] { "0000", "0001", "0002", "0003", "0004", "0005", "0006", "0007", "0008", "0009" },
            combinations
        );
    }

    [Fact]
    public void ShouldGenerateCombinationsInRange()
    {
        var combinations = PasswordCracker.GetCombinations(4, 98, 102);

        Assert.Equal(5, combinations.Count);
        Assert.Equal(new[] { "0098", "0099", "0100", "0101", "0102" }, combinations);
    }

    [Fact]
    public void ShouldCalculateSha256Hash()
    {
        var hash = PasswordCracker.GetCryptoHash("12345678");

        Assert.Equal("ef797c8118f02dfb649607dd5d3f8c7623048c9c063d532cc95c5ed7a898a64f", hash);
    }

    [Fact]
    public void ShouldCheckPasswordMatch()
    {
        var expectedHash = "ef797c8118f02dfb649607dd5d3f8c7623048c9c063d532cc95c5ed7a898a64f";

        Assert.True(PasswordCracker.CheckPassword(expectedHash, "12345678"));
        Assert.False(PasswordCracker.CheckPassword(expectedHash, "87654321"));
    }

    [Fact]
    public void ShouldCrackPassword()
    {
        // "1234" の SHA-256 ハッシュ
        var cryptoHash = PasswordCracker.GetCryptoHash("1234");

        var result = PasswordCracker.CrackPassword(cryptoHash, 4);

        Assert.Equal("1234", result);
    }

    [Fact]
    public void ShouldReturnNullWhenNotFound()
    {
        // 存在しないハッシュ
        var cryptoHash = "invalid_hash";

        var result = PasswordCracker.CrackPassword(cryptoHash, 2);

        Assert.Null(result);
    }
}
