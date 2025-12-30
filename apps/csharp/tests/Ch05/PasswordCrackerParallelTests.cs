using GrokkingConcurrency.CSharp.Ch02;
using GrokkingConcurrency.CSharp.Ch05;

namespace GrokkingConcurrency.CSharp.Tests.Ch05;

/// <summary>
/// 並列パスワードクラッカーのテスト
/// </summary>
public class PasswordCrackerParallelTests
{
    [Fact]
    public void ShouldSplitRangeIntoChunks()
    {
        var chunks = PasswordCrackerParallel.GetChunks(4, 4);

        Assert.Equal(4, chunks.Count);
        Assert.Equal(new ChunkRange(0, 2499), chunks[0]);
        Assert.Equal(new ChunkRange(2500, 4999), chunks[1]);
        Assert.Equal(new ChunkRange(5000, 7499), chunks[2]);
        Assert.Equal(new ChunkRange(7500, 9999), chunks[3]);
    }

    [Fact]
    public void ShouldCrackPasswordInChunk()
    {
        var cryptoHash = PasswordCracker.GetCryptoHash("1234");

        var result = PasswordCrackerParallel.CrackChunk(cryptoHash, 4, 0, 9999);

        Assert.Equal("1234", result);
    }

    [Fact]
    public void ShouldReturnNullWhenNotInChunk()
    {
        var cryptoHash = PasswordCracker.GetCryptoHash("5000");

        var result = PasswordCrackerParallel.CrackChunk(cryptoHash, 4, 0, 4999);

        Assert.Null(result);
    }

    [Fact]
    public void ShouldCrackPasswordInParallel()
    {
        var cryptoHash = PasswordCracker.GetCryptoHash("1234");

        var result = PasswordCrackerParallel.CrackPasswordParallel(cryptoHash, 4);

        Assert.Equal("1234", result);
    }

    [Fact]
    public void ShouldCrackLargerPasswordInParallel()
    {
        var cryptoHash = PasswordCracker.GetCryptoHash("54321");

        var result = PasswordCrackerParallel.CrackPasswordParallel(cryptoHash, 5);

        Assert.Equal("54321", result);
    }
}
