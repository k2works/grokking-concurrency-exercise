using GrokkingConcurrency.CSharp.Ch07;

namespace GrokkingConcurrency.CSharp.Tests.Ch07;

/// <summary>
/// Fork/Join パターンによる投票集計のテスト
/// </summary>
public class VoteCounterTests
{
    [Fact]
    public void ShouldCountVotes()
    {
        var votes = new List<int> { 1, 2, 1, 3, 2, 1, 1, 2, 3, 1 };

        var result = VoteCounter.CountVotes(votes);

        Assert.Equal(5, result[1]);
        Assert.Equal(3, result[2]);
        Assert.Equal(2, result[3]);
    }

    [Fact]
    public void ShouldHandleEmptyList()
    {
        var votes = new List<int>();

        var result = VoteCounter.CountVotes(votes);

        Assert.Empty(result);
    }

    [Fact]
    public void ShouldCountVotesInParallel()
    {
        // 10万票を生成
        var votes = Enumerable.Range(0, 100000)
            .Select(i => i % 10 + 1)
            .ToList();

        var result = VoteCounter.CountVotesParallel(votes, 4);

        // 各候補者に1万票ずつ
        for (var i = 1; i <= 10; i++)
        {
            Assert.Equal(10000, result[i]);
        }
    }

    [Fact]
    public void ShouldMergeResults()
    {
        var result1 = new Dictionary<int, int> { { 1, 5 }, { 2, 3 } };
        var result2 = new Dictionary<int, int> { { 1, 3 }, { 3, 4 } };

        var merged = VoteCounter.MergeResults(result1, result2);

        Assert.Equal(8, merged[1]);
        Assert.Equal(3, merged[2]);
        Assert.Equal(4, merged[3]);
    }
}
