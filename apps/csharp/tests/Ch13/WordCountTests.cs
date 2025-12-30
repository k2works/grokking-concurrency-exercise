using GrokkingConcurrency.CSharp.Ch13;

namespace GrokkingConcurrency.CSharp.Tests.Ch13;

/// <summary>
/// MapReduce パターンによるワードカウントのテスト
/// </summary>
public class WordCountTests
{
    [Fact]
    public void ShouldMapTextToWordCountPairs()
    {
        var text = "hello world hello";

        var pairs = WordCount.Map(text);

        Assert.Equal(3, pairs.Count);
        Assert.Contains(pairs, p => p.Key == "hello" && p.Value == 1);
        Assert.Contains(pairs, p => p.Key == "world" && p.Value == 1);
    }

    [Fact]
    public void ShouldReduceWordCountPairs()
    {
        var pairs = new List<KeyValuePair<string, int>>
        {
            new("hello", 1),
            new("world", 1),
            new("hello", 1)
        };

        var result = WordCount.Reduce(pairs);

        Assert.Equal(2, result["hello"]);
        Assert.Equal(1, result["world"]);
    }

    [Fact]
    public void ShouldCountWordsInMultipleTexts()
    {
        var texts = new List<string>
        {
            "hello world",
            "hello again",
            "world peace"
        };

        var result = WordCount.CountWords(texts);

        Assert.Equal(2, result["hello"]);
        Assert.Equal(2, result["world"]);
        Assert.Equal(1, result["again"]);
        Assert.Equal(1, result["peace"]);
    }
}
