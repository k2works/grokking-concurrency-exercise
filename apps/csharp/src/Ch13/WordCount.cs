namespace GrokkingConcurrency.CSharp.Ch13;

/// <summary>
/// MapReduce パターンによるワードカウント
/// </summary>
public static class WordCount
{
    /// <summary>
    /// Map: テキストを (word, 1) のペアに変換
    /// </summary>
    public static List<KeyValuePair<string, int>> Map(string text)
    {
        return text.ToLower()
            .Split(' ', StringSplitOptions.RemoveEmptyEntries)
            .Select(word => new KeyValuePair<string, int>(word, 1))
            .ToList();
    }

    /// <summary>
    /// Reduce: 同じ単語のカウントを集約
    /// </summary>
    public static Dictionary<string, int> Reduce(List<KeyValuePair<string, int>> pairs)
    {
        return pairs
            .GroupBy(p => p.Key)
            .ToDictionary(g => g.Key, g => g.Sum(p => p.Value));
    }

    /// <summary>
    /// MapReduce でワードカウント
    /// </summary>
    public static Dictionary<string, int> CountWords(List<string> texts)
    {
        // Map フェーズ（並列）
        var mapped = texts
            .AsParallel()
            .SelectMany(text => Map(text))
            .ToList();

        // Reduce フェーズ
        return Reduce(mapped);
    }
}
