namespace GrokkingConcurrency.CSharp.Ch07;

/// <summary>
/// Fork/Join パターンによる投票集計
/// </summary>
public static class VoteCounter
{
    /// <summary>
    /// 投票を集計（逐次処理）
    /// </summary>
    public static Dictionary<int, int> CountVotes(List<int> votes)
    {
        var result = new Dictionary<int, int>();
        foreach (var vote in votes)
        {
            if (result.ContainsKey(vote))
            {
                result[vote]++;
            }
            else
            {
                result[vote] = 1;
            }
        }
        return result;
    }

    /// <summary>
    /// 投票を並列で集計（Fork/Join パターン）
    /// </summary>
    public static Dictionary<int, int> CountVotesParallel(List<int> votes, int numWorkers)
    {
        if (votes.Count == 0)
        {
            return new Dictionary<int, int>();
        }

        var chunkSize = (int)Math.Ceiling((double)votes.Count / numWorkers);
        var chunks = new List<List<int>>();

        for (var i = 0; i < votes.Count; i += chunkSize)
        {
            var endIndex = Math.Min(i + chunkSize, votes.Count);
            chunks.Add(votes.GetRange(i, endIndex - i));
        }

        // Fork: 各ワーカーにチャンクを割り当て
        var tasks = chunks.Select(chunk =>
            Task.Run(() => CountVotes(chunk))
        ).ToArray();

        // Join: 結果を統合
        Task.WaitAll(tasks);
        var total = new Dictionary<int, int>();
        foreach (var task in tasks)
        {
            total = MergeResults(total, task.Result);
        }
        return total;
    }

    /// <summary>
    /// 2つの集計結果を統合
    /// </summary>
    public static Dictionary<int, int> MergeResults(Dictionary<int, int> result1,
                                                     Dictionary<int, int> result2)
    {
        var merged = new Dictionary<int, int>(result1);
        foreach (var (key, value) in result2)
        {
            if (merged.ContainsKey(key))
            {
                merged[key] += value;
            }
            else
            {
                merged[key] = value;
            }
        }
        return merged;
    }
}
