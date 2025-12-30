using GrokkingConcurrency.CSharp.Ch02;

namespace GrokkingConcurrency.CSharp.Ch05;

/// <summary>
/// チャンク範囲を表すレコード
/// </summary>
public record ChunkRange(int Start, int End);

/// <summary>
/// パスワードクラッカー（並列処理版）
/// 複数スレッドで並列にパスワードを解読する
/// </summary>
public static class PasswordCrackerParallel
{
    /// <summary>
    /// パスワード範囲をチャンクに分割
    /// </summary>
    /// <param name="numChunks">チャンク数</param>
    /// <param name="length">パスワードの桁数</param>
    /// <returns>チャンク範囲のリスト</returns>
    public static List<ChunkRange> GetChunks(int numChunks, int length)
    {
        var maxNumber = (int)Math.Pow(10, length) - 1;
        var chunkSize = (maxNumber + 1) / numChunks;

        var chunks = new List<ChunkRange>();
        for (var i = 0; i < numChunks; i++)
        {
            var start = i * chunkSize;
            var end = i == numChunks - 1 ? maxNumber : start + chunkSize - 1;
            chunks.Add(new ChunkRange(start, end));
        }
        return chunks;
    }

    /// <summary>
    /// チャンク内でパスワードを探索
    /// </summary>
    /// <param name="cryptoHash">パスワードのハッシュ</param>
    /// <param name="length">パスワードの桁数</param>
    /// <param name="chunkStart">チャンクの開始位置</param>
    /// <param name="chunkEnd">チャンクの終了位置</param>
    /// <returns>見つかったパスワード、見つからなければnull</returns>
    public static string? CrackChunk(string cryptoHash, int length, int chunkStart, int chunkEnd)
    {
        Console.WriteLine($"Processing {chunkStart} to {chunkEnd}");
        var combinations = PasswordCracker.GetCombinations(length, chunkStart, chunkEnd);

        foreach (var combination in combinations)
        {
            if (PasswordCracker.CheckPassword(cryptoHash, combination))
            {
                return combination;
            }
        }
        return null;
    }

    /// <summary>
    /// 複数スレッドでパスワードを並列解読
    /// </summary>
    /// <param name="cryptoHash">パスワードのハッシュ</param>
    /// <param name="length">パスワードの桁数</param>
    /// <returns>解読されたパスワード、見つからなければnull</returns>
    public static string? CrackPasswordParallel(string cryptoHash, int length)
    {
        var numCores = Environment.ProcessorCount;
        var chunks = GetChunks(numCores, length);

        using var cts = new CancellationTokenSource();
        var tasks = new List<Task<string?>>();

        foreach (var chunk in chunks)
        {
            var task = Task.Run(() =>
                CrackChunk(cryptoHash, length, chunk.Start, chunk.End), cts.Token);
            tasks.Add(task);
        }

        try
        {
            while (tasks.Count > 0)
            {
                var completedTask = Task.WhenAny(tasks).Result;
                var result = completedTask.Result;
                if (result != null)
                {
                    cts.Cancel();
                    return result;
                }
                tasks.Remove(completedTask);
            }
        }
        catch (AggregateException)
        {
            // キャンセルによる例外を無視
        }

        return null;
    }
}
