using System.Diagnostics;

namespace GrokkingConcurrency.CSharp.Ch04;

/// <summary>
/// スレッドの基本操作
/// </summary>
public static class ThreadBasics
{
    /// <summary>
    /// ワーカースレッドを作成
    /// </summary>
    /// <param name="task">実行するタスク</param>
    /// <returns>作成されたスレッド</returns>
    public static Thread CreateWorkerThread(Action task)
    {
        return new Thread(() => task());
    }

    /// <summary>
    /// 複数のワーカースレッドを作成
    /// </summary>
    /// <param name="count">作成するスレッド数</param>
    /// <param name="task">各スレッドで実行するタスク（インデックスを受け取る）</param>
    /// <returns>作成されたスレッドのリスト</returns>
    public static List<Thread> CreateMultipleWorkers(int count, Action<int> task)
    {
        var threads = new List<Thread>();
        for (var i = 0; i < count; i++)
        {
            var index = i;
            var thread = new Thread(() => task(index))
            {
                Name = $"Worker-{index}"
            };
            threads.Add(thread);
        }
        return threads;
    }

    /// <summary>
    /// 名前付きスレッドを作成
    /// </summary>
    /// <param name="name">スレッド名</param>
    /// <param name="task">実行するタスク</param>
    /// <returns>作成されたスレッド</returns>
    public static Thread CreateNamedThread(string name, Action task)
    {
        return new Thread(() => task()) { Name = name };
    }

    /// <summary>
    /// アクティブスレッド数を取得
    /// </summary>
    /// <returns>アクティブなスレッド数</returns>
    public static int GetActiveThreadCount()
    {
        return Process.GetCurrentProcess().Threads.Count;
    }
}
