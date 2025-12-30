namespace GrokkingConcurrency.CSharp.Ch06;

/// <summary>
/// タスクを表すレコード
/// </summary>
public record GameTask(string Name, Action Action)
{
    public void Run() => Action();
}

/// <summary>
/// プロセッサ空き状態を示すイベント
/// threading.Event の C# 版
/// </summary>
public class ProcessorFreeEvent
{
    private readonly object _lock = new();
    private bool _signaled;

    /// <summary>
    /// シグナルを待機
    /// </summary>
    public void WaitForSignal()
    {
        lock (_lock)
        {
            while (!_signaled)
            {
                Monitor.Wait(_lock);
            }
            _signaled = false; // シグナルを消費
        }
    }

    /// <summary>
    /// シグナルを送信
    /// </summary>
    public void Signal()
    {
        lock (_lock)
        {
            _signaled = true;
            Monitor.Pulse(_lock);
        }
    }
}

/// <summary>
/// ゲームループ - マルチタスキングの例
/// 入力処理、ゲーム計算、描画処理を順番に実行
/// </summary>
public class GameLoop(
    Action inputTask,
    Action computeTask,
    Action renderTask,
    Func<bool> continueCondition)
{
    /// <summary>
    /// ゲームループを実行
    /// </summary>
    public void Run()
    {
        while (continueCondition())
        {
            inputTask();
            computeTask();
            renderTask();
        }
    }
}
