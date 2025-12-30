using System.Collections.Concurrent;

namespace GrokkingConcurrency.CSharp.Ch07;

/// <summary>
/// パイプラインのステージ
/// </summary>
public record Stage<TInput, TOutput>(string Name, Func<TInput, TOutput> Processor);

/// <summary>
/// パイプラインパターン
/// 処理をステージに分割し、各ステージを並列に実行
/// </summary>
public class Pipeline<T>
{
    private readonly List<StageWorker> _workers = [];
    private readonly List<BlockingCollection<object>> _queues = [];
    private volatile bool _running;

    /// <summary>
    /// ステージを追加
    /// </summary>
    public void AddStage(string name, Func<object, object> processor)
    {
        BlockingCollection<object> inputQueue;
        if (_queues.Count == 0)
        {
            inputQueue = new BlockingCollection<object>();
            _queues.Add(inputQueue);
        }
        else
        {
            inputQueue = _queues[^1];
        }

        var outputQueue = new BlockingCollection<object>();
        _queues.Add(outputQueue);

        var worker = new StageWorker(name, inputQueue, outputQueue, processor, this);
        _workers.Add(worker);
    }

    /// <summary>
    /// パイプラインを開始
    /// </summary>
    public void Start()
    {
        _running = true;
        foreach (var worker in _workers)
        {
            worker.Start();
        }
    }

    /// <summary>
    /// データを送信
    /// </summary>
    public void Submit(T item)
    {
        if (_queues.Count > 0)
        {
            _queues[0].TryAdd(item!);
        }
    }

    /// <summary>
    /// パイプラインを停止
    /// </summary>
    public void Stop()
    {
        _running = false;
        foreach (var worker in _workers)
        {
            worker.Stop();
        }
    }

    /// <summary>
    /// ステージワーカー
    /// </summary>
    private class StageWorker
    {
        private readonly string _name;
        private readonly BlockingCollection<object> _inputQueue;
        private readonly BlockingCollection<object> _outputQueue;
        private readonly Func<object, object> _processor;
        private readonly Pipeline<T> _pipeline;
        private readonly Thread _thread;
        private readonly CancellationTokenSource _cts = new();

        public StageWorker(string name, BlockingCollection<object> inputQueue,
                          BlockingCollection<object> outputQueue, Func<object, object> processor,
                          Pipeline<T> pipeline)
        {
            _name = name;
            _inputQueue = inputQueue;
            _outputQueue = outputQueue;
            _processor = processor;
            _pipeline = pipeline;
            _thread = new Thread(Run) { Name = name, IsBackground = true };
        }

        public void Start() => _thread.Start();

        public void Stop()
        {
            _cts.Cancel();
        }

        private void Run()
        {
            while (_pipeline._running || _inputQueue.Count > 0)
            {
                try
                {
                    if (_inputQueue.TryTake(out var item, 100, _cts.Token))
                    {
                        Console.WriteLine($"{_name}: processing {item}");
                        var result = _processor(item);
                        _outputQueue.TryAdd(result);
                    }
                }
                catch (OperationCanceledException)
                {
                    break;
                }
            }
        }
    }
}
