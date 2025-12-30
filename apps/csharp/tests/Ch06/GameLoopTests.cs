using GrokkingConcurrency.CSharp.Ch06;
using System.Collections.Concurrent;

namespace GrokkingConcurrency.CSharp.Tests.Ch06;

/// <summary>
/// ゲームループのテスト
/// </summary>
public class GameLoopTests
{
    [Fact]
    public void ShouldCreateTask()
    {
        var counter = 0;

        var task = new GameTask("TestTask", () => counter++);

        Assert.Equal("TestTask", task.Name);
    }

    [Fact]
    public async Task ShouldExecuteTask()
    {
        var counter = 0;
        using var semaphore = new SemaphoreSlim(0, 1);

        var task = new GameTask("TestTask", () =>
        {
            counter++;
            semaphore.Release();
        });

        var thread = new Thread(() => task.Run());
        thread.Start();
        await semaphore.WaitAsync(TimeSpan.FromSeconds(5));

        Assert.Equal(1, counter);
    }

    [Fact]
    public async Task ShouldCoordinateTasksWithEvent()
    {
        var executionOrder = new ConcurrentBag<string>();
        var processorEvent = new ProcessorFreeEvent();
        using var allDone = new CountdownEvent(3);

        var t1 = new Thread(() =>
        {
            processorEvent.WaitForSignal();
            executionOrder.Add("task1");
            processorEvent.Signal();
            allDone.Signal();
        });

        var t2 = new Thread(() =>
        {
            processorEvent.WaitForSignal();
            executionOrder.Add("task2");
            processorEvent.Signal();
            allDone.Signal();
        });

        var t3 = new Thread(() =>
        {
            processorEvent.WaitForSignal();
            executionOrder.Add("task3");
            processorEvent.Signal();
            allDone.Signal();
        });

        t1.Start();
        t2.Start();
        t3.Start();

        // スレッドが待機状態になるのを待つ
        await Task.Delay(50);
        processorEvent.Signal();

        allDone.Wait(TimeSpan.FromSeconds(5));

        Assert.Equal(3, executionOrder.Count);
    }

    [Fact]
    public void ShouldRunGameLoopWithMultipleTasks()
    {
        var results = new ConcurrentBag<string>();
        var cycleCount = 0;
        const int maxCycles = 3;

        var gameLoop = new GameLoop(
            () => results.Add("input"),
            () => results.Add("compute"),
            () => results.Add("render"),
            () => ++cycleCount <= maxCycles
        );

        gameLoop.Run();

        // 3サイクル × 3タスク = 9回
        Assert.Equal(9, results.Count);
    }
}
