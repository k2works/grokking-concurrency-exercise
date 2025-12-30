using GrokkingConcurrency.CSharp.Ch04;
using System.Collections.Concurrent;

namespace GrokkingConcurrency.CSharp.Tests.Ch04;

/// <summary>
/// スレッド基本操作のテスト
/// </summary>
public class ThreadBasicsTests
{
    [Fact]
    public async Task ShouldCreateAndRunThread()
    {
        var results = new ConcurrentBag<string>();
        using var semaphore = new SemaphoreSlim(0, 1);

        var thread = ThreadBasics.CreateWorkerThread(() =>
        {
            results.Add("task executed");
            semaphore.Release();
        });

        thread.Start();
        await semaphore.WaitAsync(TimeSpan.FromSeconds(5));

        Assert.Single(results);
        Assert.Contains("task executed", results);
    }

    [Fact]
    public async Task ShouldRunMultipleThreadsConcurrently()
    {
        var results = new ConcurrentBag<int>();
        const int threadCount = 5;
        using var countdown = new CountdownEvent(threadCount);

        var threads = ThreadBasics.CreateMultipleWorkers(threadCount, i =>
        {
            results.Add(i);
            countdown.Signal();
        });

        foreach (var thread in threads)
        {
            thread.Start();
        }

        countdown.Wait(TimeSpan.FromSeconds(5));

        Assert.Equal(threadCount, results.Count);
        Assert.Equal(new[] { 0, 1, 2, 3, 4 }, results.OrderBy(x => x));
    }

    [Fact]
    public void ShouldSetThreadName()
    {
        var thread = ThreadBasics.CreateNamedThread("TestWorker", () => { });

        Assert.Equal("TestWorker", thread.Name);
    }

    [Fact]
    public void ShouldWaitForThreadCompletion()
    {
        var results = new ConcurrentBag<string>();

        var thread = ThreadBasics.CreateWorkerThread(() =>
        {
            Thread.Sleep(100);
            results.Add("completed");
        });

        thread.Start();
        Assert.Empty(results);

        thread.Join();
        Assert.Single(results);
        Assert.Contains("completed", results);
    }

    [Fact]
    public void ShouldGetActiveThreadCount()
    {
        var count = ThreadBasics.GetActiveThreadCount();

        Assert.True(count >= 1);
    }
}
