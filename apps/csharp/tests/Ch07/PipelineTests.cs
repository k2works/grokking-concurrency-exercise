using GrokkingConcurrency.CSharp.Ch07;
using System.Collections.Concurrent;

namespace GrokkingConcurrency.CSharp.Tests.Ch07;

/// <summary>
/// パイプラインパターンのテスト
/// </summary>
public class PipelineTests
{
    [Fact]
    public void ShouldCreatePipelineStage()
    {
        var stage = new Stage<string, string>("TestStage", input => input.ToUpper());

        Assert.Equal("TestStage", stage.Name);
    }

    [Fact]
    public async Task ShouldProcessDataThroughPipeline()
    {
        var results = new ConcurrentBag<string>();
        using var countdown = new CountdownEvent(3);

        var pipeline = new Pipeline<string>();
        pipeline.AddStage("wash", item =>
        {
            var str = (string)item;
            results.Add("washed:" + str);
            return "washed:" + str;
        });
        pipeline.AddStage("dry", item =>
        {
            var str = (string)item;
            results.Add("dried:" + str);
            return "dried:" + str;
        });
        pipeline.AddStage("fold", item =>
        {
            var str = (string)item;
            results.Add("folded:" + str);
            countdown.Signal();
            return "folded:" + str;
        });

        pipeline.Start();
        pipeline.Submit("load1");
        pipeline.Submit("load2");
        pipeline.Submit("load3");

        countdown.Wait(TimeSpan.FromSeconds(10));
        pipeline.Stop();

        Assert.Contains("washed:load1", results);
        Assert.Contains("dried:washed:load1", results);
        Assert.Contains("folded:dried:washed:load1", results);
    }

    [Fact]
    public void ShouldHandleEmptyPipeline()
    {
        var pipeline = new Pipeline<string>();
        pipeline.Start();
        pipeline.Stop();
        // エラーなく終了すればOK
    }
}
