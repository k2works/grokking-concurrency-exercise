module GameLoopTests

open Xunit
open System.Collections.Concurrent
open System.Threading
open GrokkingConcurrency.Ch06
open GrokkingConcurrency.Ch06.GameLoop

[<Fact>]
let ``GameTask executes its action`` () =
    let mutable executed = false
    let task: GameTask = { Name = "TestTask"; Action = fun () -> executed <- true }
    runTask task
    Assert.True(executed)

[<Fact>]
let ``ProcessorFreeEvent blocks until signaled`` () =
    let event = createProcessorFreeEvent ()
    let results = ConcurrentQueue<string>()

    let waiter = Thread(ThreadStart(fun () ->
        waitForSignal event
        results.Enqueue("waited")
    ))

    waiter.Start()
    Thread.Sleep(50)
    Assert.Equal(0, results.Count) // Still waiting

    signal event
    waiter.Join(1000) |> ignore
    Assert.Equal(1, results.Count)

[<Fact>]
let ``runOneFrame runs all tasks`` () =
    let results = ConcurrentQueue<string>()
    let tasks: GameTask list = [
        { Name = "Task1"; Action = fun () -> results.Enqueue("task1") }
        { Name = "Task2"; Action = fun () -> results.Enqueue("task2") }
        { Name = "Task3"; Action = fun () -> results.Enqueue("task3") }
    ]

    runOneFrame tasks
    Assert.Equal(3, results.Count)

[<Fact>]
let ``runOneFrame runs tasks in order`` () =
    let results = ConcurrentQueue<string>()
    let tasks: GameTask list = [
        { Name = "First"; Action = fun () -> results.Enqueue("1") }
        { Name = "Second"; Action = fun () -> results.Enqueue("2") }
        { Name = "Third"; Action = fun () -> results.Enqueue("3") }
    ]

    runOneFrame tasks
    let resultList = results |> Seq.toList
    Assert.True((["1"; "2"; "3"]) = resultList)
