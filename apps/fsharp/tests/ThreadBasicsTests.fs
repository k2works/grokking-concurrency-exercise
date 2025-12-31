module ThreadBasicsTests

open Xunit
open System.Collections.Concurrent
open GrokkingConcurrency.Ch04.ThreadBasics

[<Fact>]
let ``createWorker creates a thread that runs the given task`` () =
    let mutable executed = false
    let thread = createWorker "TestWorker" (fun () -> executed <- true)
    thread.Start()
    thread.Join()
    Assert.True(executed)

[<Fact>]
let ``createWorker sets thread name`` () =
    let thread = createWorker "MyWorker" (fun () -> ())
    Assert.Equal("MyWorker", thread.Name)

[<Fact>]
let ``createMultipleWorkers creates specified number of threads`` () =
    let threads = createMultipleWorkers 5 (fun _ -> ())
    Assert.Equal(5, List.length threads)

[<Fact>]
let ``createMultipleWorkers threads execute with correct indices`` () =
    let results = ConcurrentQueue<int>()
    let threads = createMultipleWorkers 3 (fun i -> results.Enqueue(i))
    threads |> List.iter (fun t -> t.Start())
    threads |> List.iter (fun t -> t.Join())
    let resultSet = results |> Seq.toList |> Set.ofList
    Assert.True((Set.ofList [0; 1; 2]) = resultSet)

[<Fact>]
let ``startAndJoinAll runs all threads`` () =
    let results = ConcurrentQueue<string>()
    let threads = createMultipleWorkers 3 (fun i -> results.Enqueue($"Worker-{i}"))
    startAndJoinAll threads
    Assert.Equal(3, results.Count)
