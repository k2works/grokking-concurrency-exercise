namespace GrokkingConcurrency.Ch04

open System.Threading

module ThreadBasics =
    /// Create a worker thread with the given name and task
    let createWorker (name: string) (task: unit -> unit) : Thread =
        let thread = Thread(ThreadStart(task))
        thread.Name <- name
        thread

    /// Create multiple worker threads
    let createMultipleWorkers (count: int) (task: int -> unit) : Thread list =
        [ for i in 0 .. count - 1 do
            let thread = Thread(ThreadStart(fun () -> task i))
            thread.Name <- $"Worker-{i}"
            yield thread ]

    /// Start all threads and wait for them to complete
    let startAndJoinAll (threads: Thread list) : unit =
        threads |> List.iter (fun t -> t.Start())
        threads |> List.iter (fun t -> t.Join())
