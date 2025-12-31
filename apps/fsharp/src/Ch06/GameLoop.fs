namespace GrokkingConcurrency.Ch06

open System.Threading

type GameTask = { Name: string; Action: unit -> unit }

type ProcessorFreeEvent = {
    Lock: obj
    mutable Signaled: bool
}

module GameLoop =
    /// Run a single task
    let runTask (task: GameTask) : unit =
        task.Action()

    /// Create a new processor free event
    let createProcessorFreeEvent () : ProcessorFreeEvent =
        { Lock = obj(); Signaled = false }

    /// Wait for the event to be signaled
    let waitForSignal (event: ProcessorFreeEvent) : unit =
        lock event.Lock (fun () ->
            while not event.Signaled do
                Monitor.Wait(event.Lock) |> ignore
        )

    /// Signal the event
    let signal (event: ProcessorFreeEvent) : unit =
        lock event.Lock (fun () ->
            event.Signaled <- true
            Monitor.PulseAll(event.Lock)
        )

    /// Reset the event
    let reset (event: ProcessorFreeEvent) : unit =
        lock event.Lock (fun () ->
            event.Signaled <- false
        )

    /// Run all tasks in a single frame
    let runOneFrame (tasks: GameTask list) : unit =
        tasks |> List.iter runTask

    /// Run game loop for specified number of frames
    let run (tasks: GameTask list) (frames: int) : unit =
        for frame in 1 .. frames do
            printfn $"Frame {frame}"
            runOneFrame tasks
