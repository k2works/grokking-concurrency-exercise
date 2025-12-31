namespace GrokkingConcurrency.Ch05

open System
open System.Threading
open GrokkingConcurrency.Ch02.PasswordCracker

type ChunkRange = { Start: int; End: int }

module PasswordCrackerParallel =
    /// Divide a range into chunks
    let getChunks (numChunks: int) (totalSize: int) : ChunkRange list =
        let chunkSize = totalSize / numChunks
        let remainder = totalSize % numChunks

        [ for i in 0 .. numChunks - 1 do
            let start = i * chunkSize + min i remainder
            let extraOne = if i < remainder then 1 else 0
            yield { Start = start; End = start + chunkSize + extraOne } ]

    /// Crack password using parallel execution
    let crackPasswordParallel (cryptoHash: string) (length: int) : string option =
        let combinations = getCombinations length
        let numCores = Environment.ProcessorCount
        let chunks = getChunks numCores (List.length combinations)

        let result = ref None
        let combinationsArray = combinations |> List.toArray

        let tasks =
            chunks
            |> List.map (fun chunk ->
                async {
                    let subList = combinationsArray.[chunk.Start .. chunk.End - 1]
                    for password in subList do
                        if Option.isNone !result && checkPassword password cryptoHash then
                            result := Some password
                })

        tasks
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore

        !result
