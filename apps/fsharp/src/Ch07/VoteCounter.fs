namespace GrokkingConcurrency.Ch07

open System

module VoteCounter =
    /// Count votes sequentially
    let countVotes (votes: string list) : Map<string, int> =
        votes
        |> List.groupBy id
        |> List.map (fun (key, group) -> (key, List.length group))
        |> Map.ofList

    /// Merge two vote count results
    let mergeResults (a: Map<string, int>) (b: Map<string, int>) : Map<string, int> =
        let allKeys = Set.union (Map.keys a |> Set.ofSeq) (Map.keys b |> Set.ofSeq)
        allKeys
        |> Set.toList
        |> List.map (fun key ->
            let countA = Map.tryFind key a |> Option.defaultValue 0
            let countB = Map.tryFind key b |> Option.defaultValue 0
            (key, countA + countB))
        |> Map.ofList

    /// Count votes using fork-join pattern
    let countVotesParallel (votes: string list) : Map<string, int> =
        if List.isEmpty votes then
            Map.empty
        else
            let numCores = Environment.ProcessorCount
            let chunkSize = max 1 (List.length votes / numCores)
            let chunks = votes |> List.chunkBySize chunkSize

            let results =
                chunks
                |> List.map (fun chunk -> async { return countVotes chunk })
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.toList

            results |> List.reduce mergeResults
