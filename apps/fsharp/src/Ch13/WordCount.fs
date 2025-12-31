namespace GrokkingConcurrency.Ch13

open System

module WordCount =
    /// Map: Convert text to (word, 1) pairs
    let map (text: string) : (string * int) list =
        text.ToLower().Split([|' '; '\t'; '\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.filter (fun s -> s.Length > 0)
        |> Array.map (fun word -> (word, 1))
        |> Array.toList

    /// Reduce: Aggregate word counts
    let reduce (pairs: (string * int) list) : Map<string, int> =
        pairs
        |> List.groupBy fst
        |> List.map (fun (key, group) -> (key, group |> List.sumBy snd))
        |> Map.ofList

    /// MapReduce: Count words in multiple texts using parallel execution
    let countWords (texts: string list) : Map<string, int> =
        // Map phase (parallel)
        let mapped =
            texts
            |> List.toArray
            |> Array.Parallel.collect (fun text -> map text |> List.toArray)
            |> Array.toList

        // Reduce phase
        reduce mapped
