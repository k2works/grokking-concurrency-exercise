module VoteCounterTests

open Xunit
open GrokkingConcurrency.Ch07.VoteCounter

[<Fact>]
let ``countVotes counts votes for each candidate`` () =
    let votes = ["A"; "B"; "A"; "C"; "A"; "B"]
    let result = countVotes votes
    Assert.Equal(3, Map.find "A" result)
    Assert.Equal(2, Map.find "B" result)
    Assert.Equal(1, Map.find "C" result)

[<Fact>]
let ``countVotesParallel counts votes using fork-join`` () =
    let votes = List.replicate 100 "A" @ List.replicate 50 "B" @ List.replicate 30 "C"
    let result = countVotesParallel votes
    Assert.Equal(100, Map.find "A" result)
    Assert.Equal(50, Map.find "B" result)
    Assert.Equal(30, Map.find "C" result)

[<Fact>]
let ``countVotesParallel handles empty list`` () =
    let result = countVotesParallel []
    Assert.True(Map.isEmpty result)

[<Fact>]
let ``mergeResults combines two vote counts`` () =
    let a = Map.ofList [("A", 10); ("B", 5)]
    let b = Map.ofList [("A", 5); ("C", 3)]
    let result = mergeResults a b
    Assert.Equal(15, Map.find "A" result)
    Assert.Equal(5, Map.find "B" result)
    Assert.Equal(3, Map.find "C" result)
