module PasswordCrackerParallelTests

open Xunit
open GrokkingConcurrency.Ch02.PasswordCracker
open GrokkingConcurrency.Ch05.PasswordCrackerParallel

[<Fact>]
let ``getChunks divides range into equal parts`` () =
    let chunks = getChunks 4 100
    Assert.Equal(4, List.length chunks)
    Assert.Equal(0, (List.head chunks).Start)
    Assert.Equal(100, (List.last chunks).End)

[<Fact>]
let ``getChunks handles uneven division`` () =
    let chunks = getChunks 3 10
    Assert.Equal(3, List.length chunks)
    // Verify all elements are covered
    let covered =
        chunks
        |> List.collect (fun c -> [c.Start .. c.End - 1])
        |> Set.ofList
    Assert.True((Set.ofList [0..9]) = covered)

[<Fact>]
let ``crackPasswordParallel finds password`` () =
    let password = "ab"
    let hash = getCryptoHash password
    Assert.Equal(Some password, crackPasswordParallel hash 2)

[<Fact>]
let ``crackPasswordParallel returns None for non-existent password`` () =
    let hash = "0000000000000000000000000000000000000000000000000000000000000000"
    Assert.Equal(None, crackPasswordParallel hash 2)

[<Fact>]
let ``crackPasswordParallel uses multiple threads`` () =
    let password = "zz"
    let hash = getCryptoHash password
    let result = crackPasswordParallel hash 2
    Assert.Equal(Some password, result)
