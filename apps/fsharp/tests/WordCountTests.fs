module WordCountTests

open Xunit
open GrokkingConcurrency.Ch13.WordCount

[<Fact>]
let ``map converts text to word-count pairs`` () =
    let result = map "hello world hello"
    Assert.Contains(("hello", 1), result)
    Assert.Contains(("world", 1), result)
    let helloCount = result |> List.filter (fun (w, _) -> w = "hello") |> List.length
    Assert.Equal(2, helloCount)

[<Fact>]
let ``reduce aggregates word counts`` () =
    let pairs = [("hello", 1); ("world", 1); ("hello", 1)]
    let result = reduce pairs
    Assert.Equal(2, Map.find "hello" result)
    Assert.Equal(1, Map.find "world" result)

[<Fact>]
let ``countWords performs MapReduce`` () =
    let texts = [
        "hello world"
        "hello fsharp"
        "world of fsharp"
    ]
    let result = countWords texts
    Assert.Equal(2, Map.find "hello" result)
    Assert.Equal(2, Map.find "world" result)
    Assert.Equal(2, Map.find "fsharp" result)
    Assert.Equal(1, Map.find "of" result)
