module PipelineTests

open Xunit
open GrokkingConcurrency.Ch07.Pipeline

[<Fact>]
let ``Pipeline processes data through stages`` () =
    let pipeline =
        createPipeline<int>
        |> addStage "double" (fun (x: obj) -> (x :?> int) * 2 :> obj)
        |> addStage "addOne" (fun (x: obj) -> (x :?> int) + 1 :> obj)

    let results = run pipeline [1; 2; 3]
    let intResults = results |> List.map (fun x -> x :?> int)
    Assert.True(([3; 5; 7]) = intResults) // (1*2)+1, (2*2)+1, (3*2)+1

[<Fact>]
let ``Pipeline handles single stage`` () =
    let pipeline =
        createPipeline<string>
        |> addStage "uppercase" (fun (s: obj) -> (s :?> string).ToUpper() :> obj)

    let results = run pipeline ["hello"; "world"]
    let stringResults = results |> List.map (fun x -> x :?> string)
    Assert.True((["HELLO"; "WORLD"]) = stringResults)

[<Fact>]
let ``Pipeline handles type conversions`` () =
    let pipeline =
        createPipeline<int>
        |> addStage "toString" (fun (x: obj) -> (x :?> int).ToString() :> obj)
        |> addStage "length" (fun (x: obj) -> (x :?> string).Length :> obj)

    let results = run pipeline [1; 10; 100]
    let intResults = results |> List.map (fun x -> x :?> int)
    Assert.True(([1; 2; 3]) = intResults)
