namespace GrokkingConcurrency.Ch07

type Stage = { Name: string; Processor: obj -> obj }

type Pipeline<'T> = { Stages: Stage list }

module Pipeline =
    /// Create a new empty pipeline
    let createPipeline<'T> : Pipeline<'T> =
        { Stages = [] }

    /// Add a stage to the pipeline
    let addStage (name: string) (processor: obj -> obj) (pipeline: Pipeline<'T>) : Pipeline<'T> =
        { pipeline with Stages = pipeline.Stages @ [{ Name = name; Processor = processor }] }

    /// Process data through all stages
    let run (pipeline: Pipeline<'T>) (data: 'T list) : obj list =
        if List.isEmpty pipeline.Stages then
            data |> List.map (fun x -> x :> obj)
        else
            data
            |> List.map (fun item ->
                pipeline.Stages
                |> List.fold (fun current stage -> stage.Processor current) (item :> obj)
            )
