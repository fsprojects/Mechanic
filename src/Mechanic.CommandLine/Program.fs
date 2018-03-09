open Mechanic
open Mechanic.Files
open Mechanic.GraphAlg
open Mechanic.Utils

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 1 ->
        SymbolGetter.getExternalDefs (argv.[0])
        exit 0
        let p = ProjectFile.loadFromFile argv.[0]
        p |> ProjectFile.getSourceFiles
        |> SymbolGraph.solveOrder (fun f -> f.FullName)
        |> function
            | TopologicalOrderResult.TopologicalOrder xs ->
                xs |> fun x -> ProjectFile.updateProjectFile x p 
                TopologicalOrderResult.TopologicalOrder (xs |> List.map (fun f -> f.FullName))
            | TopologicalOrderResult.Cycle xs -> TopologicalOrderResult.Cycle (xs |> List.map (fun f -> f.FullName))
        |> printfn "%A"
    | 2 ->
        let root = argv.[0]
        let pattern = argv.[1]
        SymbolGraph.solveOrderFromPattern root pattern 
        |> printfn "%A"
    | _ -> ()
    0
