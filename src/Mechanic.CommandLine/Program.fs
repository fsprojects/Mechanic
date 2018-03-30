open Mechanic
open Mechanic.Files
open Mechanic.GraphAlg
open Mechanic.Utils
open Mechanic.Options

[<EntryPoint>]
let main argv =
    let options = { LogOutput = LogOutput.Default }
    match argv.Length with
    | 1 ->
        let p = ProjectFile.loadFromFile argv.[0]
        p |> ProjectFile.getSourceFiles
        |> SymbolGraph.solveOrder options (fun f -> f.FullName) (Some argv.[0])
        |> function
            | TopologicalOrderResult.TopologicalOrder xs ->
                xs |> fun x -> ProjectFile.updateProjectFile x p 
                TopologicalOrderResult.TopologicalOrder (xs |> List.map (fun f -> f.FullName))
            | TopologicalOrderResult.Cycle xs -> TopologicalOrderResult.Cycle (xs |> List.map (fun f -> f.FullName))
        |> printfn "%A"
    | 2 ->
        let root = argv.[0]
        let pattern = argv.[1]
        SymbolGraph.solveOrderFromPattern options root pattern 
        |> printfn "%A"
    0
