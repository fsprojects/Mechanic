open Mechanic
open Mechanic.Files
open Mechanic.GraphAlg
open Mechanic.Utils

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 1 ->
        let p = ProjectFile.loadFromFile argv.[0]
        p |> ProjectFile.getSourceFiles
        |> SymbolGraph.solveOrder (fun f -> f.FullName)
        |> function
            |TopologicalOrderResult.TopologicalOrder xs ->
                xs |> fun x -> ProjectFile.updateProjectFile x p 
                TopologicalOrderResult.TopologicalOrder xs
            |x -> x
        |> printfn "%A"
    | 2 ->
        let root = argv.[0]
        let pattern = argv.[1]
        SymbolGraph.solveOrderFromPattern root pattern 
        |> printfn "%A"
    0
