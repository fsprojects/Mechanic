open Mechanic
open Mechanic.Files

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 1 ->
        ProjectFile.loadFromFile argv.[0]
        |> ProjectFile.getSourceFiles
        |> List.map (fun f -> f.FullName)
        |> SymbolGraph.solveOrder
        |> printfn "%A"
    | 2 ->
        let root = argv.[0]
        let pattern = argv.[1]
        SymbolGraph.solveOrderFromPattern root pattern 
        |> printfn "%A"
    0
