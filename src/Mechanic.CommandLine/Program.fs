open Mechanic

[<EntryPoint>]
let main argv =
    let root = argv.[0]
    let pattern = argv.[1]
    SymbolGraph.solveOrderFromPattern root pattern 
    |> printfn "%A"
    0
