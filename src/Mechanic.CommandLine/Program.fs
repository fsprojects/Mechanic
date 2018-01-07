open Mechanic

[<EntryPoint>]
let main argv =
    SymbolGraph.solveOrder (argv |> Array.toList) |> printfn "%A"
    0
