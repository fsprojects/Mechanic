open Mechanic

[<EntryPoint>]
let main argv =
    Say.hello "World from F#" |> printfn "%s"
    SymbolGraph.solveOrder (argv |> Array.toList) |> printfn "%A"
    0
