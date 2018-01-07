open Mechanic

[<EntryPoint>]
let main argv =
    Say.hello "World from F#" |> printfn "%s"
    SymbolGraph.getDependencies (argv |> Array.toList)
    0
