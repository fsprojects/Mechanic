open Mechanic

[<EntryPoint>]
let main argv =
    Say.hello "World from F#" |> printfn "%s"
    SymbolGetter.run argv.[0]
    0
