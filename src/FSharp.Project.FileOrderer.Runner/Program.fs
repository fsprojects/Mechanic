open FSharp.Project.FileOrderer

[<EntryPoint>]
let main argv =
    Say.hello "World from F#" |> printfn "%s"
    0
