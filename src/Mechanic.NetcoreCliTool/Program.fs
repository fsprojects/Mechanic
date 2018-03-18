open System
open System.IO
open Mechanic
open Mechanic.Files
open Mechanic.GraphAlg
open Mechanic.Files

[<EntryPoint>]
let main argv =
    let projectDirectory =
        if argv.Length = 0 then
            Environment.CurrentDirectory
        else
            Path.Combine (Environment.CurrentDirectory, argv.[0])

    let solve (n, m) projectFile =
        printfn "Project %s" projectFile.FileName

        projectFile
        |> ProjectFile.getSourceFiles
        |> SymbolGraph.solveOrder (fun f -> f.FullName)
        |> function
            | TopologicalOrderResult.TopologicalOrder xs ->
                ProjectFile.updateProjectFile xs projectFile
                TopologicalOrderResult.TopologicalOrder (xs |> List.map (fun f -> f.FullName))
                |> printfn "%A"
                (n + 1, m)
            | TopologicalOrderResult.Cycle xs ->
                TopologicalOrderResult.Cycle (xs |> List.map (fun f -> f.FullName))
                |> printfn "%A"
                (n, m + 1)

    let projectFiles =
        Directory.EnumerateFiles projectDirectory
        |> Seq.map ProjectFile.tryLoad
        |> Seq.filter Option.isSome
        |> Seq.map (fun projectFile -> projectFile.Value)

    if Seq.isEmpty projectFiles then
        printfn "No *.fsproj files found in %s" Environment.CurrentDirectory
    else
        projectFiles
        |> Seq.fold solve (0, 0)
        |> fun (n, m) ->
            printfn "\nupdate %i projects, %i projects untouched because of cycle dependency" n m

    0 // return an integer exit code
