open System
open System.IO
open Mechanic
open Mechanic.Files
open Mechanic.GraphAlg

[<EntryPoint>]
let main argv =
    let loadFromDir dir =
        Directory.EnumerateFiles dir
        |> Seq.map ProjectFile.tryLoad
        |> Seq.filter Option.isSome
        |> Seq.map (fun projectFile -> projectFile.Value)

    let projectFiles =
        if argv.Length = 0 then
            loadFromDir Environment.CurrentDirectory
        else
            try
                let path = Path.Combine (Environment.CurrentDirectory, argv.[0])
                let attributes = File.GetAttributes path
                if attributes.HasFlag FileAttributes.Directory then
                    loadFromDir path
                else
                    let fi = FileInfo path
                    match fi.Extension with
                    | ".fsproj" ->
                       let projectFile = ProjectFile.loadFromFile path
                       seq {
                           yield projectFile
                       }
                    | _ -> Seq.empty
            with
            | _ -> Seq.empty

    let solve (n, m) projectFile =
        let options = { LogOutput = Options.LogOutput.Default }
        printfn "Project %s" projectFile.FileName

        projectFile
        |> ProjectFile.getSourceFiles
        |> SymbolGraph.solveOrder options (fun f -> f.FullName) (Some projectFile.FileName)
        |> function
            | TopologicalOrderResult.TopologicalOrder xs ->
                ProjectFile.updateProjectFile xs projectFile
                TopologicalOrderResult.TopologicalOrder (xs |> List.map (fun f -> f.FullName))
                |> printfn "%A\n"
                (n + 1, m)
            | TopologicalOrderResult.Cycle xs ->
                TopologicalOrderResult.Cycle (xs |> List.map (fun f -> f.FullName))
                |> printfn "%A\n"
                (n, m + 1)

    if Seq.isEmpty projectFiles then
        printfn "No *.fsproj files found in %s" Environment.CurrentDirectory
    else
        printfn "found %i projects\n" (Seq.length projectFiles)
        projectFiles
        |> Seq.fold solve (0, 0)
        |> fun (n, m) ->
            printfn "\nupdate %i projects, %i projects untouched because of cycle dependency" n m

    0 // return an integer exit code
