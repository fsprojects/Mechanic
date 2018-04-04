open Argu
open Mechanic
open Mechanic.Files
open Mechanic.GraphAlg
open Mechanic.Utils
open Mechanic.Options

type CliArguments =
    | [<MainCommand; Unique>] Project of string
    | Pattern of string * string
    | Dry_Run
    | Shuffle_Test
    | Log_Ast_Tree
    | Log_Collected_Symbols
    | Log_File_Dependencies
    | Log_File_Dependencies_With_Symbols
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Project _ -> "Project file."
            | Pattern _ -> "Alternative to project file - specify directory and wildcard pattern. Only print out resulting order."
            | Dry_Run -> "Don't update project file."
            | Shuffle_Test -> "Do extensive testing of the correctness of Mechanic on given project. 
Tries varoius order of source files and check Mechanic result by compiler."
            | Log_Ast_Tree -> "Print out AST tree for each source file from project."
            | Log_Collected_Symbols -> "Print out collected symbols for each source file from project."
            | Log_File_Dependencies -> "Print out file dependencies in project."
            | Log_File_Dependencies_With_Symbols -> "Print out file dependencies (with depended symbols) in project."

let (|ProjectArg|_|) = (fun (opts: ParseResults<CliArguments>) -> opts.TryGetResult Project)
let (|PatternArg|_|) = (fun (opts: ParseResults<CliArguments>) -> opts.TryGetResult Pattern)
let (|ShuffleTestArg|_|) = (fun (opts: ParseResults<CliArguments>) -> 
    opts.TryGetResult Project |> Option.bind (fun p -> if opts.Contains Shuffle_Test then Some p else None))

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArguments>()
    let parsedOpts = 
        try
            Some <| parser.Parse argv
        with :? ArguParseException -> None
    match parsedOpts with
    | None -> 
        printfn "%s" <| parser.PrintUsage()
    | Some opts ->
        let options = 
            { LogOutput = 
                { LogOutput.Default with 
                   AstTree = opts.Contains Log_Ast_Tree 
                   CollectedSymbols = opts.Contains Log_Collected_Symbols
                   FileDependencies = opts.Contains Log_File_Dependencies
                   FileDependenciesWithSymbols = opts.Contains Log_File_Dependencies_With_Symbols
                } }
        match opts with
        | ShuffleTestArg projFile ->
            let p = ProjectFile.loadFromFile projFile
            ProjectFile.getSourceFiles p |> SymbolGraph.shuffleTest options (fun f -> f.FullName) projFile |> ignore
        | ProjectArg projFile ->
            let p = ProjectFile.loadFromFile projFile
            p |> ProjectFile.getSourceFiles
            |> SymbolGraph.solveOrder options (fun f -> f.FullName) (Some projFile)
            |> function
                | TopologicalOrderResult.TopologicalOrder xs ->
                    if (not <| opts.Contains Dry_Run) then xs |> fun x -> ProjectFile.updateProjectFile x p 
                    TopologicalOrderResult.TopologicalOrder (xs |> List.map (fun f -> f.FullName))
                | TopologicalOrderResult.Cycle xs -> TopologicalOrderResult.Cycle (xs |> List.map (fun f -> f.FullName))
            |> printfn "%A"
        | PatternArg (root, pattern) ->
            SymbolGraph.solveOrderFromPattern options root pattern 
            |> printfn "%A"
        | _ -> printfn "%s" <| parser.PrintUsage()
    0