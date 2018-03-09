module Mechanic.SymbolGetter
open Microsoft.FSharp.Compiler.SourceCodeServices
open Mechanic.AstSymbolCollector
open System
open Mechanic
open System.IO
let checker = FSharpChecker.Create()

let parseSingleFile (file, input) = 
    let (projOptions, _) = 
        checker.GetProjectOptionsFromScript(file, input)
        |> Async.RunSynchronously

    let (parsingOptions, _) = checker.GetParsingOptionsFromProjectOptions projOptions
  
    let parseFileResults = 
        checker.ParseFile(file, input, parsingOptions) 
        |> Async.RunSynchronously

    parseFileResults

let getSymbols file =
    if not (System.IO.File.Exists file) then
        failwithf "The file %s does not exist." file

    let input = System.IO.File.ReadAllText file 
    let parseFileResults = parseSingleFile(file, input)
    let tree = parseFileResults.ParseTree.Value
    //printfn "%A" tree

    let defs = AstSymbolCollector.getDefSymbols tree
    let opens = AstSymbolCollector.getOpenDecls defs tree
    let defSymbolNames = 
        AstSymbolCollector.getDefSymbols tree |> List.choose (function { LocalRange = None; SymbolName = s } -> Some s | _ -> None)
        |> set |> Set.toList 
        |> List.filter (Symbol.get >> Utils.Namespace.lastPart >> (fun x -> x.StartsWith "op_") >> not)

    file, defSymbolNames, opens

let getExternalDefs projFile =
    let projFile = (FileInfo projFile).FullName
    let (_,fscArgs) = Utils.Shell.runCmd "src/Mechanic" "dotnet" (sprintf "proj-info %s --fsc-args" projFile)
    printfn "%A" fscArgs
    
    let mkTempFile content =
        let tempPath = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName() + ".fs")
        File.WriteAllText(tempPath, content)
        tempPath
    let emptyLibSource = """module Tmp
let foo = 42"""

    let fscArgs = (fscArgs |> List.filter (fun l -> not(isNull l) && l.StartsWith("-"))) @ [mkTempFile emptyLibSource]
    printfn "%A" fscArgs
    let projOpts = checker.GetProjectOptionsFromCommandLineArgs(projFile, fscArgs |> List.toArray)
    let wholeProjectResults = checker.ParseAndCheckProject(projOpts) |> Async.RunSynchronously
    printfn "%A" wholeProjectResults.Errors
    let rec getSymbols entities = entities |> Seq.collect (fun (e: FSharpEntity) -> [e.TryFullName |> Option.defaultValue e.DisplayName] @ getSymbols e.NestedEntities) |> Seq.toList
    wholeProjectResults.ProjectContext.GetReferencedAssemblies() |> List.collect (fun a -> getSymbols a.Contents.Entities)
    |> List.iter (printfn "%A")