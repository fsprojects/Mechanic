module Mechanic.SymbolGetter
open Microsoft.FSharp.Compiler.SourceCodeServices
open Mechanic.AstSymbolCollector
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
    // printfn "%A" tree

    let defs = AstSymbolCollector.getDefSymbols tree
    let opens = AstSymbolCollector.getOpenDecls defs tree
    let defSymbolNames = 
        AstSymbolCollector.getDefSymbols tree |> List.choose (function { LocalRange = None; SymbolName = s } -> Some s | _ -> None)
        |> set |> Set.toList 
        |> List.filter (Symbol.get >> Utils.Namespace.lastPart >> (fun x -> x.StartsWith "op_") >> not)

    file, defSymbolNames, opens

let getExternalFindDefFun projFile =
    let projFile = (FileInfo projFile).FullName
    Utils.Shell.runCmd "." "dotnet" (sprintf "restore %s" projFile) |> ignore
    let (projOpts,_,_) = ProjectCracker.GetProjectOptionsFromProjectFile projFile
    let fscArgs = projOpts.OtherOptions |> Seq.toList
    //fscArgs |> Seq.iter (printfn "%A")
    
    let mkTempFile content =
        let tempPath = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName() + ".fs")
        File.WriteAllText(tempPath, content)
        tempPath
    let emptyLibSource = """module Tmp
let foo = 42"""

    let fscArgs = (fscArgs |> List.filter (fun l -> not(isNull l) && l.StartsWith("-"))) @ [mkTempFile emptyLibSource]
    //printfn "%A" fscArgs
    let projOpts = checker.GetProjectOptionsFromCommandLineArgs(projFile, fscArgs |> List.toArray)
    let wholeProjectResults = checker.ParseAndCheckProject(projOpts) |> Async.RunSynchronously
    // printfn "%A" wholeProjectResults.Errors
    let assemblies = wholeProjectResults.ProjectContext.GetReferencedAssemblies()
    fun x -> assemblies |> List.tryPick (fun a -> a.Contents.FindEntityByPath (Symbol.get x |> Utils.Namespace.splitByDot)  |> Option.map (fun _ -> x))
