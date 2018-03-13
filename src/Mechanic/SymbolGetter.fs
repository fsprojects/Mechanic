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
