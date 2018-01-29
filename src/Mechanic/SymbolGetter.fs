module Mechanic.SymbolGetter
open Microsoft.FSharp.Compiler.SourceCodeServices
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

    let opens = AstSymbolCollector.getOpenDecls tree
    let defSymbolNames = 
        AstSymbolCollector.getDefSymbols tree |> set |> Set.toList 
        |> List.filter (Utils.Namespace.lastPart >> (fun x -> x.StartsWith "op_") >> not)

    file, defSymbolNames, opens
