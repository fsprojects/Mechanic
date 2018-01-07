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

    // Wait until type checking succeeds (or 100 attempts)
    parseFileResults

let getSymbols file =
    let input = System.IO.File.ReadAllText file 
    let parseFileResults = parseSingleFile(file, input)
    let tree = parseFileResults.ParseTree.Value

    let opens = AstSymbolCollector.getOpenDecls tree |> List.rev
    let defSymbolNames = AstSymbolCollector.getDefSymbols tree |> set |> Set.toList
    let usedSymbolNames = AstSymbolCollector.getUsedSymbols tree |> set |> Set.toList

    file, defSymbolNames, opens, usedSymbolNames
