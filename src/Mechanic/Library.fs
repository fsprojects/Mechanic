namespace Mechanic

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

module SymbolGetter =
    let checker = FSharpChecker.Create()

    let parseAndTypeCheckSingleFile (file, input) = 
        
        let projOptions = 
            let sysLib nm = 
                if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then
                    // file references only valid on Windows
                    System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86) +
                    @"\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\" + nm + ".dll"
                else
                    let sysDir = System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
                    let (++) a b = System.IO.Path.Combine(a,b)
                    sysDir ++ nm + ".dll" 

            let fsCore4300() = 
                if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then
                    // file references only valid on Windows
                    System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86) +
                    @"\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll"  
                else 
                    sysLib "FSharp.Core"

            checker.GetProjectOptionsFromCommandLineArgs
               ("tmp.fsproj",
                [| yield "--simpleresolution" 
                   yield "--noframework" 
                   yield "--debug:full" 
                   yield "--define:DEBUG" 
                   yield "--optimize-" 
                   yield "--out:" + "tmp.dll"
                   yield "--doc:test.xml" 
                   yield "--warn:3" 
                   yield "--fullpaths" 
                   yield "--flaterrors" 
                   yield "--target:library" 
                   yield file
                   let references =
                     [ sysLib "mscorlib" 
                       sysLib "System"
                       sysLib "System.Core"
                       //fsCore4300() 
                       ]
                   for r in references do 
                         yield "-r:" + r |])
      
        let (parseFileResults, checkFileResults) = 
            checker.ParseAndCheckFileInProject(file, 0, input, projOptions) 
            |> Async.RunSynchronously

        // Wait until type checking succeeds (or 100 attempts)
        match checkFileResults with
        | FSharpCheckFileAnswer.Succeeded(res) -> parseFileResults, res
        | res -> failwithf "Parsing did not finish... (%A)" res

    let run file =

        let input = System.IO.File.ReadAllText file 

        let (parseFileResults, checkFileResults) = 
            parseAndTypeCheckSingleFile(file, input)

        printfn "%A" checkFileResults.Errors
        let (defSymbols, usedSymbols) = 
            checkFileResults.GetAllUsesOfAllSymbolsInFile() |> Async.RunSynchronously
            |> Array.toList
            |> List.partition (fun s -> s.IsFromDefinition)

        printfn "Def: %A" (defSymbols |> List.map (fun s -> s.Symbol.FullName))
        printfn "Used: %A" (usedSymbols |> List.map (fun s ->s.Symbol.FullName))

module Say =
    let hello name =
        sprintf "Hello %s" name
