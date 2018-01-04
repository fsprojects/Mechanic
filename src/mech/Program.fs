// Learn more about F# at http://fsharp.org

open System
open Argu
open Mechanic

type CLIArguments =
    | Working_Directory of path:string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Working_Directory _ -> "specify a working directory."
 
let parser = ArgumentParser.Create<CLIArguments>(programName = "mech.exe")
 
[<EntryPoint>]
let main argv =
    let results = parser.Parse argv
    let wd = results.GetResult (<@ Working_Directory @>, defaultValue = ".")
    printfn "Working_Directory=%s" wd
    printfn "foo=%s" Say.foo
    0 // return an integer exit code
