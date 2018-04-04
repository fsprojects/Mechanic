module Mechanic.ProjectInfo
open Mechanic.Utils

let getFscArgs = memoize <| fun projFile ->
    let projFile = (System.IO.FileInfo projFile).FullName
    let runRestore() =
        let args = (sprintf "restore %s" projFile)
        let (exitCode, logOut) = Utils.Shell.runCmd "." "dotnet" args
        if exitCode <> 0 || logOut |> Seq.exists (fun l -> l.Contains "error MSB") then 
            let msg = (sprintf "\"dotnet %s\" failed (exitCode %i) with output:" args exitCode) :: logOut |> String.concat System.Environment.NewLine
            failwith msg
    match projFile with
    | ProjectCracker.ProjectRecognizer.NetCoreSdk -> runRestore()
    | _ -> ()
    
    let (projOpts,_,_) = ProjectCracker.GetProjectOptionsFromProjectFile projFile
    let fscArgs = projOpts.OtherOptions |> Seq.toList
    fscArgs |> List.filter (fun l -> not(isNull l) && l.StartsWith("-"))