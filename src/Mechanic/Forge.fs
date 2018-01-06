module Mechanic.Forge

open System
open System.Diagnostics

let [<Literal>] defaultTimeout = 10000

let makeStartInfo path args = 
    ProcessStartInfo (
        FileName = path,
        Arguments = args,
        UseShellExecute = false,
        RedirectStandardOutput = true,
        RedirectStandardError = true
    )

let setupHandlers (p:Process) =
    let handler f _ (args:DataReceivedEventArgs) = f args.Data
    let output = ResizeArray()
    let errors = ResizeArray()
    p.OutputDataReceived.AddHandler(DataReceivedEventHandler (handler output.Add))
    p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (handler errors.Add))
    output, errors

let cleanOutput (output:ResizeArray<_>) =
    output
    |> Seq.filter (String.IsNullOrWhiteSpace >> not) 
    |> List.ofSeq

let execute (startInfo:ProcessStartInfo) =
    use p = new Process(StartInfo = startInfo)
    let output, errors = setupHandlers p
    match p.Start() with
    | true ->
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit(defaultTimeout) |> ignore
    | false ->
        failwith "Failed to start process"
    cleanOutput output, cleanOutput errors, p.ExitCode

let moveFileUp (forgePath:string) (project:string) (file:string) =
    sprintf "move file -p %s -n %s --up --no-prompt" project file
    |> makeStartInfo forgePath
    |> execute  

let moveFileDown (forgePath:string) (project:string) (file:string) =
    sprintf "move file -p %s -n %s --down --no-prompt" project file
    |> makeStartInfo forgePath
    |> execute

let list (forgePath:string) (project:string) =
    sprintf "list files -p %s --no-prompt" project
    |> makeStartInfo forgePath
    |> execute