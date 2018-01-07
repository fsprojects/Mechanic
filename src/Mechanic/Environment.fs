module Mechanic.Environment

open System
open System.IO


// Environment constants
let [<Literal>] FsCoreName        = "FSharp.Core"
let [<Literal>] SystemLibLocation = @"\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0"
let [<Literal>] FsLibLocation     = @"\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0"


let programFiles = Environment.GetFolderPath Environment.SpecialFolder.ProgramFilesX86

let isWin32 = System.Environment.OSVersion.Platform = System.PlatformID.Win32NT

let runtimeDir = Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()

let join (paths:string list) = 
    String.Join("", paths)

let pathCombine name path =
    Path.Combine(path, name)

let toDll name = name + ".dll"

let systemLib name = 
    if isWin32 then 
        join [programFiles; SystemLibLocation]
        |> pathCombine (toDll name)
    else
        runtimeDir
        |> pathCombine (toDll name)

let fsCore =
    if isWin32 then
        join [programFiles; FsLibLocation]
        |> pathCombine (toDll FsCoreName)
    else
        systemLib FsCoreName
