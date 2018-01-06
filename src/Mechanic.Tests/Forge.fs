module Forge.Tests

open System.IO
open Mechanic
open Expecto


let [<Literal>] ForgePath = "../paket-files/github.com/fsharp-editing/Forge/temp/Forge.exe"

let makeTempProject () = 
    let projectFileText = sprintf """<?xml version="1.0" encoding="utf-8"?>
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>netstandard2.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="%s" />
    <Compile Include="%s" />
  </ItemGroup>
</Project>
"""
    let tempPath = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
    Directory.CreateDirectory tempPath |> ignore
    let pf = Path.Combine(tempPath, "TestProject.fsproj")
    let f1 = Path.Combine(tempPath, "TestFile1.fs")
    let f2 = Path.Combine(tempPath, "TestFile2.fs")
    File.WriteAllText(pf, (projectFileText "TestFile1.fs" "TestFile2.fs"))
    File.WriteAllText(f1, "namespace Test")
    File.WriteAllText(f2, "namespace Test")
    tempPath, pf, f1, f2


[<Tests>]
let tests =
    testList "Forge command tests" [
        testCase "Forge list command returns source files from project file" <| fun _ ->
            let path, pf, f1, f2 = makeTempProject()
            let output, errors, exitCode = Forge.list ForgePath pf
            Directory.Delete(path, true)
            Expect.equal output ["TestFile1.fs"; "TestFile2.fs"] "File names list matches"
            Expect.equal errors [] "Errors are empty"
            Expect.equal exitCode 0 "Exit code is zero"
        
        testCase "Forge moves file down correctly" <| fun _ ->
            let path, pf, f1, f2 = makeTempProject()
            let output, errors, exitCode = Forge.moveFileDown ForgePath pf f1
            let newOrder, _, _ = Forge.list ForgePath pf
            Directory.Delete(path, true)
            Expect.equal newOrder ["TestFile2.fs"; "TestFile1.fs"] "File names list matches"
            Expect.equal errors [] "Errors are empty"
            Expect.equal exitCode 0 "Exit code is zero"
        
        testCase "Forge moves file up correctly" <| fun _ ->
            let path, pf, f1, f2 = makeTempProject()
            let output, errors, exitCode = Forge.moveFileUp ForgePath pf f2
            let newOrder, _, _ = Forge.list ForgePath pf
            Directory.Delete(path, true)
            Expect.equal newOrder ["TestFile2.fs"; "TestFile1.fs"] "File names list matches"
            Expect.equal errors [] "Errors are empty"
            Expect.equal exitCode 0 "Exit code is zero"
    ]

    
    
    
