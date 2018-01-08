module Mechanic.Tests.Files

open System
open System.IO
open Expecto
open Mechanic.Files

let projectFileText = """<?xml version="1.0" encoding="utf-8"?><Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><TargetFramework>netstandard2.0</TargetFramework></PropertyGroup><ItemGroup><Compile Include="File1.fs" /><Compile Include="File2.fs" /><Compile Include="File3.fs" /></ItemGroup></Project>"""

let missingProjectNode = """<?xml version="1.0" encoding="utf-8"?><PropertyGroup><TargetFramework>netstandard2.0</TargetFramework></PropertyGroup>"""

let makeTempProjFile contents =
    let file = Path.GetTempFileName()
    let pFile = Path.ChangeExtension(file, ".fsproj")
    File.WriteAllText(pFile, contents, Text.Encoding.UTF8)
    pFile


[<Tests>]
 let tests =
     testList "Project file tests" [
 
         testCase "Project file is loaded and created from disk" <| fun _ ->
            let pFile = makeTempProjFile projectFileText
            let pf = ProjectFile.loadFromFile pFile
            File.Delete(pFile)
            Expect.equal pf.FileName pFile "File path is loaded correctly"
            Expect.equal pf.Document.OuterXml projectFileText "File contents are correct"

         testCase "Project file load throws when Project node is missing" <| fun _ ->
            let pFile = makeTempProjFile missingProjectNode
            Expect.throws
              (fun _ -> ProjectFile.loadFromFile pFile |> ignore) 
              "Throws when project node is missing"
            File.Delete(pFile)

         testCase "Source files are parsed correctly" <| fun _ ->
            let pFile = makeTempProjFile projectFileText
            let pf = ProjectFile.loadFromFile pFile
            let sfNames = ProjectFile.getSourceFiles pf |> List.map (fun x -> x.ShortName)
            File.Delete(pFile)            
            Expect.equal sfNames ["File1.fs"; "File2.fs"; "File3.fs"] "File names are correct"

         testCase "Source file order is persisted to disk correctly" <| fun _ ->
            let pFile = makeTempProjFile projectFileText
            let pf = ProjectFile.loadFromFile pFile
            let sfsRev = ProjectFile.getSourceFiles pf |> List.rev
            ProjectFile.updateProjectFile sfsRev pf |> ignore

            let pf2 = ProjectFile.loadFromFile pFile
            let sfs2 = ProjectFile.getSourceFiles pf2 |> List.map (fun x -> x.ShortName)
            File.Delete(pFile)            
            Expect.equal sfs2 ["File3.fs"; "File2.fs"; "File1.fs"] "New file order is persisted"
     ]