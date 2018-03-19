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

let createNewSourceFile (newShortName: string) (existingSourceFile: SourceFile): SourceFile =
    let replacedFullName = existingSourceFile.FullName.Replace (existingSourceFile.ShortName, newShortName)
    let newNode = existingSourceFile.XmlNode.Clone ()
    let attr = newNode.Attributes.ItemOf ("Include")
    attr.Value <- newShortName
    newNode.Attributes.SetNamedItem (attr) |> ignore
    { FullName = replacedFullName; ShortName = newShortName; XmlNode = newNode }


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

         testCase "Source files full path are parsed correctly" <| fun _ ->
            let pFile = makeTempProjFile projectFileText
            let pDir = FileInfo(pFile).Directory.FullName
            let pf = ProjectFile.loadFromFile pFile
            let sfNames = ProjectFile.getSourceFiles pf |> List.map (fun x -> x.FullName)
            File.Delete(pFile)
            let expectedPaths = ["File1.fs"; "File2.fs"; "File3.fs"] |> List.map (fun x -> Path.Combine(pDir, x))        
            Expect.equal sfNames expectedPaths "File paths are correct"

         testCase "Source file order is persisted to disk correctly" <| fun _ ->
            let pFile = makeTempProjFile projectFileText
            let pf = ProjectFile.loadFromFile pFile
            let sfs = ProjectFile.getSourceFiles pf
            let sfsRev = sfs |> List.rev
            ProjectFile.updateProjectFile sfsRev pf |> ignore

            let pf2 = ProjectFile.loadFromFile pFile
            let sfs2 = ProjectFile.getSourceFiles pf2 |> List.map (fun x -> x.ShortName)
            File.Delete(pFile) 
            Expect.equal sfs2 ["File3.fs"; "File2.fs"; "File1.fs"] "New file order was persisted"
            Expect.notEqual sfs2 (sfs |> List.map (fun x -> x.ShortName)) "Old file order doesn't apply anymore"
         
         testCase "Source files with new item are persisted correctly" <| fun _ ->
            let pFile = makeTempProjFile projectFileText

            let pf = ProjectFile.loadFromFile pFile
            let existingSourceFiles = ProjectFile.getSourceFiles pf
            let newSourceFile = existingSourceFiles
                                    |> List.head
                                    |> createNewSourceFile "File4.fs"
            let sourceFiles = existingSourceFiles @ [newSourceFile]
            let initialOuterXml = pf.Document.OuterXml
            ProjectFile.updateProjectFile sourceFiles pf |> ignore

            let pf' = ProjectFile.loadFromFile pFile
            let newSourceFiles = ProjectFile.getSourceFiles pf'
            let newSourceFilesShortNames = (newSourceFiles |> List.map (fun s -> s.ShortName))

            File.Delete (pFile)
            Expect.notEqual pf'.Document.OuterXml initialOuterXml "Project file was successfully updated"
            Expect.notEqual ["File1.fs"; "File2.fs"; "File3.fs"] newSourceFilesShortNames "Project file does not contain old list of source files"
            Expect.equal ["File1.fs"; "File2.fs"; "File3.fs"; "File4.fs"] newSourceFilesShortNames "Project file contains new list of source files"
        ]