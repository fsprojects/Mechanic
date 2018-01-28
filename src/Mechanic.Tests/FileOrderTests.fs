module Tests.FileOrder

open Expecto
open Mechanic
open Mechanic.GraphAlg
open System.IO

let makeTempProject sources = 
    let projectFileText files = 
        let items = files |> List.map (sprintf """<Compile Include="%s" />""") |> String.concat System.Environment.NewLine
        """<?xml version="1.0" encoding="utf-8"?>
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>netstandard2.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
        """ + items + """
  </ItemGroup>
</Project>
"""
    let tempPath = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
    Directory.CreateDirectory tempPath |> ignore
    let pf = Path.Combine(tempPath, "TestProject.fsproj")
    let createSourceFile i source =
        let f1 = Path.Combine(tempPath, sprintf "TestFile%i.fs" i)
        File.WriteAllText(f1, source)
        f1
    let files = sources |> List.mapi createSourceFile
    File.WriteAllText(pf, (projectFileText files))
    tempPath, pf, files

let expectOrder sources =
    let (_, _, files) = makeTempProject sources
    Expect.equal (SymbolGraph.solveOrder files) (TopologicalOrder files) "Wrong order of files"

let checkCycle sources =
    let (_, _, files) = makeTempProject sources
    match SymbolGraph.solveOrder files with
    | Cycle _ -> true
    | _ -> false

let expectCycle sources =
    checkCycle sources 
    |> fun x -> Expect.isTrue x "Dependency cycle expected"

let expectNotCycle sources =
    checkCycle sources 
    |> fun x -> Expect.isFalse x "Dependency cycle not expected"

let expectDependency sources expectedDeps =
    let (_, _, files) = makeTempProject sources
    let deps = Mechanic.SymbolGraph.getDependencies files
    Expect.sequenceEqual 
        (deps |> List.map (fun (a,b,_) -> a,b) |> List.sort) 
        (expectedDeps |> List.map (fun (i,j) -> List.item (i-1) files, List.item (j-1) files) |> List.sort)
        "Dependency differs"

[<Tests>]
 let tests =
    testList "FileOrder" [
        test "file order test 1" {
            let source1 = """module Test1
        let x = 42
        """
            let source2 = """module Test2
        open Test1
        let y = x
        """
            expectDependency [source1; source2] [1,2]
        }

        test "file order test 2" {
            let source1 = """module Test1
        let x = 42
        """
            let source2 = """module Test2
        let y = Test1.x
        """
            expectDependency [source1; source2] [1,2]
        }

        test "file order test 3" {
            let source1 = """module Test1
        module M =
            let x = 42
        """
            let source2 = """module Test2
        open Test1
        let y = M.x
        """
            expectDependency [source1; source2] [1,2]
        }

        test "file order test 4" {
            let source1 = """module Test1
        module M =
            let x = 42
        """
            let source2 = """module Test2
        open Test1.M
        let y = x
        """
            expectDependency [source1; source2] [1,2]
        }        

        test "file order test 5" {
            let source1 = """module Test1
        module M =
            let x = 42
        """
            let source2 = """module Test2
        let y = Test1.M.x
        """
            expectDependency [source1; source2] [1,2]
        }

        test "file order test 6" {
            let source1 = """module Test1
        module M =
            let x = 42
        """
            let source2 = """module Test2
        open Test1.M
        let y = M.x
        """
            expectDependency [source1; source2] [1,2]
        }        

        test "file order test 7" {
            let source1 = """module Test.M
        let x = 42
        """
            let source2 = """module Test.M2
        let y = M.x
        """
            expectDependency [source1; source2] [1,2]
        }        

        test "file order test 8" {
            let source1 = """namespace Test
        module M =
            let x = 42
        """
            let source2 = """namespace Test
        module M2 =
            let y = M.x
        """
            expectDependency [source1; source2] [1,2]
        }        

        test "file order test cycle" {
            let source1 = """module Test1
        let x = Test2.y
        """
            let source2 = """module Test2
        let y = Test1.x
        """
            expectCycle [source1; source2]
        }

        test "file order inner module test 1" {
            let source1 = """module Test1
        let x = 1
        """
            let source2 = """module Test2
        let x = 1
        """
            let source3 = """module Test3
        open Test1
        module M1 =
            open Test2
        module M2 =
            let y = x
        """
            expectDependency [source1; source2; source3] [1,3]
        }

        test "file order inner module test 2" {
            let source1 = """module Test1
        let x = 1
        """
            let source2 = """module Test2
        let x = 1
        """
            let source3 = """module Test3
        open Test1
        module M1 =
            open Test2
            let y = x
        """
            expectDependency [source1; source2; source3] [2,3]
        }

        test "file order inner module test 3" {
            let source1 = """module Test1
        let x = 1
        """
            let source2 = """module Test2
        let x = 1
        """
            let source3 = """module Test3
        module M1 =
            open Test1
            open Test2
            let y = x
        """
            expectDependency [source1; source2; source3] [2,3]
        }
    ]