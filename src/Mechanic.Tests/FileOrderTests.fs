module Tests.FileOrder

open Expecto
open Mechanic
open Mechanic.GraphAlg
open System.IO

let makeTempProject source1 source2 = 
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
    File.WriteAllText(f1, source1)
    File.WriteAllText(f2, source2)
    tempPath, pf, f1, f2

let checkOrder source1 source2 =
    let (_, _, f2, f1) = makeTempProject source2 source1
    Expect.equal (SymbolGraph.solveOrder [f1; f2]) (TopologicalOrder [f1; f2]) "Wrong order of files"
    Expect.equal (SymbolGraph.solveOrder [f2; f1]) (TopologicalOrder [f1; f2]) "Wrong order of files"

let checkCycle source1 source2 =
    let (_, _, f1, f2) = makeTempProject source1 source2
    match SymbolGraph.solveOrder [f2; f1] with
    | Cycle _ -> true
    | _ -> false
    |> fun x -> Expect.isTrue x "Dependency cycle expected"


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
            checkOrder source1 source2
        }

        test "file order test 2" {
            let source1 = """module Test1
        let x = 42
        """
            let source2 = """module Test2
        let y = Test1.x
        """
            checkOrder source1 source2
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
            checkOrder source1 source2
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
            checkOrder source1 source2
        }        

        test "file order test 5" {
            let source1 = """module Test1
        module M =
            let x = 42
        """
            let source2 = """module Test2
        let y = Test1.M.x
        """
            checkOrder source1 source2
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
            checkOrder source1 source2
        }        

        test "file order test 7" {
            let source1 = """module Test.M
        let x = 42
        """
            let source2 = """module Test.M2
        let y = M.x
        """
            checkOrder source1 source2
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
            checkOrder source1 source2
        }        

        test "file order test cycle" {
            let source1 = """module Test1
        let x = Test2.y
        """
            let source2 = """module Test2
        let y = Test1.x
        """
            checkCycle source1 source2
        }

        test "file order inner module test 1" {
            let source1 = """module Test1
        module M1 =
            open Test2.M2
        module M2 =
            let x = y
        """
            let source2 = """module Test2
        module M1 =
            open Test1.M1
        module M2 =
            let y = x
        """
            checkCycle source1 source2
        }
    ]