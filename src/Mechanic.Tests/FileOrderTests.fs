module Tests.FileOrder

open Expecto
open Mechanic
open Mechanic.GraphAlg
open System.IO

let options = { LogOutput = Options.LogOutput.Default }

let makeTempProjectFromTemplate templatePath sources = 
    let projectFileText files = 
        let items = files |> List.map (sprintf """<Compile Include="%s" />""") |> String.concat System.Environment.NewLine
        let template = File.ReadAllText (Path.Combine("templates", templatePath))
        template.Replace("@@@items@@@", items)

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

let withProjectTemplates sources f =
    let templates = [
        "newSdk.fsproj.template"
        "verboseSdk.fsproj.template"
    ]
    templates |> Seq.iter (fun t -> makeTempProjectFromTemplate t sources |> f)

let expectOrder sources =
    withProjectTemplates sources <| fun (_, projFile, files) ->
        Expect.equal (SymbolGraph.solveOrder options id (Some projFile) files) (TopologicalOrder files) "Wrong order of files"

let checkCycle sources expectF =
    withProjectTemplates sources <| fun (_, projFile, files) ->
        match SymbolGraph.solveOrder options id (Some projFile) files with
        | Cycle _ -> expectF true
        | _ -> expectF false

let expectCycle sources =
    checkCycle sources (fun x -> Expect.isTrue x "Dependency cycle expected")

let expectNotCycle sources =
    checkCycle sources (fun x -> Expect.isFalse x "Dependency cycle not expected")

let expectDependencyHelper useExternalDeps sources expectedDeps =
    withProjectTemplates sources <| fun (_, projFile, files) ->
        let deps = Mechanic.SymbolGraph.getDependencies options files (if useExternalDeps then Some projFile else None)
        Expect.sequenceEqual 
            (deps |> List.map (fun (a,b,_) -> a,b) |> List.sort) 
            (expectedDeps |> List.map (fun (i,j) -> List.item (i-1) files, List.item (j-1) files) |> List.sort)
            "Dependency differs"

let expectDependencyWithExternalDefs sources expectedDeps = expectDependencyHelper true sources expectedDeps
let expectDependency sources expectedDeps = expectDependencyHelper false sources expectedDeps

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

        test "file order test let static" {
            let source1 = """namespace Test
        type T() =
            static let x = 42
        """
            let source2 = """namespace Test
        module M2 =
            let y = T.x
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

        test "file order inner module namespace test 1" {
            let source1 = """namespace N
        module Test1 =
            let x = 1
        """
            let source2 = """namespace N
        module M1 =
            open Test1
            let y = x
        """
            expectDependency [source1; source2] [1,2]
        }

        test "file order inner module namespace test 2" {
            let source1 = """module N.M1
        module Test1 =
            let x = 1
        """
            let source2 = """module N.M2
        module Test2 =
            open M1.Test1
            let y = x
        """
            expectDependency [source1; source2] [1,2]
        }

        test "file order inner module namespace test 3" {
            let source1 = """namespace N
        module Test1 =
            let x = 1
        """
            let source2 = """namespace N2
        module M1 =
            open N.Test1
            let y = x
        """
            expectDependency [source1; source2] [1,2]
        }

        test "record typed" {
            let source1 = """module M
            type R = { x: int }
        """
            let source2 = """module M2
            open M
            let y = { x = 42 } : R
        """
            expectDependency [source1; source2] [1,2]
        }

        test "union typed" {
            let source1 = """module M
            type DU = A | B
        """
            let source2 = """module M2
            open M
            let y = A : DU
        """
            expectDependency [source1; source2] [1,2]
        }

        test "class type" {
            let source1 = """module M
            type C() = class end
        """
            let source2 = """module M2
            open M
            let y = C()
        """
            expectDependency [source1; source2] [1,2]
        }

        test "record field" {
            let source1 = """module M
            type R = { x: int }
        """
            let source2 = """module M2
            open M
            let y = { x = 42 }
        """
            expectDependency [source1; source2] [1,2]
        }

        test "union case" {
            let source1 = """module M
            type DU = A | B
        """
            let source2 = """module M2
            open M
            let y = A
        """
            expectDependency [source1; source2] [1,2]
        }

        test "type in record" {
            let source1 = """module M
            type DU = A | B
        """
            let source2 = """module M2
            open M
            type R = { x: DU }
        """
            expectDependency [source1; source2] [1,2]
        }

        test "type in union" {
            let source1 = """module M
            type R = { x: DU }
        """
            let source2 = """module M2
            open M
            type DU = A of R | B
        """
            expectDependency [source1; source2] [1,2]
        }

        test "type in params" {
            let source1 = """module M
            type R = { x: DU }
        """
            let source2 = """module M2
            open M
            let f (r : R) = r.x
        """
            expectDependency [source1; source2] [1,2]
        }

        test "type abbrev" {
            let source1 = """module M
            type R = { x: DU }
        """
            let source2 = """module M2
            open M
            let f r = (r : R).x
        """
            expectDependency [source1; source2] [1,2]
        }

        test "type in class params" {
            let source1 = """module M
            type R = { x: DU }
        """
            let source2 = """module M2
            open M
            type C(r : R) = class end
        """
            expectDependency [source1; source2] [1,2]
        }

        test "type in record - tuple" {
            let source1 = """module M
            type DU = A | B
        """
            let source2 = """module M2
            open M
            type R = { x: DU * DU }
        """
            expectDependency [source1; source2] [1,2]
        }

        test "type in record - fun" {
            let source1 = """module M
            type DU = A | B
        """
            let source2 = """module M2
            open M
            type R = { x: DU -> () }
        """
            expectDependency [source1; source2] [1,2]
        }

        test "type in record - app" {
            let source1 = """module M
            type DU = A | B
        """
            let source2 = """module M2
            open M
            type R = { x: seq<DU> }
        """
            expectDependency [source1; source2] [1,2]
        }
        
        test "type in multi params" {
            let source1 = """module M
            type R = { x: DU }
        """
            let source2 = """module M2
            open M
            let f (r : R, r2 : R) = r.x
        """
            expectDependency [source1; source2] [1,2]
        }

        test "let-type clash" {
            let source1 = """module M
            type DU = A | B
        """
            let source2 = """module M2
            let DU = 42
        """
            let source3 = """module M3
            open M
            open M2
            let y = A : DU
        """
            expectDependency [source1; source2; source3] [1,3]
        }

        test "let-field clash" {
            let source1 = """module M
            type R = { x: int }
        """
            let source2 = """module M2
            let x = 42
        """
            let source3 = """module M3
            open M
            open M2
            let y = x
        """
            expectDependency [source1; source2; source3] [2,3]
        }

        test "file order shadowing test 1" {
            let source1 = """module N.M1
        type R = { x: int }
        """
            let source2 = """module N.M2
        type R = { x: int }
        let y = {x=1}
        """
            expectDependency [source1; source2] []
        }

        test "file order shadowing test 2" {
            let source1 = """namespace N
        type R = { x: int }
        """
            let source2 = """namespace N
        type R = { x: int }
        let y = {x=1}
        """
            expectDependency [source1; source2] []
        }

        test "file order shadowing test 3" {
            let source1 = """namespace N
        type R = { x: int }
        """
            let source2 = """module N.M2
        type R = { x: int }
        let y = {x=1}
        """
            expectDependency [source1; source2] []
        }

        test "file order shadowing test 4" {
            let source1 = """namespace N
        type R = { x: int }
        """
            let source2 = """module N.M2
        module M3 =        
            type R = { x: int }
            let y = {x=1}
        """
            expectDependency [source1; source2] []
        }

        test "file order shadowing test 5" {
            let source1 = """module N.M1
        type R = { x: int }
        """
            let source2 = """namespace N
        open M1
        type R = { x: int }
        let y = {x=1}
        """
            expectDependency [source1; source2] []
        }

        test "file order shadowing test 6" {
            let source1 = """namespace N
        type R = { x: int }
        """
            let source2 = """module N.M2
        type R = { x: int }
        """
            let source3 = """module N.M3
        open N.M2
        let y = {x=1}
        """
            expectDependency [source1; source2; source3] [2,3]
        }

        test "let params shadowing" {
            let source1 = """namespace N
            let x = 42
        """
            let source2 = """namespace N
            let f x =
                let y = x
                y
        """
            expectDependency [source1; source2] []
        }
        
        test "class params shadowing" {
            let source1 = """namespace N
            let x = 42
        """
            let source2 = """namespace N
            type C(x: int) =
                let y = x
        """
            expectDependency [source1; source2] []
        }

        test "let params not def" {
            let source1 = """namespace N
            let f x = ()
        """
            let source2 = """namespace N
            let y = x
        """
            expectDependency [source1; source2] []
        }

        test "let in let shadowing" {
            let source1 = """namespace N
            let x = 42
        """
            let source2 = """namespace N
            let y = 
                let x = 42
                x
        """
            expectDependency [source1; source2] []
        }
        
        test "let in let limited scope" {
            let source1 = """namespace N
            let x = 42
        """
            let source2 = """namespace N
            let y = 
                let x = 42
                x

            let z = x
        """
            expectDependency [source1; source2] [1,2]
        }

        test "let in let limited scope 2" {
            let source1 = """namespace N
            let x = 42
        """
            let source2 = """namespace N
            let y = 
                let w = x
                let x = 42
                x
        """
            expectDependency [source1; source2] [1,2]
        }

        test "let in let not def" {
            let source1 = """namespace N
            let y = 
                let x = 42
                x
        """
            let source2 = """namespace N
            let z = x
        """
            expectDependency [source1; source2] []
        }
        
        test "class params not def" {
            let source1 = """namespace N
            type C(x: int) = class end
        """
            let source2 = """namespace N
            let y = x
        """
            expectDependency [source1; source2] []
        }

        test "union private" {
            let source1 = """module M
            type DU = A | B
        """
            let source2 = """module M2
            type DU = private A | B
        """
            let source3 = """module M3
            open M
            open M2
            let y = A
        """
            expectDependency [source1; source2; source3] [1,3]
        }

        test "autoopen" {
            let source1 = """[<AutoOpen>]
            module M
            let x = 42
        """
            let source2 = """let y = x
        """
            expectDependency [source1; source2] [1,2]
        }

        test "explicit open before autoopen" {
            let source1 = """[<AutoOpen>]
            module M
            let x = 42
        """
            let source2 = """module M2
            let x = "hello"
        """
            let source3 = """open M2
                let y = x
        """
            expectDependency [source1; source2; source3] [2,3]
            
        }

        test "external deps 1" {
            let source1 = """module M
            let x = System.IO.DirectoryInfo(".")
        """
            let source2 = """[<AutoOpen>]
            module M2
            let System.IO.DirectoryInfo x = ()
        """
            expectDependencyWithExternalDefs [source1; source2] []
        }
    ]