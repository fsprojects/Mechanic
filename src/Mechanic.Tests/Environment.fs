module Mechanic.Tests.Environment

open Expecto
open Mechanic.Environment


[<Tests>]
 let tests =
     testList "Project file tests" [
 
         testCase "Paths and filename are combined correctly" <| fun _ ->
            let path = 
                join [programFiles; SystemLibLocation]
                |> pathCombine (toDll "TestLib")
            let expected = @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETCore\v4.5.1\TestLib.dll"
            Expect.equal path expected "Paths combined correctly"
     ]