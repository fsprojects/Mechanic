(* -- Fake Dependencies paket-inline
source https://api.nuget.org/v3/index.json

nuget Fake.Core.Target prerelease
nuget FSharp.Core prerelease
-- Fake Dependencies -- *)

#r @"packages/build/FAKE/tools/FakeLib.dll"

open Fake.Core
open Fake.Core.TargetOperators

let clean _ = printfn "--- Cleaning stuff ---"
let build _ = printfn "--- Building the app ---"
let test _ = printfn "--- Running tests ---"

Target.Create "Clean" clean
Target.Create "Build" build
Target.Create "Test" test


"Clean"
  ==> "Build"
  ==> "Test"

Target.RunOrDefault "Test"
