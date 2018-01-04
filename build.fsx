(* -- Fake Dependencies paket-inline
source https://api.nuget.org/v3/index.json

nuget Fake.Core.Target prerelease
nuget FSharp.Core prerelease
-- Fake Dependencies -- *)

#r @"packages/build/FAKE/tools/FakeLib.dll"
open System
open System.Diagnostics

open Fake
open Fake.Core
open Fake.Core.TargetOperators
open Fake.Core.Globbing.Operators

let mutable dotnetCliPath = "dotnet"
let dotnetCliVersion = "2.1.3"

let runDotNet args =
  let proc (info : ProcessStartInfo) =
    info.FileName <- dotnetCliPath
    info.WorkingDirectory <- "."
    info.Arguments <- args
  let result = ProcessHelper.ExecProcess proc TimeSpan.MaxValue
  if result <> 0 then failwithf "dotnet %s failed" args

Target.Create "Clean" <| fun _ ->
  !!"**/bin"++"**/obj" |> CleanDirs

Target.Create "InstallDotNet" <| fun _ ->
  dotnetCliPath <- DotNetCli.InstallDotNetSDK dotnetCliVersion

Target.Create "Build" <| fun _ ->
  runDotNet "build"

Target.Create "Test" <| fun _ ->
  !!"**/*Tests.fsproj"
  |> Seq.iter ( sprintf "run --project %s --no-build" >> runDotNet ) 

"Clean"
  ==> "InstallDotNet"
  ==> "Build"
  ==> "Test"

Target.RunOrDefault "Test"

