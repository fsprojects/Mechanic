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

let dotnetCliVersion = DotNetCli.getVersion()
let mutable dotnetCliPath = "dotnet"
let installDotNet _ = dotnetCliPath <- DotNetCli.InstallDotNetSDK dotnetCliVersion

let clean _ =
  !!"src/**/bin"
  ++"src/**/obj"
  |> CleanDirs

let runDotNet args =
  let proc (info : ProcessStartInfo) =
    info.FileName <- dotnetCliPath
    info.WorkingDirectory <- "."
    info.Arguments <- args

  printfn "Calling dotnet %s" args
  let result = ExecProcess proc TimeSpan.MaxValue

  if result <> 0 then failwithf "dotnet %s failed" args

let build _ =
  runDotNet "build"
let test _ =
  !!"**/*Tests.fsproj"
  |> Seq.map (sprintf "test \"%s\" --no-build")
  |> Seq.iter runDotNet

Target.Create "Clean" clean
Target.Create "InstallDotNetCore" installDotNet
Target.Create "Build" build
Target.Create "Test" test


"Clean"
  ==> "InstallDotNetCore"
  ==> "Build"
  ==> "Test"

Target.RunOrDefault "Test"
