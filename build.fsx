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

// Helpers and settings that figure themselves out

let projectsPattern = "**/*.fsproj"
let testProjectsPattern = "**/*Tests.fsproj"
let projects = !!projectsPattern
let srcProjects = !!projectsPattern -- testProjectsPattern
let testProjects = !!testProjectsPattern

let dotnetCliVersion = "2.1.3"
let mutable dotnetCliPath = "dotnet"
let installDotNet _ = dotnetCliPath <- DotNetCli.InstallDotNetSDK dotnetCliVersion

let gitVersionPath = !!"packages/**/GitVersion.exe" |> Seq.head
let version =
  let gitVersion = Fake.GitVersionHelper.GitVersion (fun ps -> { ps with ToolPath = gitVersionPath })
  if Fake.EnvironmentHelper.getEnvironmentVarAsBool "APPVEYOR"
  then
    let version = { gitVersion with BuildMetaData = Fake.AppVeyor.AppVeyorEnvironment.BuildNumber }
    Fake.AppVeyor.UpdateBuildVersion version.InformationalVersion
    version
  elif Fake.EnvironmentHelper.getEnvironmentVarAsBool "TRAVIS"
  then
    let version = { gitVersion with BuildMetaData = Fake.EnvironmentHelper.environVar "TRAVIS_JOB_NUMBER" }
    version
  else
    { gitVersion with BuildMetaData = "local" }

let runDotNet args =
  let proc (info : ProcessStartInfo) =
    info.FileName <- dotnetCliPath
    info.WorkingDirectory <- "."
    info.Arguments <- args

  let result = ProcessHelper.ExecProcess proc TimeSpan.MaxValue
  if result <> 0 then failwithf "dotnet %s failed" args

// Build target implementations

let clean _ = !!"**/bin"++"**/obj" |> CleanDirs

let pokeVersion oldVersion newVersion project =
  if Fake.Core.Xml.Read false project "" "" "/Project/PropertyGroup/PackageVersion" |> Seq.exists ((=) oldVersion)
  then Fake.Core.Xml.PokeInnerText project "Project/PropertyGroup/PackageVersion" newVersion


let setVersion _ =
  srcProjects |> Seq.iter (pokeVersion "0.0.0" version.NuGetVersion)
  Target.ActivateFinal "ResetVersion"

let resetVersion _ = srcProjects |> Seq.iter (pokeVersion version.NuGetVersion "0.0.0")

let build _ = DotNetCli.Build (fun c -> 
  { c with 
      ToolPath = dotnetCliPath
      Configuration = "debug" } )

let test _ = testProjects |> Seq.map (sprintf "test \"%s\" --no-build") |> Seq.iter runDotNet

// Build target definitions

Target.Create "Clean" clean
Target.Create "InstallDotNetCore" installDotNet
Target.Create "Build" build
Target.Create "Test" test
Target.Create "SetVersion" setVersion
Target.CreateFinal "ResetVersion" resetVersion

"Clean"
  ==> "InstallDotNetCore"
  ==> "SetVersion"
  ==> "Build"
  ==> "Test"

Target.RunOrDefault "Test"
