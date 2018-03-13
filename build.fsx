open System.IO
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
open Fake.ReleaseNotesHelper

// Helpers and settings that figure themselves out

let projectsPattern = "src/**/*.fsproj"
let testProjectsPattern = "src/**/*Tests.fsproj"

let srcProjects = !! projectsPattern -- testProjectsPattern
let testProjects = !! testProjectsPattern

let dotnetCliVersion = "2.1.4"
let mutable dotnetCliPath = "dotnet"
let installDotNet _ = dotnetCliPath <- DotNetCli.InstallDotNetSDK dotnetCliVersion

// Read additional information from the release notes document
let releaseNotes = File.ReadAllLines "RELEASE_NOTES.md"

let releaseNotesData =
    releaseNotes
    |> parseAllReleaseNotes

let release = List.head releaseNotesData

let runDotNet args =
    let proc (info : ProcessStartInfo) =
        info.FileName <- dotnetCliPath
        info.WorkingDirectory <- "."
        info.Arguments <- args

    let result = ProcessHelper.ExecProcess proc TimeSpan.MaxValue
    if result <> 0 then failwithf "dotnet %s failed" args

// Build target implementations

let clean _ = !! "src/**/bin"++"**/obj" |> CleanDirs

let pokeVersion oldVersion newVersion project =
    if Fake.Core.Xml.Read false project "" "" "/Project/PropertyGroup/PackageVersion" |> Seq.exists ((=) oldVersion) then 
        Fake.Core.Xml.PokeInnerText project "Project/PropertyGroup/PackageVersion" newVersion

let setVersion _ =
    srcProjects |> Seq.iter (pokeVersion "0.0.0" release.NugetVersion)
    Target.ActivateFinal "ResetVersion"

let resetVersion _ = srcProjects |> Seq.iter (pokeVersion release.NugetVersion "0.0.0")

let build _ = DotNetCli.Build (fun c -> 
    { c with 
        ToolPath = dotnetCliPath
        Configuration = "debug" 
        WorkingDir = "src"} )

let test _ = testProjects |> Seq.map (sprintf "run -p \"%s\"") |> Seq.iter runDotNet

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
