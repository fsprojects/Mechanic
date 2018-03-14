#load ".fake/build.fsx/intellisense.fsx"

open System.Diagnostics
open System.IO

open Fake.IO
open Fake.Core
open Fake.DotNet
open Fake.Core.TargetOperators
open Fake.IO.Globbing.Operators

// Helpers and settings that figure themselves out

let projectsPattern = "src/**/*.fsproj"
let testProjectsPattern = "src/**/*Tests.fsproj"

let srcProjects = !! projectsPattern -- testProjectsPattern
let testProjects = !! testProjectsPattern
let installDotNet _ = 
    DotNet.Install DotNet.Release_2_1_4

// Read additional information from the release notes document
let releaseNotes = File.ReadAllLines "RELEASE_NOTES.md"

let releaseNotesData =
    releaseNotes
    |> ReleaseNotes.parseAllReleaseNotes

let release = List.head releaseNotesData

// Build target implementations

let clean _ = !! "src/**/bin"++"**/obj" |> Shell.CleanDirs

let pokeVersion oldVersion newVersion project =
    if Xml.Read false project "" "" "/Project/PropertyGroup/PackageVersion" |> Seq.exists ((=) oldVersion) then
        Xml.PokeInnerText project "Project/PropertyGroup/PackageVersion" newVersion

let setVersion _ =
    srcProjects |> Seq.iter (pokeVersion "0.0.0" release.NugetVersion)
    Target.ActivateFinal "ResetVersion"

let resetVersion _ = srcProjects |> Seq.iter (pokeVersion release.NugetVersion "0.0.0")

let build _ = DotNet.Build (fun c ->
    { c with
        Configuration = DotNet.Debug
        Common = { c.Common with WorkingDirectory = "src" } } ) ""

let failOnError (p : ProcessResult) =
    if p.ExitCode <> 0 then failwithf "process failed with exit code %d" p.ExitCode
let test _ =
    testProjects |> Seq.map (sprintf "-p \"%s\"") |> Seq.iter (DotNet.Exec id "run" >> failOnError)

let releasePackage _ =
    DotNet.Publish (fun c ->
    { c with
        Configuration = DotNet.Release
        Common = { c.Common with WorkingDirectory = "src/Mechanic.CommandLine" } }) ""


// Build target definitions

Target.Create "Clean" clean
Target.Create "InstallDotNetCore" installDotNet
Target.Create "Build" build
Target.Create "Test" test
Target.Create "SetVersion" setVersion
Target.CreateFinal "ResetVersion" resetVersion
Target.Create "Release" releasePackage

"Clean"
  ==> "InstallDotNetCore"
  ==> "SetVersion"
  ==> "Build"
  ==> "Test"
  ==> "Release"

Target.RunOrDefault "Test"
