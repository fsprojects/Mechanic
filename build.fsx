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
let installDotNet = lazy DotNet.install DotNet.Release_2_1_4

// Read additional information from the release notes document
let releaseNotes = File.ReadAllLines "RELEASE_NOTES.md"

let releaseNotesData =
    releaseNotes
    |> ReleaseNotes.parseAll

let release = List.head releaseNotesData

// Build target implementations

let clean _ = !! "src/**/bin"++"**/obj" |> Shell.CleanDirs

let pokeVersion oldVersion newVersion project =
    if Xml.read false project "" "" "/Project/PropertyGroup/PackageVersion" |> Seq.exists ((=) oldVersion) then
        Xml.pokeInnerText project "Project/PropertyGroup/PackageVersion" newVersion

let setVersion _ =
    srcProjects |> Seq.iter (pokeVersion "0.0.0" release.NugetVersion)
    Target.activateFinal "ResetVersion"

let resetVersion _ = srcProjects |> Seq.iter (pokeVersion release.NugetVersion "0.0.0")

let inline withWorkDir wd =
    DotNet.Options.lift installDotNet.Value
    >> DotNet.Options.withWorkingDirectory wd

let build _ = DotNet.build (fun c ->
    { c with
        Configuration = DotNet.Debug }
    |> withWorkDir "src") ""

let failOnError (p : ProcessResult) =
    if p.ExitCode <> 0 then failwithf "process failed with exit code %d" p.ExitCode
let test _ =
    testProjects |> Seq.map (sprintf "-p \"%s\"") |> Seq.iter (DotNet.exec installDotNet.Value "run" >> failOnError)

let releasePackage _ =
    DotNet.publish (fun c ->
    { c with
        Configuration = DotNet.Release }
    |> withWorkDir "src/Mechanic.CommandLine") ""


// Build target definitions

Target.create "Clean" clean
Target.create "InstallDotNetCore" (fun _ -> ignore installDotNet.Value)
Target.create "Build" build
Target.create "Test" test
Target.create "SetVersion" setVersion
Target.createFinal "ResetVersion" resetVersion
Target.create "Release" releasePackage

"Clean"
  ==> "InstallDotNetCore"
  ==> "SetVersion"
  ==> "Build"
  ==> "Test"
  ==> "Release"

Target.runOrDefault "Test"
