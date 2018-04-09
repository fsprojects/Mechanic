/// Copied from https://github.com/fable-compiler/Fable/blob/f8457dc6ff3208af85166a14da0feb520927bc55/src/dotnet/dotnet-fable/ProjectCoreCracker.fs
/// Verbose SDK changes based on https://github.com/fsharp/FsAutoComplete/blob/master/src/FsAutoComplete.Core/ProjectCrackerDotnetSdk.fs
module Mechanic.ProjectCracker

open System
open System.IO

open Microsoft.FSharp.Compiler.SourceCodeServices

module ProjectRecognizer =
    // This part is from https://github.com/fsharp/FsAutoComplete/blob/ad1b81ae50ad08ae67d856aea602da97dde4186f/src/FsAutoComplete.Core/ProjectCrackerTypes.fs#L67
    let (|NetCoreProjectJson|NetCoreSdk|Net45|Unsupported|) file =
        let rec getProjectType (sr:StreamReader) limit =
            let isNetCore (line:string) = line.ToLower().Contains("sdk=")
            if limit = 0 then
                Unsupported // unsupported project type
            else
                let line = sr.ReadLine()
                if not <| line.Contains("ToolsVersion") && not <| line.Contains("Sdk=") then
                    getProjectType sr (limit-1)
                else // both net45 and preview3-5 have 'ToolsVersion', > 5 has 'Sdk'
                    if isNetCore line then NetCoreSdk else Net45
        if Path.GetExtension file = ".json" then
            NetCoreProjectJson // dotnet core preview 2 or earlier
        else
            use sr = File.OpenText(file)
            getProjectType sr 3

module Environment =
  let msbuild = Fake.DotNet.MSBuild.msBuildExe

module MSBuildPrj = Dotnet.ProjInfo.Inspect

type private ProjectParsingSdk = DotnetSdk | VerboseSdk

type NavigateProjectSM =
    | NoCrossTargeting of NoCrossTargetingData
    | CrossTargeting of string list
and NoCrossTargetingData = { FscArgs: string list; P2PRefs: MSBuildPrj.ResolvedP2PRefsInfo list; Properties: Map<string,string> }

let private runProcess (workingDir: string) (exePath: string) (args: string) =
    let psi = System.Diagnostics.ProcessStartInfo()
    psi.FileName <- exePath
    psi.WorkingDirectory <- workingDir
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.Arguments <- args
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false

    //Some env var like `MSBUILD_EXE_PATH` override the msbuild used.
    //The dotnet cli (`dotnet`) set these when calling child processes, and
    //is wrong because these override some properties of the called msbuild
    let msbuildEnvVars =
        psi.Environment.Keys
        |> Seq.filter (fun s -> s.StartsWith("msbuild", StringComparison.OrdinalIgnoreCase))
        |> Seq.toList
    for msbuildEnvVar in msbuildEnvVars do
       psi.Environment.Remove(msbuildEnvVar) |> ignore

    use p = new System.Diagnostics.Process()
    p.StartInfo <- psi

    let sbOut = System.Text.StringBuilder()
    p.OutputDataReceived.Add(fun ea -> sbOut.AppendLine(ea.Data) |> ignore)

    let sbErr = System.Text.StringBuilder()
    p.ErrorDataReceived.Add(fun ea -> sbErr.AppendLine(ea.Data) |> ignore)

    p.Start() |> ignore
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()

    let exitCode = p.ExitCode
    exitCode, (workingDir, exePath, args)

let private msbuildPropBool (s: string) =
  match s.Trim() with
  | "" -> None
  | MSBuildPrj.MSBuild.ConditionEquals "True" -> Some true
  | _ -> Some false

let private msbuildPropStringList (s: string) =
  match s.Trim() with
  | "" -> []
  | MSBuildPrj.MSBuild.StringList list  -> list
  | _ -> []

let rec private projInfo additionalMSBuildProps file =
  let projType =
      match file with
      | ProjectRecognizer.NetCoreSdk -> ProjectParsingSdk.DotnetSdk
      | ProjectRecognizer.Net45 -> ProjectParsingSdk.VerboseSdk
      | _ -> failwithf "Unsupported type of project %s" file
  let projDir = Path.GetDirectoryName file
  let projectAssetsJsonPath = Path.Combine(projDir, "obj", "project.assets.json")
  if projType = DotnetSdk && not(File.Exists(projectAssetsJsonPath)) then
     failwithf "Cannot find restored info for project %s" file

  let getFscArgs = 
    match projType with
    | DotnetSdk -> Dotnet.ProjInfo.Inspect.getFscArgs
    | VerboseSdk -> 
        let asFscArgs props =
            let fsc = Microsoft.FSharp.Build.Fsc()
            Dotnet.ProjInfo.FakeMsbuildTasks.getResponseFileFromTask props fsc
        Dotnet.ProjInfo.Inspect.getFscArgsOldSdk (asFscArgs >> Ok)
  let getP2PRefs = Dotnet.ProjInfo.Inspect.getResolvedP2PRefs
  let gp () = Dotnet.ProjInfo.Inspect.getProperties (["TargetPath"; "IsCrossTargetingBuild"; "TargetFrameworks"; "TargetFramework"])

  let results =
      let runCmd exePath args = runProcess projDir exePath (args |> String.concat " ")

      let msbuildExec =
        let msbuildPath =
            match projType with
            | ProjectParsingSdk.DotnetSdk ->
                Dotnet.ProjInfo.Inspect.MSBuildExePath.DotnetMsbuild "dotnet"
            | ProjectParsingSdk.VerboseSdk ->
                Dotnet.ProjInfo.Inspect.MSBuildExePath.Path Environment.msbuild
        Dotnet.ProjInfo.Inspect.msbuild msbuildPath runCmd
      let log = ignore

      let additionalArgs = additionalMSBuildProps |> List.map (Dotnet.ProjInfo.Inspect.MSBuild.MSbuildCli.Property)

      let inspect =
        match projType with
        | ProjectParsingSdk.DotnetSdk ->
            Dotnet.ProjInfo.Inspect.getProjectInfos
        | ProjectParsingSdk.VerboseSdk ->
            Dotnet.ProjInfo.Inspect.getProjectInfosOldSdk

      file
      |> inspect log msbuildExec [getFscArgs; getP2PRefs; gp] additionalArgs

  let todo =
      match results with
      | Result.Ok [getFscArgsResult; getP2PRefsResult; gpResult] ->
          match getFscArgsResult, getP2PRefsResult, gpResult with
          | Result.Error(MSBuildPrj.MSBuildSkippedTarget), Result.Error(MSBuildPrj.MSBuildSkippedTarget), Result.Ok(MSBuildPrj.GetResult.Properties props) ->
              // Projects with multiple target frameworks, fails if the target framework is not choosen
              let prop key = props |> Map.ofList |> Map.tryFind key

              match prop "IsCrossTargetingBuild", prop "TargetFrameworks" with
              | Some (MSBuildPrj.MSBuild.ConditionEquals "true"), Some (MSBuildPrj.MSBuild.StringList tfms) ->
                  CrossTargeting tfms
              | _ ->
                  failwithf "error getting msbuild info: some targets skipped, found props: %A" props
          | Result.Ok(MSBuildPrj.GetResult.FscArgs fa), Result.Ok(MSBuildPrj.GetResult.ResolvedP2PRefs p2p), Result.Ok(MSBuildPrj.GetResult.Properties p) ->
              NoCrossTargeting { FscArgs = fa; P2PRefs = p2p; Properties = p |> Map.ofList }
          | r ->
              failwithf "error getting msbuild info: %A" r
      | Result.Ok r ->
          failwithf "error getting msbuild info: internal error, more info returned than expected %A" r
      | Result.Error r ->
          match r with
          | Dotnet.ProjInfo.Inspect.GetProjectInfoErrors.MSBuildSkippedTarget ->
              failwithf "Unexpected MSBuild result, all targets skipped"
          | Dotnet.ProjInfo.Inspect.GetProjectInfoErrors.UnexpectedMSBuildResult(r) ->
              failwithf "Unexpected MSBuild result %s" r
          | Dotnet.ProjInfo.Inspect.GetProjectInfoErrors.MSBuildFailed(exitCode, (workDir, exePath, args)) ->
              [ sprintf "MSBuild failed with exitCode %i" exitCode
                sprintf "Working Directory: '%s'" workDir
                sprintf "Exe Path: '%s'" exePath
                sprintf "Args: '%s'" args ]
              |> String.concat " "
              |> failwith

  match todo with
  | CrossTargeting (tfm :: _) ->
      // Atm setting a preferenece is not supported in FSAC
      // As workaround, lets choose the first of the target frameworks and use that
      file |> projInfo ["TargetFramework", tfm]
  | CrossTargeting [] ->
      failwithf "Unexpected, found cross targeting but empty target frameworks list"
  | NoCrossTargeting { FscArgs = rsp; P2PRefs = p2ps; Properties = props } ->

      //TODO cache projects info of p2p ref
    //   let p2pProjects =
    //       p2ps
    //       // do not follow others lang project, is not supported by FCS anyway
    //       |> List.filter (fun p2p -> p2p.ProjectReferenceFullPath.ToLower().EndsWith(".fsproj"))
    //       |> List.map (fun p2p -> p2p.ProjectReferenceFullPath |> projInfo ["TargetFramework", p2p.TargetFramework] )

      //let tar =
           //match props |> Map.tryFind "TargetPath" with
           //| Some t -> t
           //| None -> failwith "error, 'TargetPath' property not found"

      let compileFilesToAbsolutePath (f: string) =
          if f.EndsWith(".fs") then
              if Path.IsPathRooted f then f else Path.Combine(projDir, f)
          else
              f

      let projOptions =
          {
              ProjectFileName = file
              SourceFiles = [||]
              OtherOptions = rsp |> List.map compileFilesToAbsolutePath |> Array.ofList
              ReferencedProjects = [||] //p2pProjects |> Array.ofList
              IsIncompleteTypeCheckEnvironment = false
              UseScriptResolutionRules = false
              LoadTime = DateTime.Now
              UnresolvedReferences = None;
              OriginalLoadReferences = []
              ExtraProjectInfo = None
              Stamp = None
          }
      let projRefs = p2ps |> List.map (fun p2p -> p2p.ProjectReferenceFullPath)
      projOptions, projRefs, props

let GetProjectOptionsFromProjectFile (file : string) =
  projInfo [] file
