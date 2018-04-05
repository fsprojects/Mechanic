module Mechanic.Utils

let memoize (f: 'a -> 'b) =
    let cache = System.Collections.Concurrent.ConcurrentDictionary<_, _>(HashIdentity.Structural)
    fun x ->
        cache.GetOrAdd(x, lazy (f x)).Force()

module List =
    let rec internal distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec allPermutations = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (allPermutations xs)

    let rec cartesianMult = function
    | [] -> [[]]
    | xs::rest -> xs |> List.collect (fun x -> cartesianMult rest |> List.map (fun ys -> x :: ys))

    let swapPairAtIndex i xs =
        match List.skip i xs with
        | x :: y :: rest -> (List.take i xs) @ (y :: x :: rest)
        | rest -> (List.take i xs) @ rest

    let rec moveItemAtIndexBy i n xs =
        if n > 0 then moveItemAtIndexBy (i+1) (n-1) (swapPairAtIndex i xs)
        elif n < 0 && i > 0 then moveItemAtIndexBy (i-1) (n+1) (swapPairAtIndex (i-1) xs)
        else xs

module Namespace =
    let splitByDot (s:string) = 
        s.Split('.') |> Array.filter (System.String.IsNullOrEmpty >> not) |> Array.toList
        |> function | [] -> [""] | x -> x
    let joinByDot xs = xs |> List.filter (fun s -> String.length s > 0) |> String.concat "."
    let firstPart = splitByDot >> List.head
    let lastPart = splitByDot >> List.last
    let removeFirstPart s = s |> splitByDot |> (function | [] -> [] | _::xs -> xs) |> joinByDot
    let removeLastPart s = s |> splitByDot |> (fun xs -> xs |> List.take (List.length xs - 1)) |> joinByDot
    let rec getAllPrefixes = function
        | "" -> []
        | s -> let s' = removeLastPart s in s' :: getAllPrefixes s'
    let rec getAllSuffixes = function
        | "" -> []
        | s -> let s' = removeFirstPart s in s' :: getAllSuffixes s'
    let rec merge n1 n2 =
        let l1 = splitByDot n1
        let l2 = splitByDot n2
        let len1 = List.length l1
        let len2 = List.length l2
        let l = min len1 len2
        [0..l] |> List.tryFind (fun i -> 
            let l1' = l1 |> List.skip i |> List.take (len1-i)
            let l2' = l2 |> List.take (len2-i)
            if List.isEmpty l1' || List.isEmpty l2' then false else Seq.forall2 (=) l1' l2')
        |> Option.map (fun i -> l1 @ (List.skip (min len2 (len1-i)) l2))
        |> Option.defaultValue (l1 @ l2)
        |> joinByDot

module Path =
    let normalizeDirectorySeparator (path: string) =
        match System.IO.Path.DirectorySeparatorChar with
        | '\\' -> path.Replace('/', '\\')
        | '/' -> path.Replace('\\', '/')
        | _ -> path

module Shell =
    let runCmd (workingDir: string) (exePath: string) (args: string) =
        let logOut = System.Collections.Concurrent.ConcurrentQueue<string>()
        let logErr = System.Collections.Concurrent.ConcurrentQueue<string>()
        
        let runProcess () =
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
                |> Seq.filter (fun s -> s.StartsWith("msbuild", System.StringComparison.OrdinalIgnoreCase))
                |> Seq.toList
            for msbuildEnvVar in msbuildEnvVars do
                psi.Environment.Remove(msbuildEnvVar) |> ignore

            use p = new System.Diagnostics.Process()
            p.StartInfo <- psi

            p.OutputDataReceived.Add(fun ea -> logOut.Enqueue (ea.Data))

            p.ErrorDataReceived.Add(fun ea -> logErr.Enqueue (ea.Data))

            p.Start() |> ignore
            p.BeginOutputReadLine()
            p.BeginErrorReadLine()
            p.WaitForExit()

            let exitCode = p.ExitCode

            exitCode, (workingDir, exePath, args)

        let (exitCode, _) = runProcess()
        exitCode, (logOut |> Seq.filter (not<<isNull) |> Seq.toList)