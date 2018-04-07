module Mechanic.SymbolGraph
open System.IO
open Utils.Namespace
open Mechanic.Utils
open Mechanic.GraphAlg
open AstSymbolCollector

let getDependencies options files maybeProjFile =
    let depsData = files |> List.map (fun (f: string) -> if f.EndsWith ".fs" then SymbolGetter.getSymbols options f else f, [], [])
    let autoOpens parents = 
        depsData |> List.collect (fun (_,_,g) -> g |> List.collect (fun x -> x.Opens |> List.filter (fun o -> 
            match o.AutoOpenParent with
            | Some p when p = "" || parents |> List.exists (fun x -> x.OpenName = p) -> true
            | _ -> false)))
    let depsData = 
        depsData |> List.map (fun (f,defs,opens) -> 
            f, defs, opens |> List.map (fun g -> { g with Opens = g.Opens @ autoOpens g.Opens }))
    let findEntity = maybeProjFile |> Option.map SymbolGetter.getExternalFindDefFun |> Option.defaultValue (fun _ -> None)
    let allDefsMap = 
        let defs = depsData |> Seq.collect (fun (f,defs,_) -> defs |> List.map (fun d -> Symbol.map lastPart d, (d, Some f)))
        defs |> Seq.groupBy fst |> Seq.map (fun (k, xs) -> k, xs |> Seq.map snd |> Seq.toList) |> Map.ofSeq
    let depsData = 
        depsData |> List.map (fun (f,defs,opens) -> 
            f, defs, opens |> List.map (fun o -> 
                { o with UsedSymbols = o.UsedSymbols |> List.filter (fun u -> allDefsMap |> Map.containsKey (Symbol.map lastPart u)) } ))
        |> List.collect (fun (f2, defs2, opens2) -> 
            opens2 |> List.map (fun o -> 
                f2, defs2, o.Opens |> List.map (fun x -> x.OpenName), o.UsedSymbols))
    if options.LogOutput.CollectedSymbols then
        depsData |> Seq.iter (fun (f,defs,opens,uses) -> 
            printfn "File: %A" f
            printfn "Def: %A" defs
            printfn "Opens: %A" opens
            printfn "Used: %A" uses
        )
    let deps =
        depsData |> List.collect (fun (f2, _, opens2, uses2) ->
            // Concat two list and merge same part on end of first list and start of second list.
            // Ex: merge ["A";"B";"C"] ["C";"D"] = ["A";"B";"C";"D"]
            
            let opensVariants symbol = ("" :: opens2) |> List.map (fun o -> symbol |> Symbol.map (fun s -> merge o s))
            //printfn "%A" allDefsMap
            let tryFindDef s = 
                // check for external definition
                opensVariants s |> List.tryPick findEntity |> Option.map (fun d -> None, f2, d)
                |> Option.orElseWith (fun () ->
                    allDefsMap |> Map.tryFind (Symbol.map lastPart s)
                    |> Option.bind (fun g -> 
                        let r = 
                            // try local definitions (from same file) first
                            opensVariants s |> List.tryPick (fun o -> g |> List.tryFind (fun (d,f) -> o = d && Option.forall ((=)f2) f))
                            |> Option.orElseWith (fun () -> opensVariants s |> List.tryPick (fun o -> g |> List.tryFind (fun (d,_) -> o = d)))
                        match r with
                        | None -> 
                            //printfn "No match: %s -- %A -- %A" f2 (opensVariants s) g
                            None
                        | Some _ -> 
                            //printfn "Find match: %A -- %s" r f2
                            r)
                    |> Option.map (fun (d,f) -> f, f2, d))
            uses2 |> List.choose tryFindDef
        )
        |> List.choose (fun (f1,f2,x) -> f1 |> Option.bind (fun f1 -> if f1 <> f2 then Some (f1, f2, x) else None))
        |> List.groupBy (fun (f1, f2, _) -> f1, f2) |> List.map (fun ((f1, f2), xs) -> 
            f1, f2, xs |> List.map (fun (_,_,x) -> x) |> List.distinct)
    if options.LogOutput.FileDependencies then
        if options.LogOutput.FileDependenciesWithSymbols then    
            printfn "%A" deps
        else        
            printfn "%A" (deps |> List.map (fun (f1, f2, _) -> f1, f2))
    deps

let solveOrder (options: Mechanic.Options) fileNameSelector maybeProjFile xs =
    let filesMap = xs |> Seq.map (fun x -> fileNameSelector x, x) |> Map.ofSeq
    let files = xs |> List.map fileNameSelector
    let deps = getDependencies options files maybeProjFile
    let edges = deps |> List.map (fun (f1,f2,_) -> f1, f2)
    match GraphAlg.topologicalOrder files edges with
    | TopologicalOrderResult.Cycle xs ->
        printfn "Cycle with %A" (deps |> List.filter (fun (x,y,_) -> List.contains x xs && List.contains y xs))
        TopologicalOrderResult.Cycle (xs |> List.map (fun x -> filesMap.[x]))
    | TopologicalOrderResult.TopologicalOrder xs -> TopologicalOrderResult.TopologicalOrder (xs |> List.map (fun x -> filesMap.[x]))

let solveOrderFromPattern options root filePattern =
    Directory.EnumerateFiles(root,filePattern) |> Seq.toList
    |> solveOrder options id None

let shuffleTest (options: Mechanic.Options) fileNameSelector projFile xs =
    let printSeq label xs =
        printfn "%s" label
        xs |> Seq.iter (printfn "%A")
    let sourceFiles = xs |> Seq.map fileNameSelector
    if not (Seq.isEmpty (Mechanic.SymbolGetter.checkWithFsc projFile sourceFiles)) then
        printfn "Original order of project %s is not valid." projFile
        false
    else
    let n = Seq.length xs
    [0..n-1] |> Seq.forall (fun i ->
        ([i-1..-1..0] @ [i+1..n-1]) |> List.forall (fun j ->
            let ys = List.moveItemAtIndexBy i (j-i) xs
            printfn "Shuffle test: %s(%i) moved by %i." (Seq.map fileNameSelector xs |> Seq.item i) i (j-i)
            match solveOrder options fileNameSelector (Some projFile) (Seq.toList ys) with
            | TopologicalOrderResult.TopologicalOrder result ->
                let sourceFiles = result |> List.map fileNameSelector
                let errors = Mechanic.SymbolGetter.checkWithFsc projFile sourceFiles
                if not (Seq.isEmpty errors) then 
                    printfn "Mechanic outputs invalid order of project %s:" projFile
                    printSeq "Tested order:" (Seq.map fileNameSelector ys) 
                    printSeq "Resulting order:" sourceFiles
                    printSeq "Errors:" errors
                    false
                else true
            | TopologicalOrderResult.Cycle _ -> true
        )
    )