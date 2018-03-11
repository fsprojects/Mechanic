module Mechanic.SymbolGraph
open System.IO
open Utils.Namespace
open Mechanic.Utils
open Mechanic.GraphAlg
open AstSymbolCollector

let getDependencies files =
    let depsData = files |> List.map (fun (f: string) -> if f.EndsWith ".fs" then SymbolGetter.getSymbols f else f, [], [])
    let autoOpens = depsData |> List.collect (fun (_,_,g) -> g |> List.collect (fun x -> x.Opens |> List.filter (fun o -> o.IsAutoOpen)))
    let depsData = depsData |> List.map (fun (f,defs,opens) -> f, defs, opens |> List.map (fun g -> { g with Opens = g.Opens @ autoOpens }))
    let allDefsMap = 
        depsData |> Seq.collect (fun (f,defs,_) -> defs |> List.map (fun d -> Symbol.map lastPart d, (d, f)))
        |> Seq.groupBy fst |> Seq.map (fun (k, xs) -> k, xs |> Seq.map snd |> Seq.toList) |> Map.ofSeq
    let depsData = 
        depsData |> List.map (fun (f,defs,opens) -> 
            f, defs, opens |> List.map (fun o -> 
                { o with UsedSymbols = o.UsedSymbols |> List.filter (fun u -> allDefsMap |> Map.containsKey (Symbol.map lastPart u)) } ))
        |> List.collect (fun (f2, defs2, opens2) -> 
            opens2 |> List.map (fun o -> 
                f2, defs2, o.Opens |> List.map (fun x -> x.OpenName), o.UsedSymbols))
    // depsData |> Seq.iter (fun (f,defs,opens,uses) -> 
    //     printfn "File: %A" f
    //     printfn "Def: %A" defs
    //     printfn "Opens: %A" opens
    //     printfn "Used: %A" uses
    // )
    let deps =
        depsData |> List.collect (fun (f2, _, opens2, uses2) ->
            // Concat two list and merge same part on end of first list and start of second list.
            // Ex: merge ["A";"B";"C"] ["C";"D"] = ["A";"B";"C";"D"]
            
            let opensVariants symbol = ("" :: opens2) |> List.map (fun o -> symbol |> Symbol.map (fun s -> merge o s))
            //printfn "%A" allDefsMap
            let tryFindDef s = 
                allDefsMap |> Map.tryFind (Symbol.map lastPart s)
                |> Option.bind (fun g -> 
                    let r = 
                        // try local definitions (from same file) first
                        opensVariants s |> List.tryPick (fun o -> g |> List.tryFind (fun (d,f) -> o = d && f=f2))
                        |> Option.orElseWith (fun () -> opensVariants s |> List.tryPick (fun o -> g |> List.tryFind (fun (d,_) -> o = d)))
                    match r with
                    | None -> 
                        //printfn "No match: %s -- %A -- %A" f2 (opensVariants s) g
                        None
                    | Some _ -> 
                        //printfn "Find match: %A -- %s" r f2
                        r)
                |> Option.map (fun (d,f) -> f, f2, d)
            uses2 |> List.choose tryFindDef
        )
        |> List.filter (fun (f1,f2,_) -> f1 <> f2) 
        |> List.groupBy (fun (f1, f2, _) -> f1, f2) |> List.map (fun ((f1, f2), xs) -> 
            f1, f2, xs |> List.map (fun (_,_,x) -> x) |> List.distinct)
    //printfn "%A" deps
    deps

let solveOrder fileNameSelector xs =
    let filesMap = xs |> Seq.map (fun x -> fileNameSelector x, x) |> Map.ofSeq
    let files = xs |> List.map fileNameSelector
    let deps = getDependencies files
    let edges = deps |> List.map (fun (f1,f2,_) -> f1, f2)
    match GraphAlg.topologicalOrder files edges with
    | TopologicalOrderResult.Cycle xs ->
        printfn "Cycle with %A" (deps |> List.filter (fun (x,y,_) -> List.contains x xs && List.contains y xs))
        TopologicalOrderResult.Cycle (xs |> List.map (fun x -> filesMap.[x]))
    | TopologicalOrderResult.TopologicalOrder xs -> TopologicalOrderResult.TopologicalOrder (xs |> List.map (fun x -> filesMap.[x]))

let solveOrderFromPattern root filePattern =
    Directory.EnumerateFiles(root,filePattern) |> Seq.toList
    |> solveOrder id