module Mechanic.GraphAlg
open FSharpx.Collections

type TopologicalOrderResult<'a> =
    | TopologicalOrder of 'a list
    | Cycle of 'a list

let topologicalOrder orderedNodes edges =
    //TODO: maintain original order of nodes
    let orderPos = orderedNodes |> List.mapi (fun i v -> v, i) |> Map.ofList
    let heur nodeInLevels nodes edges = 
        let getLevel n = nodeInLevels |> Map.tryFind n |> Option.defaultValue 0
        let rec f visited n = 
            if Set.contains n visited then 0
            else getLevel n + (edges |> Seq.filter (fun (v,w) -> v<>w && w=n) |> Seq.sumBy (fst >> (f (Set.add n visited))))
        nodes |> Seq.mapi (fun i n -> max 0 (f Set.empty n - i)) |> Seq.sum
    let nodes = orderedNodes |> set
    let nodeInLevel = edges |> Seq.groupBy snd |> Seq.map (fun (v, xs) -> v, Seq.length xs)
    let initZeroInLevelNodes = nodes - (nodeInLevel |> Seq.map fst |> set)
    let nodeInLevel = Seq.append nodeInLevel (initZeroInLevelNodes |> Seq.map (fun x -> x, 0)) |> Map.ofSeq
    let rec solve heap =
        match PriorityQueue.tryPop heap with
        | Some ((costH, cost, nodeLevels, edges, acc), rest) ->
            let zeroInLevelNodes = nodeLevels |> Map.toSeq |> Seq.filter (fun (_, level) -> level = 0) |> Seq.map fst |> set
            match edges, Set.count zeroInLevelNodes with
            | [], 0 -> TopologicalOrder acc
            | (_ :: _), 0 -> 
                //TODO: remove nodes not part of cycle
                Cycle (nodeLevels |> Map.toList |> List.map fst)
            | _ -> 
                let variants = 
                    zeroInLevelNodes |> Seq.sortBy (fun n -> orderPos.[n]) |> Seq.map (fun n ->
                        let (edges, nodeLevels) =
                            let nodeLevels = nodeLevels |> Map.filter (fun v _ -> v <> n)
                            let (edgesToRemove, remainEdges) = edges |> List.partition (fun (v,_) -> v = n)
                            let nodeLevels = (nodeLevels, edgesToRemove) ||> Seq.fold (fun m (_,w) -> m |>Map.add w (m.[w]-1))
                            remainEdges, nodeLevels
                        let h = heur nodeLevels (nodeLevels |> Map.keys |> Seq.sortBy (fun n -> orderPos.[n])) edges
                        let c = cost + abs (orderPos.[n] - List.length acc)
                        let acc = acc @ [n]
                        c+h, c, nodeLevels, edges, acc
                    ) |> List.ofSeq
                solve ((rest, variants) ||> Seq.fold (fun h x -> PriorityQueue.insert x h))
        | None -> failwith ""
    
    match solve (PriorityQueue.empty false |> PriorityQueue.insert (0, 0, nodeInLevel, edges, [])) with
    | TopologicalOrder result ->
        let islandNodes = nodes - (set result)
        TopologicalOrder (Set.toList islandNodes @ result)
    | x -> x
