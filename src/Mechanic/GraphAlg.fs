module Mechanic.GraphAlg

type TopologicalOrderResult<'a> =
    | TopologicalOrder of 'a list
    | Cycle of 'a list

let getMinCycle (nodes: list<_>) edges = 
    let choosePick f xs =
        match xs |> List.choose id with
        | [] -> None
        | xs -> xs |> f |> Some
    let rec getCycleAcc edgesMap path =
        match path with
        | [] -> failwith ""
        | (v::vs) ->
        match vs |> List.contains v with
        | true -> Some vs
        | false ->
        let nodes = edgesMap |> Map.tryFind v |> Option.defaultValue []
        nodes |> List.map (fun v -> getCycleAcc edgesMap (v :: path)) |> choosePick (Seq.minBy (List.length))

    let edgesMap = edges |> Seq.groupBy fst |> Seq.map (fun (v,g) -> v, g |> Seq.map snd |> Seq.toList) |> Map.ofSeq
    nodes |> List.map (fun v -> getCycleAcc edgesMap [v]) |> choosePick (Seq.minBy (List.length))

let topologicalOrder orderedNodes edges =
    //TODO: maintain original order of nodes
    let nodes = orderedNodes |> set
    let nodeInLevel = edges |> Seq.groupBy snd |> Seq.map (fun (v, xs) -> v, Seq.length xs)
    let initZeroInLevelNodes = nodes - (nodeInLevel |> Seq.map fst |> set)
    let nodeInLevel = Seq.append nodeInLevel (initZeroInLevelNodes |> Seq.map (fun x -> x, 0)) |> Map.ofSeq
    let rec solve edges nodeLevels acc =
        let zeroInLevelNodes = nodeLevels |> Map.toSeq |> Seq.filter (fun (_, level) -> level = 0) |> Seq.map fst |> set
        let nodeLevels = nodeLevels |> Map.filter (fun _ level -> level > 0)
        let (edges, nodeLevels) =
            let (edgesToRemove, remainEdges) = edges |> List.partition (fun (v,_) -> Set.contains v zeroInLevelNodes)
            let nodeLevels = (nodeLevels, edgesToRemove) ||> Seq.fold (fun m (_,w) -> m |>Map.add w (m.[w]-1))
            remainEdges, nodeLevels
        let acc = acc @ (Set.toList zeroInLevelNodes)
        match edges, Set.count zeroInLevelNodes with
        | [], 0 -> TopologicalOrder acc
        | (_ :: _), 0 -> 
            //TODO: remove nodes not part of cycle
            let cycleNodes = nodeLevels |> Map.toList |> List.map fst |> set
            match getMinCycle (Set.toList cycleNodes) (edges |> List.filter (fun (v,w) -> Set.contains v cycleNodes && Set.contains w cycleNodes)) with
            | Some c -> Cycle c
            | None -> failwith ""
        | _ -> solve edges nodeLevels acc
    match solve edges nodeInLevel [] with
    | TopologicalOrder result ->
        let islandNodes = nodes - (set result)
        TopologicalOrder (Set.toList islandNodes @ result)
    | x -> x
