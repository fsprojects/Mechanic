module Mechanic.GraphAlg

type TopologicalOrderResult<'a> =
    | TopologicalOrder of 'a list
    | Cycle of 'a list

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
            Cycle (nodeLevels |> Map.toList |> List.map fst)
        | _ -> solve edges nodeLevels acc
    match solve edges nodeInLevel [] with
    | TopologicalOrder result ->
        let islandNodes = nodes - (set result)
        TopologicalOrder (Set.toList islandNodes @ result)
    | x -> x
