module Mechanic.GraphAlg

type TopologicalOrderResult<'a> =
    | TopologicalOrder of 'a list
    | Cycle of 'a list

let topologicalOrder orderedNodes edges =
    let orderPos = orderedNodes |> List.mapi (fun i v -> v, i) |> Map.ofList
    let nodes = orderedNodes |> set
    let nodeInLevel = edges |> Seq.groupBy snd |> Seq.map (fun (v, xs) -> v, Seq.length xs)
    let initZeroInLevelNodes = nodes - (nodeInLevel |> Seq.map fst |> set)
    let nodeInLevel = Seq.append nodeInLevel (initZeroInLevelNodes |> Seq.map (fun x -> x, 0)) |> Map.ofSeq
    let rec solve (nodeLevels, edges, acc) =
        let zeroInLevelNodes = nodeLevels |> Map.toSeq |> Seq.filter (fun (_, level) -> level = 0) |> Seq.map fst |> set
        match edges, Set.count zeroInLevelNodes with
        | [], 0 -> TopologicalOrder acc
        | (_ :: _), 0 -> 
            //TODO: remove nodes not part of cycle
            Cycle (nodeLevels |> Map.toList |> List.map fst)
        | _ -> 
            let next = 
                zeroInLevelNodes |> Seq.sortBy (fun n -> orderPos.[n]) |> Seq.map (fun n ->
                    let (edges, nodeLevels) =
                        let nodeLevels = nodeLevels |> Map.filter (fun v _ -> v <> n)
                        let (edgesToRemove, remainEdges) = edges |> List.partition (fun (v,_) -> v = n)
                        let nodeLevels = (nodeLevels, edgesToRemove) ||> Seq.fold (fun m (_,w) -> m |> Map.add w (m.[w]-1))
                        remainEdges, nodeLevels
                    let acc = acc @ [n]
                    nodeLevels, edges, acc
                ) |> Seq.tryHead
            next |> Option.map solve |> Option.defaultValue (TopologicalOrder acc)
    
    match solve (nodeInLevel, edges, []) with
    | TopologicalOrder result ->
        let islandNodes = nodes - (set result)
        TopologicalOrder (Set.toList islandNodes @ result)
    | x -> x
