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

let editDistance order1 order2 =
    let rec f xs ys =
        let orderPos1 = xs |> List.mapi (fun i v -> v, i) |> Map.ofList
        match ys with
        | [] -> 0
        | (y::ys) -> orderPos1.[y] + f (xs |> List.filter ((<>)y)) ys
    f order1 order2

let rec fixOrder origOrder edges order =
    let d = editDistance origOrder order
    printfn "fixOrder %A" (order, d)
    let rec move xs (i,j) =
        if j<i then move (List.rev xs) (j,i) |> List.rev
        elif i=j then xs
        else
        let a = Array.ofList xs
        let n = a.Length - 1
        (Array.toList a.[0..i-1]) @ (Array.toList a.[i+1..j]) @ [a.[i]] @ (Array.toList a.[j+1..n])

    [0..(List.length order)-1] |> List.collect (fun i -> [0..(List.length order)-1] |> List.map (fun j -> i,j)) 
    |> List.map (move order)
    |> List.filter (fun order -> 
        let orderPos = order |> List.mapi (fun i v -> v, i) |> Map.ofList
        edges |> List.exists (fun (v,w) -> orderPos.[w] < orderPos.[v]) |> not)
    |> List.map (fun x -> d - editDistance origOrder x, x) |> List.filter (fun (d2,_) -> d2 > 0) 
    |> function
       | [] -> order
       | xs -> 
            xs |> List.maxBy fst |> snd |> fixOrder origOrder edges

let topologicalOrder orderedNodes edges =
    let edges = edges |> List.filter (fun (v,w) -> v<>w)
    let orderPos = orderedNodes |> List.mapi (fun i v -> v, i) |> Map.ofList
    let nodes = orderedNodes |> set
    let nodeInLevel = edges |> Seq.groupBy snd |> Seq.map (fun (v, xs) -> v, Seq.length xs)
    let initZeroInLevelNodes = nodes - (nodeInLevel |> Seq.map fst |> set)
    let nodeInLevel = Seq.append nodeInLevel (initZeroInLevelNodes |> Seq.map (fun x -> x, 0)) |> Map.ofSeq
    let orderF i edges v =
        let rec f v =
            match edges |> List.filter (fun (x,w) -> x=v && orderPos.[w] < orderPos.[v]) with
            | [] -> None
            | xs -> xs |> List.map (fun (_,w) -> (f w |> Option.defaultValue (orderPos.[w], orderPos.[v])) |> Some) |> List.min
        f v |> Option.map (fun (x, y) -> max (i+1) x, max (i+1) y) |> Option.defaultValue (orderPos.[v], orderPos.[v])
    let rec solve (nodeLevels, edges, acc) =
        let zeroInLevelNodes = nodeLevels |> Map.toSeq |> Seq.filter (fun (_, level) -> level = 0) |> Seq.map fst |> set
        match edges, Set.count zeroInLevelNodes with
        | [], 0 -> TopologicalOrder acc
        | (_ :: _), 0 -> 
            //TODO: remove nodes not part of cycle
            let cycleNodes = nodeLevels |> Map.toList |> List.map fst |> set
            match getMinCycle (Set.toList cycleNodes) (edges |> List.filter (fun (v,w) -> Set.contains v cycleNodes && Set.contains w cycleNodes)) with
            | Some c -> Cycle c
            | None -> failwith ""
        | _ -> 
            let next = 
                zeroInLevelNodes |> Seq.sortBy (orderF (List.length acc) edges) |> Seq.map (fun n ->
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
        TopologicalOrder (fixOrder orderedNodes edges result)
    | x -> x
