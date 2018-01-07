module Tests

open FsCheck.Xunit
open Swensen.Unquote
open Mechanic
open Mechanic.GraphAlg

[<Property>]
let ``Topological order alg`` (edges: list<int * int>) =
    let rec haveCycleAcc edges acc =
        let (edgesFrom, edgesRemain) = edges |> List.partition (fun (v,_) -> Set.contains v acc)
        match edgesFrom with
        | [] -> false
        | _ ->
        let nodes = edgesFrom |> Seq.map (fun (_,w) -> w) |> set
        match Set.intersect nodes acc |> Set.isEmpty with
        | true -> haveCycleAcc edgesRemain (acc + nodes)
        | false -> true
    let haveCycle (nodes: list<int>) edges = nodes |> Seq.exists (fun v -> haveCycleAcc edges (set [v]))

    let nodes = edges |> List.collect (fun (v,w) -> [v;w]) |> List.distinct
    match GraphAlg.topologicalOrder nodes edges with
    | TopologicalOrder order ->
        List.length nodes =! List.length order
        let orderPos = order |> List.mapi (fun i v -> v, i) |> Map.ofList
        <@ edges |> Seq.forall (fun (v,w) -> orderPos.[v] < orderPos.[w]) @> |> test
    | Cycle _ -> <@ haveCycle nodes edges @> |> test
