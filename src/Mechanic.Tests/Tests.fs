module Tests.Main

open Expecto
open Mechanic
open Mechanic.GraphAlg

[<Tests>]
 let tests =
    testList "GraphAlg" [
        testProperty "Topological order alg" <| fun (edges: list<int * int>) ->
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
                Expect.equal (List.length order) (List.length nodes) "Number of nodes differs"
                let orderPos = order |> List.mapi (fun i v -> v, i) |> Map.ofList
                Expect.all edges (fun (v,w) -> orderPos.[v] < orderPos.[w]) "Ordering must respect oriented edge"
            | Cycle _ -> Expect.isTrue (haveCycle nodes edges) "Cycle reported on graph without cycle"
    ]

