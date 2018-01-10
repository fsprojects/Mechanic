module Tests.Main

open Expecto
open Mechanic
open Mechanic.GraphAlg
open Mechanic.Utils

let correctOrder edges order =
    let orderPos = order |> List.mapi (fun i v -> v, i) |> Map.ofList
    Seq.forall (fun (v,w) -> orderPos.[v] < orderPos.[w]) edges

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

        ftestPropertyWithConfig (2035338253, 296398869) {FsCheckConfig.defaultConfig with maxTest = 1000; endSize = 100} "Topological order alg - min edit distance" <| fun (edges: list<int * int>) ->
            let edges = edges |> List.filter (fun (v,w) -> v <> w)
            let nodes = edges |> List.collect (fun (v,w) -> [v;w]) |> List.distinct
            let variants = nodes |> List.allPermutations |> List.filter (correctOrder edges)
            let editDistance order1 order2 =
                let orderPos1 = order1 |> List.mapi (fun i v -> v, i) |> Map.ofList
                order2 |> List.mapi (fun i v -> abs (i - orderPos1.[v])) |> List.sum
            match variants with
            | [] -> ()
            | _ ->
            let minOrder = variants |> List.minBy (editDistance nodes)
            let minDistance = editDistance nodes minOrder
            match GraphAlg.topologicalOrder nodes edges, GraphAlg.topologicalOrder (nodes |> List.rev) (edges |> List.map (fun (v,w) -> w,v)) with
            | TopologicalOrder order, TopologicalOrder orderRev ->
                let order2 = orderRev |> List.rev
                Expect.equal (min (editDistance nodes order) (editDistance nodes order2)) minDistance (sprintf "Not minimal edit distance %A %A %A" nodes order minOrder)
            | _ -> ()
    ]

