module Tests.Main

open Expecto
open Mechanic
open Mechanic.GraphAlg
open Mechanic.Utils

module Gen =
    open FsCheck
    
    type RandomGraph = RandomGraph of list<int * int>
    let genEdges = 
        Gen.sized (fun s -> 
            let nodeGen = [0..s] |> List.map Gen.constant |> Gen.oneof
            Gen.map2 (fun x y -> x,y) nodeGen nodeGen |> Gen.listOfLength (s*s)) 
        |> Gen.map set |> Gen.map (Set.toList)
        |> Arb.fromGen
        |> Arb.convert RandomGraph (fun (RandomGraph l) -> l)
    let addToConfig config =
        {config with arbitrary = typeof<RandomGraph>.DeclaringType::config.arbitrary}


let correctOrder edges order =
    let orderPos = order |> List.mapi (fun i v -> v, i) |> Map.ofList
    Seq.forall (fun (v,w) -> orderPos.[v] < orderPos.[w]) edges

[<Tests>]
 let tests =
    testList "GraphAlg" [
        testPropertyWithConfig (Gen.addToConfig {FsCheckConfig.defaultConfig with maxTest = 1000; endSize = 100}) "Topological order alg" <| fun (Gen.RandomGraph edges) ->
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

        testPropertyWithConfig (Gen.addToConfig {FsCheckConfig.defaultConfig with maxTest = 100; endSize = 5}) "Topological order alg - min edit distance" <| fun (Gen.RandomGraph edges) ->
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

