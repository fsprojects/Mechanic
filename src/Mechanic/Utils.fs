module Mechanic.Utils

module List =
    let rec internal distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec allPermutations = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (allPermutations xs)

module Namespace =
    let splitByDot (s:string) = 
        s.Split('.') |> Array.filter (System.String.IsNullOrEmpty >> not) |> Array.toList
        |> function | [] -> [""] | x -> x
    let joinByDot xs = xs |> List.filter (fun s -> String.length s > 0) |> String.concat "."
    let lastPart = splitByDot >> List.last
    let removeLastPart s = s |> splitByDot |> (fun xs -> xs |> List.take (List.length xs - 1)) |> joinByDot
    let rec merge n1 n2 =
        let l1 = splitByDot n1
        let l2 = splitByDot n2
        let len1 = List.length l1
        let len2 = List.length l2
        let l = min len1 len2
        [0..l] |> List.tryFind (fun i -> 
            let l1' = l1 |> List.skip i |> List.take (len1-i)
            let l2' = l2 |> List.take (len2-i)
            if List.isEmpty l1' || List.isEmpty l2' then false else Seq.forall2 (=) l1' l2')
        |> Option.map (fun i -> l1 @ (List.skip (min len2 (len1-i)) l2))
        |> Option.defaultValue (l1 @ l2)
        |> joinByDot