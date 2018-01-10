module Mechanic.Utils

let tee f x = f x; x

module List =
    let rec internal distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec allPermutations = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (allPermutations xs)

module Namespace =
    let splitByDot (s:string) = s.Split('.') |> Array.filter (System.String.IsNullOrEmpty >> not) |> Array.toList
    let joinByDot xs = String.concat "." xs
    let lastPart = splitByDot >> List.last
    let removeLastPart s = s |> splitByDot |> (fun xs -> xs |> List.take (List.length xs - 1)) |> joinByDot