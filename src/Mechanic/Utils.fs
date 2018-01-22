module Mechanic.Utils

let tee f x = f x; x

module Namespace =
    let splitByDot (s:string) = 
        s.Split('.') |> Array.filter (System.String.IsNullOrEmpty >> not) |> Array.toList
        |> function | [] -> [""] | x -> x
    let joinByDot xs = xs |> List.filter (fun s -> String.length s > 0) |> String.concat "."
    let lastPart = splitByDot >> List.last
    let removeLastPart s = s |> splitByDot |> (fun xs -> xs |> List.take (List.length xs - 1)) |> joinByDot