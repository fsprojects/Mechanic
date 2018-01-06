module Mechanic.Xml
// This file is a subset of the Xml.fs file from the Paket project:
//     https://github.com/fsprojects/Paket


open System.Xml


let inline addAttribute name value (node:XmlElement) =
    node.SetAttribute(name, value) |> ignore
    node

let inline addChild child (node:XmlElement) =
    node.AppendChild(child) |> ignore
    node

let inline hasAttribute name (node:XmlNode) =
    if isNull node || isNull node.Attributes then false else
        node.Attributes
        |> Seq.cast<XmlAttribute>
        |> Seq.exists (fun x -> x.Name = name)

let inline getAttribute name (node:XmlNode) =
    if isNull node || isNull node.Attributes then None else
    node.Attributes 
    |> Seq.cast<XmlAttribute> 
    |> Seq.tryFind (fun a -> a.Name = name && (isNull a.Value |> not)) 
    |> Option.map (fun a -> a.Value)

let inline getNode name (node:XmlNode) =
    let xpath = sprintf "*[local-name() = '%s']" name
    match node.SelectSingleNode(xpath) with
    | null -> None
    | n -> Some(n)

let inline getNodes name (node:XmlNode) =
    let xpath = sprintf "*[local-name() = '%s']" name
    match node.SelectNodes(xpath) with
    | null -> []
    | nodeList -> 
        nodeList
        |> Seq.cast<XmlNode>
        |> Seq.toList

let inline getDescendants name (node:XmlNode) = 
    let xpath = sprintf ".//*[local-name() = '%s']" name
    match node.SelectNodes(xpath) with
    | null -> []
    | nodeList -> 
        nodeList
        |> Seq.cast<XmlNode>
        |> Seq.toList

let inline getChildNodes (node:XmlNode) = 
    System.Linq.Enumerable.Cast<XmlNode>(node)

let inline hasChildNodes name (node:XmlNode) =
    getChildNodes node
    |> Seq.exists (fun x -> x.Name = name)