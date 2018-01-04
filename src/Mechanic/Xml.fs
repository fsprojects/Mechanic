// The MIT License (MIT)

// Copyright (c) 2015 Alexander Groß, Steffen Forkmann

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// This file is a subset of the Xml.fs file from the Paket project:
//     https://github.com/fsprojects/Paket

module Mechanic.Xml

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

let inline getChildNodes (node:XmlNode) = System.Linq.Enumerable.Cast<XmlNode>(node)

let inline hasChildNodes name (node:XmlNode) =
    getChildNodes node
    |> Seq.exists (fun x -> x.Name = name)
