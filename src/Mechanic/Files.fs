namespace Mechanic.Files

open System.IO
open System.Xml


type ProjectFile = {
    FileName    : string
    ProjectNode : XmlNode
    Document    : XmlDocument
}

type SourceFile = {
    FullName : string
    ShortName : string 
}


module ProjectFile =

    open Mechanic.Xml

    let [<Literal>] ProjectTag       = "Project"
    let [<Literal>] ItemGroupTag     = "ItemGroup"
    let [<Literal>] CompileTag       = "Compile"
    let [<Literal>] IncludeAttribute = "Include"
    let [<Literal>] XmlSchema        = "http://schemas.microsoft.com/developer/msbuild/2003"


    let loadFromStream fileName (stream:Stream) =
        let doc = XmlDocument()
        doc.Load stream
        let ns = XmlNamespaceManager(doc.NameTable)
        ns.AddNamespace("ns", XmlSchema)
        getNode ProjectTag doc
        |> function
           | Some n ->
             { FileName = fileName; ProjectNode = n; Document = doc}
           | _ -> failwith "Could not locate project node in project File"

    let loadFromFile fileName =
        let fi = FileInfo fileName
        use stream = fi.OpenRead()
        loadFromStream fi.FullName stream

    let tryLoad fileName =
        try
            let fi = FileInfo fileName
            match fi.Extension with
            | ".fsproj" -> loadFromFile fi.FullName |> Some
            | _ -> None
        with
        | exn -> None

    let save (pf:ProjectFile) =
        use f = File.Open(pf.FileName, FileMode.Create)
        use sw = new StreamWriter(f)
        pf.Document.Save sw

    let getCompileGroup (node:XmlNode) =
        getDescendants ItemGroupTag node
        |> Seq.tryFind (hasChildNodes CompileTag)

    let parseSourceFileNames (node:XmlNode) =
        getCompileGroup node
        |> Option.map (getChildNodes >> (Seq.choose (getAttribute IncludeAttribute)))
        |> Option.defaultValue Seq.empty<string>
        |> List.ofSeq

    let getSourceFiles (pFile:ProjectFile) =
        parseSourceFileNames pFile.ProjectNode
        |> List.map (fun x ->
            let fi = FileInfo x
            { FullName  = fi.FullName
              ShortName = x })

    let makeNode tag (doc:XmlDocument) =
        doc.CreateElement tag

    let makeCompileNode fileName (doc:XmlDocument) =
        let node = makeNode CompileTag doc
        addAttribute IncludeAttribute fileName node
        
    let updateProjectFile (sFiles:SourceFile list) (pf:ProjectFile) =
        let rec addCompileNodes files (parent:XmlNode) =
            match files with
            | [] -> parent
            | x::xs ->
                makeCompileNode x.ShortName pf.Document
                |> parent.AppendChild |> ignore
                addCompileNodes xs parent

        let addNewItemGroup (sFiles:SourceFile list) (pf:ProjectFile) =
            let parent = makeNode ItemGroupTag pf.Document
            addCompileNodes sFiles parent
            |> pf.ProjectNode.AppendChild

        let cg = getCompileGroup pf.ProjectNode
        match cg with
        | Some x ->
            pf.ProjectNode.RemoveChild x |> ignore
            addNewItemGroup sFiles pf |> ignore
        | None -> 
            addNewItemGroup sFiles pf |> ignore

        save pf