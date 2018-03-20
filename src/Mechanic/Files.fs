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
    XmlNode : XmlNode
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
           | _ -> failwith "Could not locate project node in project file"

    let loadFromFile fileName =
        let fi = 
            try
                FileInfo fileName
            with
            | _ -> failwithf "The project file '%s' is not a correct file path." fileName

        use stream = 
            try
                fi.OpenRead()
            with
            | exn -> failwithf "Could not load project file '%s'. Message: %s" fileName exn.Message

        loadFromStream fi.FullName stream

    let tryLoad fileName =
        try
            let fi = FileInfo fileName
            match fi.Extension with
            | ".fsproj" -> loadFromFile fi.FullName |> Some
            | _ -> None
        with
        | _ -> None

    let save (pf:ProjectFile) =
        use f = File.Open(pf.FileName, FileMode.Create)
        use sw = new StreamWriter(f)
        pf.Document.Save sw

    let getCompileGroup (node:XmlNode) =
        getDescendants ItemGroupTag node
        |> Seq.tryFind (hasChildNodes CompileTag)

    let parseSourceFileNames (node:XmlNode) =
        getCompileGroup node
        |> Option.map (getChildNodes >> (Seq.choose (fun n -> getAttribute IncludeAttribute n |> Option.map (fun a -> n, a))))
        |> Option.defaultValue Seq.empty
        |> List.ofSeq

    let getSourceFiles (pf:ProjectFile) =
        let projectDir = FileInfo(pf.FileName).Directory.FullName
        parseSourceFileNames pf.ProjectNode
        |> List.map (fun (xml,x) ->
            let fi = FileInfo (Path.Combine(projectDir, x))
            { FullName  = fi.FullName
              ShortName = x 
              XmlNode = xml})

    let makeNode tag (doc:XmlDocument) =
        doc.CreateElement tag

    let makeCompileNode fileName (doc:XmlDocument) =
        let node = makeNode CompileTag doc
        addAttribute IncludeAttribute fileName node
        
    let private areEqual (outerXmlA: string) (outerXmlB: string): bool =
        outerXmlA.Equals(outerXmlB)

    let updateProjectFile (sFiles:SourceFile list) (pf:ProjectFile) =
        let rec addCompileNodes files (parent:XmlNode) (doc:XmlDocument) =
            match files with
            | [] -> parent
            | x::xs ->
                x.XmlNode |> parent.AppendChild |> ignore
                addCompileNodes xs parent doc

        let addNewItemGroup (sFiles:SourceFile list) (pf:ProjectFile) =
            let parent = makeNode ItemGroupTag pf.Document
            addCompileNodes sFiles parent pf.Document
            |> pf.ProjectNode.AppendChild

        let initialContent = pf.Document.OuterXml

        getCompileGroup pf.ProjectNode
        |> function
           | Some x ->
               x.RemoveAll() |> ignore
               addCompileNodes sFiles x pf.Document |> ignore
           | None -> 
               addNewItemGroup sFiles pf |> ignore
        let contentAfterUpdate = pf.Document.OuterXml
        if areEqual contentAfterUpdate initialContent
            then ()
            else save pf