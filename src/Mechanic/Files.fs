namespace Mechanic.Files

open System.IO
open System.Xml


type ProjectFile = {
    FileName : string
    Document : XmlDocument
}

type SourceFile = SourceFile of fileName:string


module ProjectFile =

    let [<Literal>] CompileTag    = "Compile"
    let [<Literal>] IncludeAttribute = "Include"
    let [<Literal>] XmlSchema = "http://schemas.microsoft.com/developer/msbuild/2003"

    let inline getNodes name (node:XmlNode) =
        let xpath = sprintf ".//*[local-name() = '%s']" name
        match node.SelectNodes(xpath) with
        | null -> []
        | nodeList -> 
            nodeList
            |> Seq.cast<XmlNode>
            |> Seq.toList

    let loadFromStream fileName (stream:Stream) =
        let doc = XmlDocument()
        doc.Load stream
        let ns = XmlNamespaceManager(doc.NameTable)
        ns.AddNamespace("ns", XmlSchema)
        { FileName = fileName; Document = doc}

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

    let parseFileNames (tag:string) (attr:string) (doc:XmlDocument) =
        getNodes tag doc
        |> List.map (fun x ->
            x.Attributes.[attr].Value)

    let getSourceFiles (pFile:ProjectFile) =
        let dir = Path.GetDirectoryName pFile.FileName
        parseFileNames CompileTag IncludeAttribute pFile.Document
        |> List.map (fun x ->
            let fi = FileInfo (Path.Combine(dir, x))
            SourceFile fi.FullName)

    