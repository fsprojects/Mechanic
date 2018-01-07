namespace Mechanic

open System
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

module AstWalker =
    open AstTraversal

    let visitLongIdent (ident: LongIdent) =
        let names = String.concat "." [ for i in ident -> i.idText ]
        names

    let visitPattern = function
        | SynPat.Named(SynPat.Wild(_), name, _, _, _) -> Some name.idText
        | SynPat.Named(_, name, _, _, _) -> Some name.idText
        | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _) -> Some <| visitLongIdent ident
        | _ -> None

    let rec getBind bindings =
        bindings |> Seq.map (fun binding ->
            let (Binding(_, _, _, _, _, _, _, pat, _, _, _, _)) = binding
            visitPattern pat)
        |> Seq.choose id |> Seq.toList

    let getDefSymbols (tree: ParsedInput) =
        let mutable xs = []
        let getNamespace path =
            path |> List.choose (function
                | TraverseStep.ModuleOrNamespace(SynModuleOrNamespace(lId,_,_,_,_,_,_,_)) -> 
                    Some (visitLongIdent lId)
                | TraverseStep.Module(SynModuleDecl.NestedModule(ComponentInfo(_,_,_,lId,_,_,_,_),_,_,_,_)) -> 
                    Some (visitLongIdent lId)
                | _ -> None
            ) |> List.rev |> String.concat "."
        let visitor = { new AstVisitorBase<_>() with
            //TODO: type defs
            //TODO: record fields
            //TODO: union fields
            override __.VisitExpr(_, subExprF, defF, e) =
                match e with
                | _ -> defF e
            override __.VisitBinding(path, defF, x) = 
                match path with
                | TraverseStep.Expr _ :: _ -> defF x
                | _ ->
                    xs <- xs @ (getBind [x] |> List.map (fun s -> getNamespace path + "." + s)); defF x
            }
        Traverse(tree, visitor) |> ignore
        xs
    
    let getUsedSymbols (tree: ParsedInput) =
        let mutable xs = []
        let visitor = { new AstVisitorBase<_>() with
            override __.VisitExpr(_, subExprF, defF, e) =
                match e with
                | SynExpr.Ident(id) -> xs <- id.idText :: xs; defF e
                | SynExpr.LongIdent(_, LongIdentWithDots(lId,_), _, _) -> xs <- visitLongIdent lId :: xs; defF e
                | _ -> defF e
            }
        Traverse(tree, visitor) |> ignore
        xs

    let getOpenDecls (tree: ParsedInput) =
        //TODO: open in module with scope
        let mutable xs = []
        let visitor = { new AstVisitorBase<_>() with
            override __.VisitExpr(_, subExprF, defF, e) =
                match e with | _ -> defF e
            override __.VisitModuleDecl(defF, d) =
                match d with
                | SynModuleDecl.Open(LongIdentWithDots(lId, _),_) -> xs <- visitLongIdent lId :: xs; defF d
                | _ -> defF d
            }
        Traverse(tree, visitor) |> ignore
        xs

module SymbolGetter =
    let checker = FSharpChecker.Create()

    let parseSingleFile (file, input) = 
        let (projOptions, _) = 
            checker.GetProjectOptionsFromScript(file, input)
            |> Async.RunSynchronously

        let (parsingOptions, _) = checker.GetParsingOptionsFromProjectOptions projOptions
      
        let parseFileResults = 
            checker.ParseFile(file, input, parsingOptions) 
            |> Async.RunSynchronously

        // Wait until type checking succeeds (or 100 attempts)
        parseFileResults

    let getSymbols file =
        let input = System.IO.File.ReadAllText file 
        let parseFileResults = parseSingleFile(file, input)
        let tree = parseFileResults.ParseTree.Value

        let opens = AstWalker.getOpenDecls tree |> List.rev
        let defSymbolNames = AstWalker.getDefSymbols tree |> set |> Set.toList
        let usedSymbolNames = AstWalker.getUsedSymbols tree |> set |> Set.toList

        file, defSymbolNames, opens, usedSymbolNames

module SymbolGraph =
    let splitByDot (s:string) = s.Split('.') |> Array.filter (String.IsNullOrEmpty >> not) |> Array.toList
    let lastPart = splitByDot >> List.last
    let tee f x = f x; x
    let getDependencies files =
        let depsData = files |> List.map SymbolGetter.getSymbols
        let allDefsMap = 
            depsData |> Seq.collect (fun (f,defs,_,_) -> defs |> List.map (fun d -> lastPart d, (d, f)))
            |> Seq.groupBy fst |> Seq.map (fun (k, xs) -> k, xs |> Seq.map snd |> Seq.toList) |> Map.ofSeq
        let depsData = 
            depsData |> List.map (fun (f,defs,opens,uses) -> 
                f, defs, opens, uses |> List.filter (fun u -> allDefsMap |> Map.containsKey (lastPart u)))
        // depsData |> Seq.iter (fun (f,defs,opens,uses) -> 
        //     printfn "File: %A" f
        //     printfn "Def: %A" defs
        //     printfn "Opens: %A" opens
        //     printfn "Used: %A" uses
        // )
        let deps =
            depsData |> List.collect (fun (f2, defs2, opens2, uses2) ->
                let rec merge l1 l2 =
                    let len1 = List.length l1
                    let len2 = List.length l2
                    let l = min len1 len2
                    [0..l] |> List.tryFind (fun i -> 
                        let l1' = l1 |> List.skip i |> List.take (len1-i)
                        let l2' = l2 |> List.take (len2-i)
                        if l1' = [] || l2' = [] then false else Seq.forall2 (fun x y -> x = y) l1' l2')
                    |> Option.map (fun i -> l1 @ (List.skip (min len2 (len1-i)) l2))
                    |> Option.defaultValue (l1 @ l2)
                let joinByDot xs = String.concat "." xs
                let opensVariants s = ("" :: opens2) |> List.map (fun o -> merge (splitByDot o) (splitByDot s) |> joinByDot)
                let tryFindDef s = 
                    allDefsMap |> Map.tryFind (lastPart s)
                    |> Option.bind (fun g -> 
                        let r = g |> List.tryFind (fun (d,f) -> opensVariants s |> List.exists ((=)d))
                        match r with
                        | None -> 
                            //printfn "No match: %s -- %A -- %A" f2 (opensVariants s) g
                            None
                        | Some _ -> r)
                    |> Option.map (fun (d,f) -> f, f2, d)
                uses2 |> List.choose tryFindDef
            )
            |> List.groupBy (fun (f1, f2, _) -> f1, f2) |> List.map (fun ((f1, f2), xs) -> f1, f2, xs |> List.map (fun (_,_,x) -> x))
        printfn "%A" deps
        deps

    let solveOrder files =
        let deps = getDependencies files
        let edges = deps |> List.map (fun (f1,f2,_) -> f1, f2)
        GraphAlg.topologicalOrder files edges

module Say =
    let hello name =
        sprintf "Hello %s" name
