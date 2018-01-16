module Mechanic.AstSymbolCollector
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices.AstTraversal
open Mechanic.Utils

type OpenDeclGroup = { Opens: list<string>; UsedSymbols: list<string> }

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

let getUsedSymbols defRange (tree: ParsedInput) =
    let mutable xs = []
    let visitor = { new AstVisitorBase<_>() with
        override __.VisitExpr(_, subExprF, defF, e) =
            match e with
            | SynExpr.Ident(id) -> xs <- (id.idText, defRange) :: xs; defF e
            | SynExpr.LongIdent(_, LongIdentWithDots(lId,_), _, r) -> xs <- ((visitLongIdent lId), r) :: xs; defF e
            | _ -> defF e
        }
    Traverse(tree, visitor) |> ignore
    xs

let getOpenDecls (tree: ParsedInput) =
    //TODO: open in module with scope
    let getUsesInRange range = getUsedSymbols range tree |> List.filter (fun (_,r) -> Range.rangeContainsRange range r) |> List.map fst
    let mkOpenDecl xs r = { Opens = xs; UsedSymbols  = getUsesInRange r }
    let getRange path =
        path |> List.choose (function
            | TraverseStep.ModuleOrNamespace(SynModuleOrNamespace(_,_,_,_,_,_,_,r))
            | TraverseStep.Module(SynModuleDecl.NestedModule(_,_,_,_,r)) -> Some r
            | _ -> None
        ) |> List.tryHead
    let mutable xs = []
    let visitor = { new AstVisitorBase<_>() with
        override __.VisitExpr(_, subExprF, defF, e) =
            match e with | _ -> defF e
        override __.VisitModuleDecl(defF, d) =
            match d with
            | SynModuleDecl.Open(LongIdentWithDots(lId, _),r) -> xs <- ((visitLongIdent lId), r) :: xs; defF d
            | SynModuleDecl.NestedModule(ComponentInfo(_,_,_,lId,_,_,_,_),_,_,_,r) -> xs <- ((visitLongIdent lId), r) :: xs; defF d
            | _ -> defF d
        override __.VisitModuleOrNamespace(SynModuleOrNamespace(lId,_,isModule,_,_,_,_,r)) =
            let ident = visitLongIdent lId
            let ident = if isModule then Namespace.removeLastPart ident else ident 
            xs <- (ident, r) :: xs
            None
        }
    Traverse(tree, visitor) |> ignore
    printfn "%A" xs
    xs |> List.groupBy snd |> List.map (fun (r, xs) -> mkOpenDecl (xs |> List.map fst) r)
