module Mechanic.AstSymbolCollector
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices.AstTraversal
open Mechanic.Utils

type Symbol =
    | Identificator of string
    | RecordField of string
    | TypeSymbol of string
module Symbol = 
    let get x = match x with |Identificator s -> s |RecordField s -> s |TypeSymbol s -> s
    let map f = function
        | Identificator s -> Identificator (f s)
        | RecordField s -> RecordField (f s)
        | TypeSymbol s -> TypeSymbol (f s)

type OpenDecl = { OpenName: string; Pos: Range.pos; Range: Range.range }
type SymbolDef = { SymbolName: Symbol; LocalRange: Range.range option }
type SymbolUse = { SymbolName: Symbol; Range: Range.range }
type OpenDeclGroup = { Opens: list<string>; UsedSymbols: list<Symbol> }

let visitLongIdent (ident: LongIdent) =
    let names = String.concat "." [ for i in ident -> i.idText ]
    names

let rec getTypes synType =
    match synType with
    | SynType.LongIdent(LongIdentWithDots(lId, _)) -> [TypeSymbol(visitLongIdent lId), lId |> List.map (fun x -> x.idRange) |> List.reduce Range.unionRanges]
    | SynType.StructTuple(ps, _)
    | SynType.Tuple (ps, _) -> ps |> List.map snd |> List.collect getTypes
    | SynType.Array (_, t, _) -> getTypes t
    | SynType.Fun (t1, t2, _) -> getTypes t1 @ getTypes t2
    | SynType.App (t, _, ts, _, _, _, _)
    | SynType.LongIdentApp (t, _, _, ts, _, _, _) -> (t :: ts) |> List.collect getTypes
    | _ -> []

let rec getSynConstructorArgs = function
    | SynConstructorArgs.Pats ps -> ps |> List.collect visitPattern
    | SynConstructorArgs.NamePatPairs (ps, _) -> ps |> List.map snd |> List.collect visitPattern

and visitPattern = function
    | SynPat.Named(SynPat.Wild(_), name, _, _, range) -> [Identificator name.idText, range]
    | SynPat.OptionalVal(name, range)
    | SynPat.Named(_, name, _, _, range) -> [Identificator name.idText, range]
    | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, args, _, range) -> 
        getSynConstructorArgs args @ [Identificator (visitLongIdent ident), range]
    | SynPat.Typed(p, typ, _) -> visitPattern p @ getTypes typ
    | SynPat.Paren(p, _) -> visitPattern p
    | SynPat.Ands(ps, _)
    | SynPat.ArrayOrList(_ , ps, _)
    | SynPat.Tuple(ps, _) -> ps |> List.collect visitPattern
    | SynPat.Or(p1, p2, _) -> visitPattern p1 @ visitPattern p2
    | _ -> []

let rec visitSimplePattern = function
    | SynSimplePat.Id(ident, _, _, _, _, range) -> [Identificator ident.idText, range]
    | SynSimplePat.Typed(p, typ, _) -> visitSimplePattern p @ getTypes typ
    | _ -> []


let rec getBind bindings =
    bindings |> Seq.collect (fun binding ->
        let (Binding(_, _, _, _, _, _, _, pat, _, _, _, _)) = binding
        visitPattern pat)
    |> Seq.toList


let getFieldsAndTypesFromTypeDefn (SynTypeDefn.TypeDefn(SynComponentInfo.ComponentInfo _, repr,_,_)) =
    match repr with
    | SynTypeDefnRepr.Simple(simpleRepr,_) ->
        match simpleRepr with
        | SynTypeDefnSimpleRepr.Record(_,fields,_) -> 
            fields |> List.collect (function 
                SynField.Field(_,_,ident, synType,_,_,_, range) -> 
                    (ident |> Option.toList |> List.map (fun ident -> RecordField ident.idText, range)) 
                    @ (getTypes synType))
        | SynTypeDefnSimpleRepr.Union(Some SynAccess.Private,_,_) -> []
        | SynTypeDefnSimpleRepr.Union(_,cases,_) ->
            cases |> List.collect (function 
                SynUnionCase.UnionCase(_,ident,synUnionType,_,_,range) -> 
                    let types =
                        match synUnionType with
                        | SynUnionCaseType.UnionCaseFullType(synType, _) -> getTypes synType
                        | SynUnionCaseType.UnionCaseFields fields ->
                            fields |> List.collect (function 
                                SynField.Field(_,_,_, synType,_,_,_, _) -> getTypes synType)
                    (Identificator ident.idText, range) :: types)
        | _ -> []
    | SynTypeDefnRepr.ObjectModel (_kind, members, _range) ->
        members |> List.collect (function 
        | SynMemberDefn.ImplicitCtor (_,_, args,_, _range) -> 
            args |> List.collect (visitSimplePattern)
            |> List.filter (function |TypeSymbol _, _ -> true |_ -> false)
        | _ -> [])
    | _ -> []

let getNamespace path =
    path |> List.choose (function
        | TraverseStep.ModuleOrNamespace(SynModuleOrNamespace(lId,_,_,_,_,_,_,_)) -> 
            Some (visitLongIdent lId)
        | TraverseStep.Module(SynModuleDecl.NestedModule(ComponentInfo(_,_,_,lId,_,_,_,_),_,_,_,_)) -> 
            Some (visitLongIdent lId)
        | TraverseStep.TypeDefn(SynTypeDefn.TypeDefn(ComponentInfo(_,_,_,lId,_,_,_,_),_,_,_)) -> 
            Some (visitLongIdent lId)
        | _ -> None
    ) |> List.rev |> String.concat "."
let getTypeDefnFromPath path =
    path |> List.rev |> List.choose (function
        | TraverseStep.TypeDefn(t) -> Some t
        | _ -> None
    ) |> List.tryHead

let getDefSymbols (tree: ParsedInput) =
    let mutable xs = []
    
    let getFieldsFromTypeDefn = getFieldsAndTypesFromTypeDefn >> List.filter (function |TypeSymbol _, _ -> false |_ -> true) >> List.map fst
    let getBind = getBind >> List.filter (function |TypeSymbol _, _ -> false |_ -> true) >> List.map fst
    
    let visitor = { new AstVisitorBase<_>() with    
        override __.VisitExpr(_, subExprF, defF, e) =
            match e with
            | _ -> defF e
        override __.VisitBinding(path, defF, x) = 
            match path with
            | TraverseStep.Expr _ :: _ -> defF x
            | _ ->
                xs <- xs @ (getBind [x] |> List.map (Symbol.map (fun x -> getNamespace path + "." + x) >> fun x -> x, None)); defF x
        override __.VisitComponentInfo(path, _) =
            let fields = path |> getTypeDefnFromPath |> Option.map getFieldsFromTypeDefn |> Option.defaultValue []
            let symbolCons x = 
                path |> getTypeDefnFromPath |> function 
                    | Some (SynTypeDefn.TypeDefn(_, SynTypeDefnRepr.ObjectModel _, _, range)) -> Identificator x, Some range
                    | _ -> TypeSymbol x, None
            xs <- xs @ [getNamespace path |> symbolCons] 
                @ (fields |> List.map (Symbol.map (fun s -> (getNamespace path |> Namespace.removeLastPart) + "." + s)  >> fun x -> x, None)); None
        }
    Traverse(tree, visitor) |> ignore
    //printfn "Defs: %A" xs    
    xs |> List.map (fun (x,r) -> { SymbolName = x; LocalRange = r })

let getUsedSymbols (tree: ParsedInput) =
    let getTypesFromTypeDefn = getFieldsAndTypesFromTypeDefn >> List.filter (function |TypeSymbol _, _ -> true |_ -> false)
    let getBind = getBind >> List.filter (function |TypeSymbol _, _ -> true |_ -> false)
    
    let mutable xs = []
    let visitor = { new AstVisitorBase<_>() with
        override __.VisitExpr(path, subExprF, defF, e) =
            match e with
            | SynExpr.Ident(id) -> xs <- (Identificator id.idText, id.idRange) :: xs; defF e
            | SynExpr.LongIdent(_, LongIdentWithDots(lId,_), _, r) -> xs <- (Identificator(visitLongIdent lId), r) :: xs; defF e
            | _ -> defF e
        override __.VisitBinding(path, defF, x) = 
            match path with
            | TraverseStep.Expr _ :: _ -> defF x
            | _ ->
                xs <- xs @ (getBind [x]); defF x
        override __.VisitType(synType, range) =
            xs <- getTypes synType @ xs; None
        override __.VisitRecordField(_path, _, ident, range) =
            ident |> Option.iter (fun (LongIdentWithDots(ident, _)) ->
                xs <- xs @ [RecordField(visitLongIdent ident), range])
            None
        override __.VisitComponentInfo(path, _) =
            let types = path |> getTypeDefnFromPath |> Option.map getTypesFromTypeDefn |> Option.defaultValue []
            xs <- xs @ types; None
        }
    Traverse(tree, visitor) |> ignore
    //printfn "Uses: %A" xs    
    xs |> List.map (fun (x,r) -> {SymbolName = x; Range = r})

let getOpenDecls (defs: SymbolDef list) (tree: ParsedInput) =
    //TODO: open in module with scope
    let localDefs = defs |> List.choose (fun d -> d.LocalRange |> Option.map (fun r -> d.SymbolName, r))
    let getUsesInRange range = 
        getUsedSymbols tree |> List.filter (fun u -> 
            Range.rangeContainsRange range u.Range
            && not (localDefs |> List.exists (fun (s, r) -> u.SymbolName = s && Range.rangeContainsRange r u.Range)))
    let mkOpenDecl xs uses = { Opens = xs; UsedSymbols  = uses }
    let getScope path =
        path |> List.choose (function
            | TraverseStep.ModuleOrNamespace(SynModuleOrNamespace(_,_,_,_,_,_,_,r))
            | TraverseStep.Module(SynModuleDecl.NestedModule(_,_,_,_,r)) -> Some r
            | _ -> None
        ) |> List.tryHead
    let getNamespace path =
        path |> List.choose (function
            | TraverseStep.ModuleOrNamespace(SynModuleOrNamespace(lId,_,isModule,_,_,_,_,_)) -> 
                Some (if isModule then visitLongIdent lId |> Namespace.removeLastPart else visitLongIdent lId)
            | _ -> None
        ) |> List.tryHead
    let getFullNamespace path =
        path |> List.choose (function
            | TraverseStep.ModuleOrNamespace(SynModuleOrNamespace(lId,_,_,_,_,_,_,_))
            | TraverseStep.Module(SynModuleDecl.NestedModule(ComponentInfo(_,_,_,lId,_,_,_,_),_,_,_,_)) -> Some (visitLongIdent lId)
            | _ -> None
        ) |> List.rev |> Namespace.joinByDot
    let openWithNamespace path x = getNamespace path |> Option.map (fun n -> Namespace.merge n x) |> Option.defaultValue x
    let openWithFullNamespace path x = getFullNamespace path |> fun n -> Namespace.merge n x
    let mutable xs = []
    let visitor = { new AstVisitorBase<_>() with
        override __.VisitExpr(_, subExprF, defF, e) =
            match e with | _ -> defF e
        override __.VisitModuleDecl(path, defF, d) =
            match d with
            | SynModuleDecl.Open(LongIdentWithDots(lId, _),r) -> xs <- ((visitLongIdent lId |> openWithNamespace path), r.Start, getScope path |> Option.get) :: xs; defF d
            | SynModuleDecl.NestedModule(ComponentInfo(_,_,_,lId,_,_,_,_),_,_,_,r) -> xs <- (visitLongIdent lId |> openWithFullNamespace path, r.Start, r) :: xs; defF d
            | _ -> defF d
        override __.VisitModuleOrNamespace(SynModuleOrNamespace(lId,_,isModule,_,_,_,_,r)) =
            let ident = visitLongIdent lId
            if isModule then xs <- (Namespace.removeLastPart ident, r.Start, r) :: xs 
            xs <- (ident, r.Start, r) :: xs
            None
        }
    Traverse(tree, visitor) |> ignore
    let opensAndUses = xs |> List.map (fun (x, pos, openR) -> { OpenName = x; Pos = pos; Range = openR }, getUsesInRange openR)
    let opensWithNoUse = opensAndUses |> List.filter (fun (_,uses) -> List.isEmpty uses)
    let usesWithOpens =
        opensAndUses |> List.collect (fun (openD, uses) -> uses |> List.map (fun u -> u, openD))
        |> List.groupBy (fun (u,_) -> u.SymbolName, u.Range) |> List.map (fun ((u,_), xs) -> 
            let opensWithRange = xs |> List.map snd 
            u, (opensWithRange |> List.sortBy (fun o -> o.Pos.Line, o.Pos.Column) |> List.map (fun o -> o.OpenName)))
    //printfn "UsesWithOpens: %A" usesWithOpens
    let r =
        let openGroups = 
            usesWithOpens 
            |> List.groupBy snd |> List.map (fun (opens,g) -> mkOpenDecl (List.rev opens) (g |> List.map fst))
        openGroups @ [opensWithNoUse |> List.map (fun (o,_) -> o.OpenName) |> fun x -> mkOpenDecl x []]
    //printfn "Opens: %A" r
    r
