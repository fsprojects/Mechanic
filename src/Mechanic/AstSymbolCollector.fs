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

type SymbolDef = { SymbolName: Symbol; LocalRange: Range.range option }
type SymbolUse = { SymbolName: Symbol; Range: Range.range }

type DefOrUse = 
    | Def of SymbolDef 
    | Use of SymbolUse
module DefOrUse =
    let map f = function
        | Def s -> Def { s with SymbolName = Symbol.map f s.SymbolName}
        | Use s -> Use { s with SymbolName = Symbol.map f s.SymbolName}

let mkDef symbol = Def { SymbolName = symbol; LocalRange = None }
let mkLocalDef symbol range = Def { SymbolName = symbol; LocalRange = Some range }
let defToLocal range = function
    | Def { SymbolName = s } -> Def { SymbolName = s; LocalRange = Some range }
    | Use x -> Use x
let defSetRange range = function
    | Def { SymbolName = s } -> Def { SymbolName = s; LocalRange = range }
    | Use x -> Use x
let mkUse symbol range = Use { SymbolName = symbol; Range = range }

let filterDefs xs = xs |> List.choose (function | Def x -> Some x | Use _ -> None)
let filterUses xs = xs |> List.choose (function | Use x -> Some x | Def _ -> None)

type OpenDecl = { OpenName: string; Pos: Range.pos; Range: Range.range; IsAutoOpen: bool }
type OpenDeclGroup = { Opens: list<OpenDecl>; UsedSymbols: list<Symbol> }

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
let getTypesUse synType = getTypes synType |> List.map (fun (x,r) -> mkUse x r)
let getTypesDef synType = getTypes synType |> List.map (fun (x,_) -> mkDef x)

let rec getSynConstructorArgs localRange = function
    | SynConstructorArgs.Pats ps -> ps |> List.collect (visitPattern localRange)
    | SynConstructorArgs.NamePatPairs (ps, _) -> ps |> List.map snd |> List.collect (visitPattern localRange)

and visitPattern localRange = function
    | SynPat.Named(SynPat.Wild(_), name, _, _, range) -> [mkDef <| Identificator name.idText]
    | SynPat.OptionalVal(name, range)
    | SynPat.Named(_, name, _, _, range) -> [mkDef <| Identificator name.idText]
    | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, args, _, range) -> 
        (getSynConstructorArgs localRange args |> List.map (defToLocal localRange))
        @ [mkDef <| Identificator (visitLongIdent ident)]
    | SynPat.Typed(p, typ, _) -> visitPattern localRange p @ getTypesUse typ
    | SynPat.Paren(p, _) -> visitPattern localRange p
    | SynPat.Ands(ps, _)
    | SynPat.ArrayOrList(_ , ps, _)
    | SynPat.Tuple(ps, _) -> ps |> List.collect (visitPattern localRange)
    | SynPat.Or(p1, p2, _) -> visitPattern localRange p1 @ visitPattern localRange p2
    | _ -> []

let rec visitSimplePattern localRange = function
    | SynSimplePat.Id(ident, _, _, _, _, range) -> [mkLocalDef (Identificator ident.idText) localRange]
    | SynSimplePat.Typed(p, typ, _) -> visitSimplePattern localRange p @ getTypesUse typ
    | _ -> []

let rec getBind localRange bindings =
    bindings |> Seq.collect (fun binding ->
        let (Binding(_, _, _, _, _, _, _, pat, _, _, _, _)) = binding
        visitPattern localRange pat)
    |> Seq.toList

let getSymbolsFromTypeDefn (SynTypeDefn.TypeDefn(SynComponentInfo.ComponentInfo _, repr,_,_)) =
    match repr with
    | SynTypeDefnRepr.Simple(simpleRepr,_) ->
        match simpleRepr with
        | SynTypeDefnSimpleRepr.Record(_,fields,_) -> 
            fields |> List.collect (function 
                SynField.Field(_,_,ident, synType,_,_,_, range) -> 
                    (ident |> Option.toList |> List.map (fun ident -> mkDef (RecordField ident.idText))) 
                    @ (getTypesUse synType))
        | SynTypeDefnSimpleRepr.Union(Some SynAccess.Private,_,_) -> []
        | SynTypeDefnSimpleRepr.Union(_,cases,_) ->
            cases |> List.collect (function 
                SynUnionCase.UnionCase(_,ident,synUnionType,_,_,range) -> 
                    let types =
                        match synUnionType with
                        | SynUnionCaseType.UnionCaseFullType(synType, _) -> getTypesUse synType
                        | SynUnionCaseType.UnionCaseFields fields ->
                            fields |> List.collect (function 
                                SynField.Field(_,_,_, synType,_,_,_, _) -> getTypesUse synType)
                    mkDef (Identificator ident.idText) :: types)
        | _ -> []
    | SynTypeDefnRepr.ObjectModel (_kind, members, _range) ->
        members |> List.collect (function 
        | SynMemberDefn.ImplicitCtor (_,_, args,_, range) -> 
            args |> List.collect (visitSimplePattern range)
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
let getStepRange = function
    | TraverseStep.ModuleOrNamespace m -> m.Range
    | TraverseStep.Module m -> m.Range
    | TraverseStep.TypeDefn d -> d.Range
    | TraverseStep.Expr e -> e.Range
    | TraverseStep.MatchClause c -> c.Range
    | TraverseStep.Binding b -> b.RangeOfBindingAndRhs
    | TraverseStep.MemberDefn m -> m.Range
let getParentRangeFromPath path =
    match path |> List.rev with
    | [] 
    | [_] -> None
    | _::(x::_) -> 
        Some (getStepRange x)

let getBinding path localRange x =
    match path with
        | TraverseStep.Expr _ :: _ -> 
            getParentRangeFromPath path |> Option.map (fun r -> 
                getBind localRange [x] |> List.map (defToLocal <| Range.mkRange r.FileName x.RangeOfHeadPat.Start r.End)) |> Option.defaultValue []
        | _ -> getBind localRange [x]

let getDefSymbols (tree: ParsedInput) =
    let mutable xs = []

    let withNamespace path x = x |> DefOrUse.map (fun s -> getNamespace path + "." + s)
    let withNamespaceExceptLastPart path x = x |> DefOrUse.map (fun s -> (getNamespace path |> Namespace.removeLastPart) + "." + s)
    
    let visitor = { new AstVisitorBase<_>() with    
        override __.VisitExpr(_, subExprF, defF, e) =
            match e with
            | _ -> defF e
        
        override __.VisitLetOrUse(path, bindings, range) = 
            xs <- xs @ (bindings |> List.collect (fun x -> x |> getBinding path range |> List.map (withNamespace path)))
            None
            
        override __.VisitComponentInfo(path, _) =
            let fields = path |> getTypeDefnFromPath |> Option.map getSymbolsFromTypeDefn |> Option.defaultValue []
            let localRange = 
                path |> getTypeDefnFromPath |> function 
                    | Some (SynTypeDefn.TypeDefn(_, SynTypeDefnRepr.ObjectModel _, _, range)) -> Some range
                    | _ -> None
            let symbolCons = 
                path |> getTypeDefnFromPath |> function 
                    | Some (SynTypeDefn.TypeDefn(_, SynTypeDefnRepr.ObjectModel _, _, _)) -> Identificator
                    | _ -> TypeSymbol
            let typeDef = getNamespace path |> symbolCons |> mkDef

            xs <- xs @ [typeDef] @ (fields |> List.map (fun field -> field |> withNamespaceExceptLastPart path |> defSetRange localRange))
            None
        }
    Traverse(tree, visitor) |> ignore
    let defs = xs |> filterDefs
    //printfn "Defs: %A" defs
    defs

let getUsedSymbols (tree: ParsedInput) =
    let mutable xs = []
    let visitor = { new AstVisitorBase<_>() with
        override __.VisitExpr(path, subExprF, defF, e) =
            match e with
            | SynExpr.Ident(id) -> xs <- mkUse (Identificator id.idText) id.idRange :: xs; defF e
            | SynExpr.LongIdent(_, LongIdentWithDots(lId,_), _, r) -> xs <- mkUse (Identificator(visitLongIdent lId)) r :: xs; defF e
            | _ -> defF e
        override __.VisitLetOrUse(path, bindings, range) = 
            xs <- xs @ (bindings |> List.collect (getBinding path range))
            None
        override __.VisitType(synType, range) =
            xs <- getTypesUse synType @ xs; None
        override __.VisitRecordField(_path, _, ident, range) =
            ident |> Option.iter (fun (LongIdentWithDots(ident, _)) ->
                xs <- xs @ [mkUse (RecordField(visitLongIdent ident)) range])
            None
        override __.VisitComponentInfo(path, _) =
            let types = path |> getTypeDefnFromPath |> Option.map getSymbolsFromTypeDefn |> Option.defaultValue []
            xs <- xs @ types; None
        }
    Traverse(tree, visitor) |> ignore
    let uses = xs |> filterUses
    //printfn "Uses: %A" uses
    uses

let getOpenDecls (defs: SymbolDef list) (tree: ParsedInput) =
    //TODO: open in module with scope
    let localDefs = defs |> List.choose (fun d -> d.LocalRange |> Option.map (fun r -> d.SymbolName |> Symbol.map Namespace.lastPart, r))
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
    let mkOpenWith openName (range: Range.range) path = 
        { OpenName = openName; Pos = range.Start; Range = getScope path |> Option.get; IsAutoOpen = false}
    let mutable xs = []
    let visitor = { new AstVisitorBase<_>() with
        override __.VisitExpr(_, subExprF, defF, e) =
            match e with | _ -> defF e
        override __.VisitModuleDecl(path, defF, d) =
            match d with
            | SynModuleDecl.Open(LongIdentWithDots(lId, _),r) -> 
                xs <- mkOpenWith (visitLongIdent lId |> openWithNamespace path) r path :: xs
                xs <- mkOpenWith (visitLongIdent lId) r path :: xs
                defF d
            | SynModuleDecl.NestedModule(ComponentInfo(_,_,_,lId,_,_,_,_),_,_,_,r) -> 
                xs <- mkOpenWith (visitLongIdent lId |> openWithFullNamespace path) r path :: xs 
                defF d
            | _ -> defF d
        override __.VisitModuleOrNamespace(SynModuleOrNamespace(lId,_,isModule,_,_,attrs,_,r)) =
            let ident = visitLongIdent lId
            let isAutoOpen = attrs |> List.exists (fun  attr -> visitLongIdent attr.TypeName.Lid = "AutoOpen")
            if isModule then xs <- { OpenName = Namespace.removeLastPart ident; Pos = r.Start; Range = r; IsAutoOpen = false } :: xs 
            xs <- { OpenName = ident; Pos = r.Start; Range = r; IsAutoOpen = isAutoOpen } :: xs 
            None
        }
    Traverse(tree, visitor) |> ignore
    let opensAndUses = xs |> List.map (fun x -> x, getUsesInRange x.Range)
    let opensWithNoUse = opensAndUses |> List.filter (fun (_,uses) -> List.isEmpty uses)
    let usesWithOpens =
        opensAndUses |> List.collect (fun (openD, uses) -> uses |> List.map (fun u -> u, openD))
        |> List.groupBy (fun (u,_) -> u.SymbolName, u.Range) |> List.map (fun ((u,_), xs) -> 
            let opensWithRange = xs |> List.map snd 
            u, (opensWithRange |> List.sortBy (fun o -> (if o.IsAutoOpen then 0 else 1), o.Pos.Line, o.Pos.Column)))
    //printfn "UsesWithOpens: %A" usesWithOpens
    let r =
        let openGroups = 
            usesWithOpens 
            |> List.groupBy snd |> List.map (fun (opens,g) -> mkOpenDecl (List.rev opens) (g |> List.map fst))
        openGroups @ [opensWithNoUse |> List.map fst |> fun x -> mkOpenDecl x []]
    //let autoOpens = xs |> List.filter (fun x -> x.IsAutoOpen) |> List.map (fun x -> x.OpenName)
    //printfn "Opens: %A" (r, autoOpens)
    r
