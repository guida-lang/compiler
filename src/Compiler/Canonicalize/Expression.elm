module Compiler.Canonicalize.Expression exposing
    ( EResult
    , FreeLocals
    , Uses(..)
    , canonicalize
    , gatherTypedArgs
    , verifyBindings
    )

import Basics.Extra exposing (flip)
import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.AST.Utils.Binop as Binop
import Compiler.AST.Utils.Type as Type
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Canonicalize.Pattern as Pattern
import Compiler.Canonicalize.Type as Type
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as R
import Compiler.Reporting.Warning as W
import Data.Graph as Graph
import Data.Map as Dict exposing (Dict)
import Prelude
import System.TypeCheck.IO as IO
import Utils.Main as Utils



-- RESULTS


type alias EResult i w a =
    R.RResult i w Error.Error a


type alias FreeLocals =
    Dict String Name.Name Uses


type Uses
    = Uses
        { direct : Int
        , delayed : Int
        }



-- CANONICALIZE


canonicalize : SyntaxVersion -> Env.Env -> Src.Expr -> EResult FreeLocals (List W.Warning) Can.Expr
canonicalize syntaxVersion env (A.At region expression) =
    R.fmap (A.At region) <|
        case expression of
            Src.Str string _ ->
                R.ok (Can.Str string)

            Src.Chr char ->
                R.ok (Can.Chr char)

            Src.Int int _ ->
                R.ok (Can.Int int)

            Src.Float float _ ->
                R.ok (Can.Float float)

            Src.Var varType name ->
                case varType of
                    Src.LowVar ->
                        findVar region env name

                    Src.CapVar ->
                        R.fmap (toVarCtor name) (Env.findCtor region env name)

            Src.VarQual varType prefix name ->
                case varType of
                    Src.LowVar ->
                        findVarQual region env prefix name

                    Src.CapVar ->
                        R.fmap (toVarCtor name) (Env.findCtorQual region env prefix name)

            Src.List exprs _ ->
                R.fmap Can.List (R.traverse (canonicalize syntaxVersion env) (List.map Tuple.second exprs))

            Src.Op op ->
                Env.findBinop region env op
                    |> R.fmap
                        (\(Env.Binop _ home name annotation _ _) ->
                            Can.VarOperator op home name annotation
                        )

            Src.Negate expr ->
                R.fmap Can.Negate (canonicalize syntaxVersion env expr)

            Src.Binops ops final ->
                R.fmap A.toValue (canonicalizeBinops syntaxVersion region env (List.map (Tuple.mapSecond Src.c2Value) ops) final)

            Src.Lambda ( _, srcArgs ) ( _, body ) ->
                delayedUsage <|
                    (Pattern.verify Error.DPLambdaArgs
                        (R.traverse (Pattern.canonicalize syntaxVersion env) (List.map Src.c1Value srcArgs))
                        |> R.bind
                            (\( args, bindings ) ->
                                Env.addLocals bindings env
                                    |> R.bind
                                        (\newEnv ->
                                            verifyBindings W.Pattern bindings (canonicalize syntaxVersion newEnv body)
                                                |> R.fmap
                                                    (\( cbody, freeLocals ) ->
                                                        ( Can.Lambda args cbody, freeLocals )
                                                    )
                                        )
                            )
                    )

            Src.Call func args ->
                R.fmap Can.Call (canonicalize syntaxVersion env func)
                    |> R.apply (R.traverse (canonicalize syntaxVersion env) (List.map Src.c1Value args))

            Src.If firstBranch branches finally ->
                R.fmap Can.If
                    (R.traverse (canonicalizeIfBranch syntaxVersion env)
                        (List.map (Src.c1Value >> Tuple.mapBoth Src.c2Value Src.c2Value) (firstBranch :: branches))
                    )
                    |> R.apply (canonicalize syntaxVersion env (Src.c1Value finally))

            Src.Let defs _ expr ->
                R.fmap A.toValue (canonicalizeLet syntaxVersion region env (List.map Src.c2Value defs) expr)

            Src.Case expr branches ->
                R.fmap Can.Case (canonicalize syntaxVersion env (Src.c2Value expr))
                    |> R.apply (R.traverse (canonicalizeCaseBranch syntaxVersion env) (List.map (Tuple.mapBoth Src.c2Value Src.c1Value) branches))

            Src.Accessor field ->
                R.pure (Can.Accessor field)

            Src.Access record field ->
                R.fmap Can.Access (canonicalize syntaxVersion env record)
                    |> R.apply (R.ok field)

            Src.Update ( _, name ) ( _, fields ) ->
                let
                    makeCanFields : R.RResult i w Error.Error (Dict String (A.Located Name) (R.RResult FreeLocals (List W.Warning) Error.Error Can.FieldUpdate))
                    makeCanFields =
                        Dups.checkLocatedFields_ (\r t -> R.fmap (Can.FieldUpdate r) (canonicalize syntaxVersion env t)) (List.map (Src.c2EolValue >> Tuple.mapBoth Src.c1Value Src.c1Value) fields)
                in
                R.fmap Can.Update (canonicalize syntaxVersion env name)
                    |> R.apply (R.bind (Utils.sequenceADict A.toValue A.compareLocated) makeCanFields)

            Src.Record ( _, fields ) ->
                Dups.checkLocatedFields (List.map (Src.c2EolValue >> Tuple.mapBoth Src.c1Value Src.c1Value) fields)
                    |> R.bind
                        (\fieldDict ->
                            R.fmap Can.Record (R.traverseDict A.toValue A.compareLocated (canonicalize syntaxVersion env) fieldDict)
                        )

            Src.Unit ->
                R.ok Can.Unit

            Src.Tuple ( _, a ) ( _, b ) cs ->
                R.fmap Can.Tuple (canonicalize syntaxVersion env a)
                    |> R.apply (canonicalize syntaxVersion env b)
                    |> R.apply (canonicalizeTupleExtras syntaxVersion region env (List.map Src.c2Value cs))

            Src.Shader src tipe ->
                R.ok (Can.Shader src tipe)

            Src.Parens ( _, expr ) ->
                R.fmap A.toValue (canonicalize syntaxVersion env expr)


canonicalizeTupleExtras : SyntaxVersion -> A.Region -> Env.Env -> List Src.Expr -> EResult FreeLocals (List W.Warning) (List Can.Expr)
canonicalizeTupleExtras syntaxVersion region env extras =
    case extras of
        [] ->
            R.ok []

        [ three ] ->
            R.fmap List.singleton <| canonicalize syntaxVersion env three

        _ ->
            case syntaxVersion of
                SV.Elm ->
                    R.throw (Error.TupleLargerThanThree region)

                SV.Guida ->
                    R.traverse (canonicalize syntaxVersion env) extras



-- CANONICALIZE IF BRANCH


canonicalizeIfBranch : SyntaxVersion -> Env.Env -> ( Src.Expr, Src.Expr ) -> EResult FreeLocals (List W.Warning) ( Can.Expr, Can.Expr )
canonicalizeIfBranch syntaxVersion env ( condition, branch ) =
    R.fmap Tuple.pair (canonicalize syntaxVersion env condition)
        |> R.apply (canonicalize syntaxVersion env branch)



-- CANONICALIZE CASE BRANCH


canonicalizeCaseBranch : SyntaxVersion -> Env.Env -> ( Src.Pattern, Src.Expr ) -> EResult FreeLocals (List W.Warning) Can.CaseBranch
canonicalizeCaseBranch syntaxVersion env ( pattern, expr ) =
    directUsage
        (Pattern.verify Error.DPCaseBranch
            (Pattern.canonicalize syntaxVersion env pattern)
            |> R.bind
                (\( cpattern, bindings ) ->
                    Env.addLocals bindings env
                        |> R.bind
                            (\newEnv ->
                                verifyBindings W.Pattern bindings (canonicalize syntaxVersion newEnv expr)
                                    |> R.fmap
                                        (\( cexpr, freeLocals ) ->
                                            ( Can.CaseBranch cpattern cexpr, freeLocals )
                                        )
                            )
                )
        )



-- CANONICALIZE BINOPS


canonicalizeBinops : SyntaxVersion -> A.Region -> Env.Env -> List ( Src.Expr, A.Located Name.Name ) -> Src.Expr -> EResult FreeLocals (List W.Warning) Can.Expr
canonicalizeBinops syntaxVersion overallRegion env ops final =
    let
        canonicalizeHelp : ( Src.Expr, A.Located Name ) -> R.RResult FreeLocals (List W.Warning) Error.Error ( Can.Expr, Env.Binop )
        canonicalizeHelp ( expr, A.At region op ) =
            R.fmap Tuple.pair (canonicalize syntaxVersion env expr)
                |> R.apply (Env.findBinop region env op)
    in
    R.bind (runBinopStepper overallRegion)
        (R.fmap More (R.traverse canonicalizeHelp ops)
            |> R.apply (canonicalize syntaxVersion env final)
        )


type Step
    = Done Can.Expr
    | More (List ( Can.Expr, Env.Binop )) Can.Expr
    | Error Env.Binop Env.Binop


runBinopStepper : A.Region -> Step -> EResult FreeLocals w Can.Expr
runBinopStepper overallRegion step =
    case step of
        Done expr ->
            R.ok expr

        More [] expr ->
            R.ok expr

        More (( expr, op ) :: rest) final ->
            runBinopStepper overallRegion <|
                toBinopStep (toBinop op expr) op rest final

        Error (Env.Binop op1 _ _ _ _ _) (Env.Binop op2 _ _ _ _ _) ->
            R.throw (Error.Binop overallRegion op1 op2)


toBinopStep : (Can.Expr -> Can.Expr) -> Env.Binop -> List ( Can.Expr, Env.Binop ) -> Can.Expr -> Step
toBinopStep makeBinop ((Env.Binop _ _ _ _ rootAssociativity rootPrecedence) as rootOp) middle final =
    case middle of
        [] ->
            Done (makeBinop final)

        ( expr, (Env.Binop _ _ _ _ associativity precedence) as op ) :: rest ->
            if precedence < rootPrecedence then
                More (( makeBinop expr, op ) :: rest) final

            else if precedence > rootPrecedence then
                case toBinopStep (toBinop op expr) op rest final of
                    Done newLast ->
                        Done (makeBinop newLast)

                    More newMiddle newLast ->
                        toBinopStep makeBinop rootOp newMiddle newLast

                    Error a b ->
                        Error a b

            else
                case ( rootAssociativity, associativity ) of
                    ( Binop.Left, Binop.Left ) ->
                        toBinopStep (toBinop op (makeBinop expr)) op rest final

                    ( Binop.Right, Binop.Right ) ->
                        toBinopStep (makeBinop << toBinop op expr) op rest final

                    _ ->
                        Error rootOp op


toBinop : Env.Binop -> Can.Expr -> Can.Expr -> Can.Expr
toBinop (Env.Binop op home name annotation _ _) left right =
    A.merge left right (Can.Binop op home name annotation left right)


canonicalizeLet : SyntaxVersion -> A.Region -> Env.Env -> List (A.Located Src.Def) -> Src.Expr -> EResult FreeLocals (List W.Warning) Can.Expr
canonicalizeLet syntaxVersion letRegion env defs body =
    directUsage <|
        (Dups.detect (Error.DuplicatePattern Error.DPLetBinding)
            (List.foldl addBindings Dups.none defs)
            |> R.bind
                (\bindings ->
                    Env.addLocals bindings env
                        |> R.bind
                            (\newEnv ->
                                verifyBindings W.Def bindings <|
                                    (Utils.foldM (addDefNodes syntaxVersion newEnv) [] defs
                                        |> R.bind
                                            (\nodes ->
                                                canonicalize syntaxVersion newEnv body
                                                    |> R.bind
                                                        (\cbody ->
                                                            detectCycles letRegion (Graph.stronglyConnComp nodes) cbody
                                                        )
                                            )
                                    )
                            )
                )
        )


addBindings : A.Located Src.Def -> Dups.Tracker A.Region -> Dups.Tracker A.Region
addBindings (A.At _ def) bindings =
    case def of
        Src.Define (A.At region name) _ _ _ ->
            Dups.insert name region region bindings

        Src.Destruct pattern _ ->
            addBindingsHelp bindings pattern


addBindingsHelp : Dups.Tracker A.Region -> Src.Pattern -> Dups.Tracker A.Region
addBindingsHelp bindings (A.At region pattern) =
    case pattern of
        Src.PAnything _ ->
            bindings

        Src.PVar name ->
            Dups.insert name region region bindings

        Src.PRecord ( _, fields ) ->
            let
                addField : Src.C2 (A.Located Name) -> Dups.Tracker A.Region -> Dups.Tracker A.Region
                addField ( _, A.At fieldRegion name ) dict =
                    Dups.insert name fieldRegion fieldRegion dict
            in
            List.foldl addField bindings fields

        Src.PUnit _ ->
            bindings

        Src.PTuple a b cs ->
            List.foldl (flip addBindingsHelp) bindings (List.map Src.c2Value (a :: b :: cs))

        Src.PCtor _ _ patterns ->
            List.foldl (flip addBindingsHelp) bindings (List.map Src.c1Value patterns)

        Src.PCtorQual _ _ _ patterns ->
            List.foldl (flip addBindingsHelp) bindings (List.map Src.c1Value patterns)

        Src.PList ( _, patterns ) ->
            List.foldl (flip addBindingsHelp) bindings (List.map Src.c2Value patterns)

        Src.PCons ( _, hd ) ( _, tl ) ->
            addBindingsHelp (addBindingsHelp bindings hd) tl

        Src.PAlias ( _, aliasPattern ) ( _, A.At nameRegion name ) ->
            Dups.insert name nameRegion nameRegion <|
                addBindingsHelp bindings aliasPattern

        Src.PChr _ ->
            bindings

        Src.PStr _ _ ->
            bindings

        Src.PInt _ _ ->
            bindings

        Src.PParens ( _, parensPattern ) ->
            addBindingsHelp bindings parensPattern


type alias Node =
    ( Binding, Name.Name, List Name.Name )


type Binding
    = Define Can.Def
    | Edge (A.Located Name.Name)
    | Destruct Can.Pattern Can.Expr


addDefNodes : SyntaxVersion -> Env.Env -> List Node -> A.Located Src.Def -> EResult FreeLocals (List W.Warning) (List Node)
addDefNodes syntaxVersion env nodes (A.At _ def) =
    case def of
        Src.Define ((A.At _ name) as aname) srcArgs ( _, body ) maybeType ->
            case maybeType of
                Nothing ->
                    Pattern.verify (Error.DPFuncArgs name)
                        (R.traverse (Pattern.canonicalize syntaxVersion env) (List.map Src.c1Value srcArgs))
                        |> R.bind
                            (\( args, argBindings ) ->
                                Env.addLocals argBindings env
                                    |> R.bind
                                        (\newEnv ->
                                            verifyBindings W.Pattern argBindings (canonicalize syntaxVersion newEnv body)
                                                |> R.bind
                                                    (\( cbody, freeLocals ) ->
                                                        let
                                                            cdef : Can.Def
                                                            cdef =
                                                                Can.Def aname args cbody

                                                            node : ( Binding, Name, List Name )
                                                            node =
                                                                ( Define cdef, name, Dict.keys compare freeLocals )
                                                        in
                                                        logLetLocals args freeLocals (node :: nodes)
                                                    )
                                        )
                            )

                Just ( _, ( _, tipe ) ) ->
                    Type.toAnnotation syntaxVersion env tipe
                        |> R.bind
                            (\(Can.Forall freeVars ctipe) ->
                                Pattern.verify (Error.DPFuncArgs name)
                                    (gatherTypedArgs syntaxVersion env name (List.map Src.c1Value srcArgs) ctipe Index.first [])
                                    |> R.bind
                                        (\( ( args, resultType ), argBindings ) ->
                                            Env.addLocals argBindings env
                                                |> R.bind
                                                    (\newEnv ->
                                                        verifyBindings W.Pattern argBindings (canonicalize syntaxVersion newEnv body)
                                                            |> R.bind
                                                                (\( cbody, freeLocals ) ->
                                                                    let
                                                                        cdef : Can.Def
                                                                        cdef =
                                                                            Can.TypedDef aname freeVars args cbody resultType

                                                                        node : ( Binding, Name, List Name )
                                                                        node =
                                                                            ( Define cdef, name, Dict.keys compare freeLocals )
                                                                    in
                                                                    logLetLocals args freeLocals (node :: nodes)
                                                                )
                                                    )
                                        )
                            )

        Src.Destruct pattern ( _, body ) ->
            Pattern.verify Error.DPDestruct
                (Pattern.canonicalize syntaxVersion env pattern)
                |> R.bind
                    (\( cpattern, _ ) ->
                        R.RResult
                            (\fs ws ->
                                case canonicalize syntaxVersion env body of
                                    R.RResult k ->
                                        case k Dict.empty ws of
                                            R.ROk freeLocals warnings cbody ->
                                                let
                                                    names : List (A.Located Name)
                                                    names =
                                                        getPatternNames [] pattern

                                                    name : Name
                                                    name =
                                                        Name.fromManyNames (List.map A.toValue names)

                                                    node : ( Binding, Name, List Name )
                                                    node =
                                                        ( Destruct cpattern cbody, name, Dict.keys compare freeLocals )
                                                in
                                                R.ROk
                                                    (Utils.mapUnionWith identity compare combineUses fs freeLocals)
                                                    warnings
                                                    (List.foldl (addEdge [ name ]) (node :: nodes) names)

                                            R.RErr freeLocals warnings errors ->
                                                R.RErr (Utils.mapUnionWith identity compare combineUses freeLocals fs) warnings errors
                            )
                    )


logLetLocals : List arg -> FreeLocals -> value -> EResult FreeLocals w value
logLetLocals args letLocals value =
    R.RResult
        (\freeLocals warnings ->
            R.ROk
                (Utils.mapUnionWith identity
                    compare
                    combineUses
                    freeLocals
                    (case args of
                        [] ->
                            letLocals

                        _ ->
                            Dict.map (\_ -> delayUse) letLocals
                    )
                )
                warnings
                value
        )


addEdge : List Name.Name -> A.Located Name.Name -> List Node -> List Node
addEdge edges ((A.At _ name) as aname) nodes =
    ( Edge aname, name, edges ) :: nodes


getPatternNames : List (A.Located Name.Name) -> Src.Pattern -> List (A.Located Name.Name)
getPatternNames names (A.At region pattern) =
    case pattern of
        Src.PAnything _ ->
            names

        Src.PVar name ->
            A.At region name :: names

        Src.PRecord ( _, fields ) ->
            List.map Src.c2Value fields ++ names

        Src.PAlias ( _, ptrn ) ( _, name ) ->
            getPatternNames (name :: names) ptrn

        Src.PUnit _ ->
            names

        Src.PTuple ( _, a ) ( _, b ) cs ->
            List.foldl (flip getPatternNames) (getPatternNames (getPatternNames names a) b) (List.map Src.c2Value cs)

        Src.PCtor _ _ args ->
            List.foldl (flip getPatternNames) names (List.map Src.c1Value args)

        Src.PCtorQual _ _ _ args ->
            List.foldl (flip getPatternNames) names (List.map Src.c1Value args)

        Src.PList ( _, patterns ) ->
            List.foldl (flip getPatternNames) names (List.map Src.c2Value patterns)

        Src.PCons ( _, hd ) ( _, tl ) ->
            getPatternNames (getPatternNames names hd) tl

        Src.PChr _ ->
            names

        Src.PStr _ _ ->
            names

        Src.PInt _ _ ->
            names

        Src.PParens ( _, parensPattern ) ->
            getPatternNames names parensPattern


gatherTypedArgs :
    SyntaxVersion
    -> Env.Env
    -> Name.Name
    -> List Src.Pattern
    -> Can.Type
    -> Index.ZeroBased
    -> List ( Can.Pattern, Can.Type )
    -> EResult Pattern.DupsDict w ( List ( Can.Pattern, Can.Type ), Can.Type )
gatherTypedArgs syntaxVersion env name srcArgs tipe index revTypedArgs =
    case srcArgs of
        [] ->
            R.ok ( List.reverse revTypedArgs, tipe )

        srcArg :: otherSrcArgs ->
            case Type.iteratedDealias tipe of
                Can.TLambda argType resultType ->
                    Pattern.canonicalize syntaxVersion env srcArg
                        |> R.bind
                            (\arg ->
                                gatherTypedArgs syntaxVersion env name otherSrcArgs resultType (Index.next index) <|
                                    (( arg, argType ) :: revTypedArgs)
                            )

                _ ->
                    let
                        ( A.At start _, A.At end _ ) =
                            ( Prelude.head srcArgs, Prelude.last srcArgs )
                    in
                    R.throw (Error.AnnotationTooShort (A.mergeRegions start end) name index (List.length srcArgs))


detectCycles : A.Region -> List (Graph.SCC Binding) -> Can.Expr -> EResult i w Can.Expr
detectCycles letRegion sccs body =
    case sccs of
        [] ->
            R.ok body

        scc :: subSccs ->
            case scc of
                Graph.AcyclicSCC binding ->
                    case binding of
                        Define def ->
                            detectCycles letRegion subSccs body
                                |> R.fmap (Can.Let def)
                                |> R.fmap (A.At letRegion)

                        Edge _ ->
                            detectCycles letRegion subSccs body

                        Destruct pattern expr ->
                            detectCycles letRegion subSccs body
                                |> R.fmap (Can.LetDestruct pattern expr)
                                |> R.fmap (A.At letRegion)

                Graph.CyclicSCC bindings ->
                    R.fmap (A.At letRegion)
                        (R.fmap Can.LetRec (checkCycle bindings [])
                            |> R.apply (detectCycles letRegion subSccs body)
                        )


checkCycle : List Binding -> List Can.Def -> EResult i w (List Can.Def)
checkCycle bindings defs =
    case bindings of
        [] ->
            R.ok defs

        binding :: otherBindings ->
            case binding of
                Define ((Can.Def name args _) as def) ->
                    if List.isEmpty args then
                        R.throw (Error.RecursiveLet name (toNames otherBindings defs))

                    else
                        checkCycle otherBindings (def :: defs)

                Define ((Can.TypedDef name _ args _ _) as def) ->
                    if List.isEmpty args then
                        R.throw (Error.RecursiveLet name (toNames otherBindings defs))

                    else
                        checkCycle otherBindings (def :: defs)

                Edge name ->
                    R.throw (Error.RecursiveLet name (toNames otherBindings defs))

                Destruct _ _ ->
                    -- a Destruct cannot appear in a cycle without any Edge values
                    -- so we just keep going until we get to the edges
                    checkCycle otherBindings defs


toNames : List Binding -> List Can.Def -> List Name.Name
toNames bindings revDefs =
    case bindings of
        [] ->
            List.reverse (List.map getDefName revDefs)

        binding :: otherBindings ->
            case binding of
                Define def ->
                    getDefName def :: toNames otherBindings revDefs

                Edge (A.At _ name) ->
                    name :: toNames otherBindings revDefs

                Destruct _ _ ->
                    toNames otherBindings revDefs


getDefName : Can.Def -> Name.Name
getDefName def =
    case def of
        Can.Def (A.At _ name) _ _ ->
            name

        Can.TypedDef (A.At _ name) _ _ _ _ ->
            name


logVar : Name.Name -> a -> EResult FreeLocals w a
logVar name value =
    R.RResult <|
        \freeLocals warnings ->
            R.ROk (Utils.mapInsertWith identity combineUses name oneDirectUse freeLocals) warnings value


oneDirectUse : Uses
oneDirectUse =
    Uses
        { direct = 1
        , delayed = 0
        }


combineUses : Uses -> Uses -> Uses
combineUses (Uses ab) (Uses xy) =
    Uses
        { direct = ab.direct + xy.direct
        , delayed = ab.delayed + xy.delayed
        }


delayUse : Uses -> Uses
delayUse (Uses { direct, delayed }) =
    Uses
        { direct = 0
        , delayed = direct + delayed
        }



-- MANAGING BINDINGS


verifyBindings :
    W.Context
    -> Pattern.Bindings
    -> EResult FreeLocals (List W.Warning) value
    -> EResult info (List W.Warning) ( value, FreeLocals )
verifyBindings context bindings (R.RResult k) =
    R.RResult
        (\info warnings ->
            case k Dict.empty warnings of
                R.ROk freeLocals warnings1 value ->
                    let
                        outerFreeLocals : Dict String Name Uses
                        outerFreeLocals =
                            Dict.diff freeLocals bindings

                        warnings2 : List W.Warning
                        warnings2 =
                            -- NOTE: Uses Map.size for O(1) lookup. This means there is
                            -- no dictionary allocation unless a problem is detected.
                            if Dict.size bindings + Dict.size outerFreeLocals == Dict.size freeLocals then
                                warnings1

                            else
                                Dict.foldl compare (addUnusedWarning context) warnings1 <|
                                    Dict.diff bindings freeLocals
                    in
                    R.ROk info warnings2 ( value, outerFreeLocals )

                R.RErr _ warnings1 err ->
                    R.RErr info warnings1 err
        )


addUnusedWarning : W.Context -> Name.Name -> A.Region -> List W.Warning -> List W.Warning
addUnusedWarning context name region warnings =
    W.UnusedVariable region context name :: warnings


directUsage : EResult () w ( expr, FreeLocals ) -> EResult FreeLocals w expr
directUsage (R.RResult k) =
    R.RResult
        (\freeLocals warnings ->
            case k () warnings of
                R.ROk () ws ( value, newFreeLocals ) ->
                    R.ROk (Utils.mapUnionWith identity compare combineUses freeLocals newFreeLocals) ws value

                R.RErr () ws es ->
                    R.RErr freeLocals ws es
        )


delayedUsage : EResult () w ( expr, FreeLocals ) -> EResult FreeLocals w expr
delayedUsage (R.RResult k) =
    R.RResult
        (\freeLocals warnings ->
            case k () warnings of
                R.ROk () ws ( value, newFreeLocals ) ->
                    let
                        delayedLocals : Dict String Name Uses
                        delayedLocals =
                            Dict.map (\_ -> delayUse) newFreeLocals
                    in
                    R.ROk (Utils.mapUnionWith identity compare combineUses freeLocals delayedLocals) ws value

                R.RErr () ws es ->
                    R.RErr freeLocals ws es
        )



-- FIND VARIABLE


findVar : A.Region -> Env.Env -> Name -> EResult FreeLocals w Can.Expr_
findVar region env name =
    case Dict.get identity name env.vars of
        Just var ->
            case var of
                Env.Local _ ->
                    logVar name (Can.VarLocal name)

                Env.TopLevel _ ->
                    logVar name (Can.VarTopLevel env.home name)

                Env.Foreign home annotation ->
                    R.ok
                        (if home == ModuleName.debug then
                            Can.VarDebug env.home name annotation

                         else
                            Can.VarForeign home name annotation
                        )

                Env.Foreigns h hs ->
                    R.throw (Error.AmbiguousVar region Nothing name h hs)

        Nothing ->
            R.throw (Error.NotFoundVar region Nothing name (toPossibleNames env.vars env.q_vars))


findVarQual : A.Region -> Env.Env -> Name -> Name -> EResult FreeLocals w Can.Expr_
findVarQual region env prefix name =
    case Dict.get identity prefix env.q_vars of
        Just qualified ->
            case Dict.get identity name qualified of
                Just (Env.Specific home annotation) ->
                    R.ok <|
                        if home == ModuleName.debug then
                            Can.VarDebug env.home name annotation

                        else
                            Can.VarForeign home name annotation

                Just (Env.Ambiguous h hs) ->
                    R.throw (Error.AmbiguousVar region (Just prefix) name h hs)

                Nothing ->
                    R.throw (Error.NotFoundVar region (Just prefix) name (toPossibleNames env.vars env.q_vars))

        Nothing ->
            let
                (IO.Canonical pkg _) =
                    env.home
            in
            if Name.isKernel prefix && Pkg.isKernel pkg then
                R.ok <| Can.VarKernel (Name.getKernel prefix) name

            else
                R.throw (Error.NotFoundVar region (Just prefix) name (toPossibleNames env.vars env.q_vars))


toPossibleNames : Dict String Name Env.Var -> Env.Qualified Can.Annotation -> Error.PossibleNames
toPossibleNames exposed qualified =
    Error.PossibleNames (Utils.keysSet identity compare exposed) (Dict.map (\_ -> Utils.keysSet identity compare) qualified)



-- FIND CTOR


toVarCtor : Name -> Env.Ctor -> Can.Expr_
toVarCtor name ctor =
    case ctor of
        Env.Ctor home typeName (Can.Union vars _ _ opts) index args ->
            let
                freeVars : Dict String Name ()
                freeVars =
                    Dict.fromList identity (List.map (\v -> ( v, () )) vars)

                result : Can.Type
                result =
                    Can.TType home typeName (List.map Can.TVar vars)

                tipe : Can.Type
                tipe =
                    List.foldr Can.TLambda result args
            in
            Can.VarCtor opts home name index (Can.Forall freeVars tipe)

        Env.RecordCtor home vars tipe ->
            let
                freeVars : Dict String Name ()
                freeVars =
                    Dict.fromList identity (List.map (\v -> ( v, () )) vars)
            in
            Can.VarCtor Can.Normal home name Index.first (Can.Forall freeVars tipe)
