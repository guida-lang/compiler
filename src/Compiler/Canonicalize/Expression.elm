module Compiler.Canonicalize.Expression exposing
    ( EResult
    , FreeLocals
    , Uses(..)
    , canonicalize
    , gatherTypedArgs
    , verifyBindings
    )

import Basics.Extra exposing (flip)
import Compiler.AST.Utils.Type as Type
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Canonicalize.Pattern as Pattern
import Compiler.Canonicalize.Type as Type
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Result as R
import Compiler.Reporting.Warning as W
import Data.Graph as Graph
import Data.Map as Dict exposing (Dict)
import Prelude
import Types as T
import Utils.Main as Utils



-- RESULTS


type alias EResult i w a =
    R.RResult i w T.CREC_Error a


type alias FreeLocals =
    Dict String T.CDN_Name Uses


type Uses
    = Uses
        { direct : Int
        , delayed : Int
        }



-- CANONICALIZE


canonicalize : Env.Env -> T.CASTS_Expr -> EResult FreeLocals (List W.Warning) T.CASTC_Expr
canonicalize env (T.CRA_At region expression) =
    R.fmap (T.CRA_At region) <|
        case expression of
            T.CASTS_Str string ->
                R.ok (T.CASTC_Str string)

            T.CASTS_Chr char ->
                R.ok (T.CASTC_Chr char)

            T.CASTS_Int int ->
                R.ok (T.CASTC_Int int)

            T.CASTS_Float float ->
                R.ok (T.CASTC_Float float)

            T.CASTS_Var varType name ->
                case varType of
                    T.CASTS_LowVar ->
                        findVar region env name

                    T.CASTS_CapVar ->
                        R.fmap (toVarCtor name) (Env.findCtor region env name)

            T.CASTS_VarQual varType prefix name ->
                case varType of
                    T.CASTS_LowVar ->
                        findVarQual region env prefix name

                    T.CASTS_CapVar ->
                        R.fmap (toVarCtor name) (Env.findCtorQual region env prefix name)

            T.CASTS_List exprs ->
                R.fmap T.CASTC_List (R.traverse (canonicalize env) exprs)

            T.CASTS_Op op ->
                Env.findBinop region env op
                    |> R.fmap
                        (\(Env.Binop _ home name annotation _ _) ->
                            T.CASTC_VarOperator op home name annotation
                        )

            T.CASTS_Negate expr ->
                R.fmap T.CASTC_Negate (canonicalize env expr)

            T.CASTS_Binops ops final ->
                R.fmap A.toValue (canonicalizeBinops region env ops final)

            T.CASTS_Lambda srcArgs body ->
                delayedUsage <|
                    (Pattern.verify T.CREC_DPLambdaArgs
                        (R.traverse (Pattern.canonicalize env) srcArgs)
                        |> R.bind
                            (\( args, bindings ) ->
                                Env.addLocals bindings env
                                    |> R.bind
                                        (\newEnv ->
                                            verifyBindings W.Pattern bindings (canonicalize newEnv body)
                                                |> R.fmap
                                                    (\( cbody, freeLocals ) ->
                                                        ( T.CASTC_Lambda args cbody, freeLocals )
                                                    )
                                        )
                            )
                    )

            T.CASTS_Call func args ->
                R.pure T.CASTC_Call
                    |> R.apply (canonicalize env func)
                    |> R.apply (R.traverse (canonicalize env) args)

            T.CASTS_If branches finally ->
                R.pure T.CASTC_If
                    |> R.apply (R.traverse (canonicalizeIfBranch env) branches)
                    |> R.apply (canonicalize env finally)

            T.CASTS_Let defs expr ->
                R.fmap A.toValue <| canonicalizeLet region env defs expr

            T.CASTS_Case expr branches ->
                R.pure T.CASTC_Case
                    |> R.apply (canonicalize env expr)
                    |> R.apply (R.traverse (canonicalizeCaseBranch env) branches)

            T.CASTS_Accessor field ->
                R.pure (T.CASTC_Accessor field)

            T.CASTS_Access record field ->
                R.pure T.CASTC_Access
                    |> R.apply (canonicalize env record)
                    |> R.apply (R.ok field)

            T.CASTS_Update (T.CRA_At reg name) fields ->
                let
                    makeCanFields : R.RResult i w T.CREC_Error (Dict String T.CDN_Name (R.RResult FreeLocals (List W.Warning) T.CREC_Error T.CASTC_FieldUpdate))
                    makeCanFields =
                        Dups.checkFields_ (\r t -> R.fmap (T.CASTC_FieldUpdate r) (canonicalize env t)) fields
                in
                R.pure (T.CASTC_Update name)
                    |> R.apply (R.fmap (T.CRA_At reg) (findVar reg env name))
                    |> R.apply (R.bind (Utils.sequenceADict identity compare) makeCanFields)

            T.CASTS_Record fields ->
                Dups.checkFields fields
                    |> R.bind
                        (\fieldDict ->
                            R.fmap T.CASTC_Record (R.traverseDict identity compare (canonicalize env) fieldDict)
                        )

            T.CASTS_Unit ->
                R.ok T.CASTC_Unit

            T.CASTS_Tuple a b cs ->
                R.pure T.CASTC_Tuple
                    |> R.apply (canonicalize env a)
                    |> R.apply (canonicalize env b)
                    |> R.apply (canonicalizeTupleExtras region env cs)

            T.CASTS_Shader src tipe ->
                R.ok (T.CASTC_Shader src tipe)


canonicalizeTupleExtras : T.CRA_Region -> Env.Env -> List T.CASTS_Expr -> EResult FreeLocals (List W.Warning) (Maybe T.CASTC_Expr)
canonicalizeTupleExtras region env extras =
    case extras of
        [] ->
            R.ok Nothing

        [ three ] ->
            R.fmap Just <| canonicalize env three

        _ ->
            R.throw (T.CREC_TupleLargerThanThree region)



-- CANONICALIZE IF BRANCH


canonicalizeIfBranch : Env.Env -> ( T.CASTS_Expr, T.CASTS_Expr ) -> EResult FreeLocals (List W.Warning) ( T.CASTC_Expr, T.CASTC_Expr )
canonicalizeIfBranch env ( condition, branch ) =
    R.pure Tuple.pair
        |> R.apply (canonicalize env condition)
        |> R.apply (canonicalize env branch)



-- CANONICALIZE CASE BRANCH


canonicalizeCaseBranch : Env.Env -> ( T.CASTS_Pattern, T.CASTS_Expr ) -> EResult FreeLocals (List W.Warning) T.CASTC_CaseBranch
canonicalizeCaseBranch env ( pattern, expr ) =
    directUsage
        (Pattern.verify T.CREC_DPCaseBranch
            (Pattern.canonicalize env pattern)
            |> R.bind
                (\( cpattern, bindings ) ->
                    Env.addLocals bindings env
                        |> R.bind
                            (\newEnv ->
                                verifyBindings W.Pattern bindings (canonicalize newEnv expr)
                                    |> R.fmap
                                        (\( cexpr, freeLocals ) ->
                                            ( T.CASTC_CaseBranch cpattern cexpr, freeLocals )
                                        )
                            )
                )
        )



-- CANONICALIZE BINOPS


canonicalizeBinops : T.CRA_Region -> Env.Env -> List ( T.CASTS_Expr, T.CRA_Located T.CDN_Name ) -> T.CASTS_Expr -> EResult FreeLocals (List W.Warning) T.CASTC_Expr
canonicalizeBinops overallRegion env ops final =
    let
        canonicalizeHelp : ( T.CASTS_Expr, T.CRA_Located T.CDN_Name ) -> R.RResult FreeLocals (List W.Warning) T.CREC_Error ( T.CASTC_Expr, Env.Binop )
        canonicalizeHelp ( expr, T.CRA_At region op ) =
            R.ok Tuple.pair
                |> R.apply (canonicalize env expr)
                |> R.apply (Env.findBinop region env op)
    in
    R.bind (runBinopStepper overallRegion)
        (R.ok More
            |> R.apply (R.traverse canonicalizeHelp ops)
            |> R.apply (canonicalize env final)
        )


type Step
    = Done T.CASTC_Expr
    | More (List ( T.CASTC_Expr, Env.Binop )) T.CASTC_Expr
    | Error Env.Binop Env.Binop


runBinopStepper : T.CRA_Region -> Step -> EResult FreeLocals w T.CASTC_Expr
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
            R.throw (T.CREC_Binop overallRegion op1 op2)


toBinopStep : (T.CASTC_Expr -> T.CASTC_Expr) -> Env.Binop -> List ( T.CASTC_Expr, Env.Binop ) -> T.CASTC_Expr -> Step
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
                    ( T.CASTUB_Left, T.CASTUB_Left ) ->
                        toBinopStep (toBinop op (makeBinop expr)) op rest final

                    ( T.CASTUB_Right, T.CASTUB_Right ) ->
                        toBinopStep (makeBinop << toBinop op expr) op rest final

                    _ ->
                        Error rootOp op


toBinop : Env.Binop -> T.CASTC_Expr -> T.CASTC_Expr -> T.CASTC_Expr
toBinop (Env.Binop op home name annotation _ _) left right =
    A.merge left right (T.CASTC_Binop op home name annotation left right)


canonicalizeLet : T.CRA_Region -> Env.Env -> List (T.CRA_Located T.CASTS_Def) -> T.CASTS_Expr -> EResult FreeLocals (List W.Warning) T.CASTC_Expr
canonicalizeLet letRegion env defs body =
    directUsage <|
        (Dups.detect (T.CREC_DuplicatePattern T.CREC_DPLetBinding)
            (List.foldl addBindings Dups.none defs)
            |> R.bind
                (\bindings ->
                    Env.addLocals bindings env
                        |> R.bind
                            (\newEnv ->
                                verifyBindings W.Def bindings <|
                                    (Utils.foldM (addDefNodes newEnv) [] defs
                                        |> R.bind
                                            (\nodes ->
                                                canonicalize newEnv body
                                                    |> R.bind
                                                        (\cbody ->
                                                            detectCycles letRegion (Graph.stronglyConnComp nodes) cbody
                                                        )
                                            )
                                    )
                            )
                )
        )


addBindings : T.CRA_Located T.CASTS_Def -> Dups.Tracker T.CRA_Region -> Dups.Tracker T.CRA_Region
addBindings (T.CRA_At _ def) bindings =
    case def of
        T.CASTS_Define (T.CRA_At region name) _ _ _ ->
            Dups.insert name region region bindings

        T.CASTS_Destruct pattern _ ->
            addBindingsHelp bindings pattern


addBindingsHelp : Dups.Tracker T.CRA_Region -> T.CASTS_Pattern -> Dups.Tracker T.CRA_Region
addBindingsHelp bindings (T.CRA_At region pattern) =
    case pattern of
        T.CASTS_PAnything ->
            bindings

        T.CASTS_PVar name ->
            Dups.insert name region region bindings

        T.CASTS_PRecord fields ->
            let
                addField : T.CRA_Located T.CDN_Name -> Dups.Tracker T.CRA_Region -> Dups.Tracker T.CRA_Region
                addField (T.CRA_At fieldRegion name) dict =
                    Dups.insert name fieldRegion fieldRegion dict
            in
            List.foldl addField bindings fields

        T.CASTS_PUnit ->
            bindings

        T.CASTS_PTuple a b cs ->
            List.foldl (flip addBindingsHelp) bindings (a :: b :: cs)

        T.CASTS_PCtor _ _ patterns ->
            List.foldl (flip addBindingsHelp) bindings patterns

        T.CASTS_PCtorQual _ _ _ patterns ->
            List.foldl (flip addBindingsHelp) bindings patterns

        T.CASTS_PList patterns ->
            List.foldl (flip addBindingsHelp) bindings patterns

        T.CASTS_PCons hd tl ->
            addBindingsHelp (addBindingsHelp bindings hd) tl

        T.CASTS_PAlias aliasPattern (T.CRA_At nameRegion name) ->
            Dups.insert name nameRegion nameRegion <|
                addBindingsHelp bindings aliasPattern

        T.CASTS_PChr _ ->
            bindings

        T.CASTS_PStr _ ->
            bindings

        T.CASTS_PInt _ ->
            bindings


type alias Node =
    ( Binding, T.CDN_Name, List T.CDN_Name )


type Binding
    = Define T.CASTC_Def
    | Edge (T.CRA_Located T.CDN_Name)
    | Destruct T.CASTC_Pattern T.CASTC_Expr


addDefNodes : Env.Env -> List Node -> T.CRA_Located T.CASTS_Def -> EResult FreeLocals (List W.Warning) (List Node)
addDefNodes env nodes (T.CRA_At _ def) =
    case def of
        T.CASTS_Define ((T.CRA_At _ name) as aname) srcArgs body maybeType ->
            case maybeType of
                Nothing ->
                    Pattern.verify (T.CREC_DPFuncArgs name)
                        (R.traverse (Pattern.canonicalize env) srcArgs)
                        |> R.bind
                            (\( args, argBindings ) ->
                                Env.addLocals argBindings env
                                    |> R.bind
                                        (\newEnv ->
                                            verifyBindings W.Pattern argBindings (canonicalize newEnv body)
                                                |> R.bind
                                                    (\( cbody, freeLocals ) ->
                                                        let
                                                            cdef : T.CASTC_Def
                                                            cdef =
                                                                T.CASTC_Def aname args cbody

                                                            node : ( Binding, T.CDN_Name, List T.CDN_Name )
                                                            node =
                                                                ( Define cdef, name, Dict.keys compare freeLocals )
                                                        in
                                                        logLetLocals args freeLocals (node :: nodes)
                                                    )
                                        )
                            )

                Just tipe ->
                    Type.toAnnotation env tipe
                        |> R.bind
                            (\(T.CASTC_Forall freeVars ctipe) ->
                                Pattern.verify (T.CREC_DPFuncArgs name)
                                    (gatherTypedArgs env name srcArgs ctipe Index.first [])
                                    |> R.bind
                                        (\( ( args, resultType ), argBindings ) ->
                                            Env.addLocals argBindings env
                                                |> R.bind
                                                    (\newEnv ->
                                                        verifyBindings W.Pattern argBindings (canonicalize newEnv body)
                                                            |> R.bind
                                                                (\( cbody, freeLocals ) ->
                                                                    let
                                                                        cdef : T.CASTC_Def
                                                                        cdef =
                                                                            T.CASTC_TypedDef aname freeVars args cbody resultType

                                                                        node : ( Binding, T.CDN_Name, List T.CDN_Name )
                                                                        node =
                                                                            ( Define cdef, name, Dict.keys compare freeLocals )
                                                                    in
                                                                    logLetLocals args freeLocals (node :: nodes)
                                                                )
                                                    )
                                        )
                            )

        T.CASTS_Destruct pattern body ->
            Pattern.verify T.CREC_DPDestruct
                (Pattern.canonicalize env pattern)
                |> R.bind
                    (\( cpattern, _ ) ->
                        R.RResult
                            (\fs ws ->
                                case canonicalize env body of
                                    R.RResult k ->
                                        case k Dict.empty ws of
                                            Ok (R.ROk freeLocals warnings cbody) ->
                                                let
                                                    names : List (T.CRA_Located T.CDN_Name)
                                                    names =
                                                        getPatternNames [] pattern

                                                    name : T.CDN_Name
                                                    name =
                                                        Name.fromManyNames (List.map A.toValue names)

                                                    node : ( Binding, T.CDN_Name, List T.CDN_Name )
                                                    node =
                                                        ( Destruct cpattern cbody, name, Dict.keys compare freeLocals )
                                                in
                                                Ok
                                                    (R.ROk
                                                        (Utils.mapUnionWith identity compare combineUses fs freeLocals)
                                                        warnings
                                                        (List.foldl (addEdge [ name ]) (node :: nodes) names)
                                                    )

                                            Err (R.RErr freeLocals warnings errors) ->
                                                Err (R.RErr (Utils.mapUnionWith identity compare combineUses freeLocals fs) warnings errors)
                            )
                    )


logLetLocals : List arg -> FreeLocals -> value -> EResult FreeLocals w value
logLetLocals args letLocals value =
    R.RResult
        (\freeLocals warnings ->
            Ok
                (R.ROk
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
        )


addEdge : List T.CDN_Name -> T.CRA_Located T.CDN_Name -> List Node -> List Node
addEdge edges ((T.CRA_At _ name) as aname) nodes =
    ( Edge aname, name, edges ) :: nodes


getPatternNames : List (T.CRA_Located T.CDN_Name) -> T.CASTS_Pattern -> List (T.CRA_Located T.CDN_Name)
getPatternNames names (T.CRA_At region pattern) =
    case pattern of
        T.CASTS_PAnything ->
            names

        T.CASTS_PVar name ->
            T.CRA_At region name :: names

        T.CASTS_PRecord fields ->
            fields ++ names

        T.CASTS_PAlias ptrn name ->
            getPatternNames (name :: names) ptrn

        T.CASTS_PUnit ->
            names

        T.CASTS_PTuple a b cs ->
            List.foldl (flip getPatternNames) (getPatternNames (getPatternNames names a) b) cs

        T.CASTS_PCtor _ _ args ->
            List.foldl (flip getPatternNames) names args

        T.CASTS_PCtorQual _ _ _ args ->
            List.foldl (flip getPatternNames) names args

        T.CASTS_PList patterns ->
            List.foldl (flip getPatternNames) names patterns

        T.CASTS_PCons hd tl ->
            getPatternNames (getPatternNames names hd) tl

        T.CASTS_PChr _ ->
            names

        T.CASTS_PStr _ ->
            names

        T.CASTS_PInt _ ->
            names


gatherTypedArgs :
    Env.Env
    -> T.CDN_Name
    -> List T.CASTS_Pattern
    -> T.CASTC_Type
    -> T.CDI_ZeroBased
    -> List ( T.CASTC_Pattern, T.CASTC_Type )
    -> EResult Pattern.DupsDict w ( List ( T.CASTC_Pattern, T.CASTC_Type ), T.CASTC_Type )
gatherTypedArgs env name srcArgs tipe index revTypedArgs =
    case srcArgs of
        [] ->
            R.ok ( List.reverse revTypedArgs, tipe )

        srcArg :: otherSrcArgs ->
            case Type.iteratedDealias tipe of
                T.CASTC_TLambda argType resultType ->
                    Pattern.canonicalize env srcArg
                        |> R.bind
                            (\arg ->
                                gatherTypedArgs env name otherSrcArgs resultType (Index.next index) <|
                                    (( arg, argType ) :: revTypedArgs)
                            )

                _ ->
                    let
                        ( T.CRA_At start _, T.CRA_At end _ ) =
                            ( Prelude.head srcArgs, Prelude.last srcArgs )
                    in
                    R.throw (T.CREC_AnnotationTooShort (A.mergeRegions start end) name index (List.length srcArgs))


detectCycles : T.CRA_Region -> List (Graph.SCC Binding) -> T.CASTC_Expr -> EResult i w T.CASTC_Expr
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
                                |> R.fmap (T.CASTC_Let def)
                                |> R.fmap (T.CRA_At letRegion)

                        Edge _ ->
                            detectCycles letRegion subSccs body

                        Destruct pattern expr ->
                            detectCycles letRegion subSccs body
                                |> R.fmap (T.CASTC_LetDestruct pattern expr)
                                |> R.fmap (T.CRA_At letRegion)

                Graph.CyclicSCC bindings ->
                    R.ok T.CASTC_LetRec
                        |> R.apply (checkCycle bindings [])
                        |> R.apply (detectCycles letRegion subSccs body)
                        |> R.fmap (T.CRA_At letRegion)


checkCycle : List Binding -> List T.CASTC_Def -> EResult i w (List T.CASTC_Def)
checkCycle bindings defs =
    case bindings of
        [] ->
            R.ok defs

        binding :: otherBindings ->
            case binding of
                Define ((T.CASTC_Def name args _) as def) ->
                    if List.isEmpty args then
                        R.throw (T.CREC_RecursiveLet name (toNames otherBindings defs))

                    else
                        checkCycle otherBindings (def :: defs)

                Define ((T.CASTC_TypedDef name _ args _ _) as def) ->
                    if List.isEmpty args then
                        R.throw (T.CREC_RecursiveLet name (toNames otherBindings defs))

                    else
                        checkCycle otherBindings (def :: defs)

                Edge name ->
                    R.throw (T.CREC_RecursiveLet name (toNames otherBindings defs))

                Destruct _ _ ->
                    -- a Destruct cannot appear in a cycle without any Edge values
                    -- so we just keep going until we get to the edges
                    checkCycle otherBindings defs


toNames : List Binding -> List T.CASTC_Def -> List T.CDN_Name
toNames bindings revDefs =
    case bindings of
        [] ->
            List.reverse (List.map getDefName revDefs)

        binding :: otherBindings ->
            case binding of
                Define def ->
                    getDefName def :: toNames otherBindings revDefs

                Edge (T.CRA_At _ name) ->
                    name :: toNames otherBindings revDefs

                Destruct _ _ ->
                    toNames otherBindings revDefs


getDefName : T.CASTC_Def -> T.CDN_Name
getDefName def =
    case def of
        T.CASTC_Def (T.CRA_At _ name) _ _ ->
            name

        T.CASTC_TypedDef (T.CRA_At _ name) _ _ _ _ ->
            name


logVar : T.CDN_Name -> a -> EResult FreeLocals w a
logVar name value =
    R.RResult <|
        \freeLocals warnings ->
            Ok (R.ROk (Utils.mapInsertWith identity combineUses name oneDirectUse freeLocals) warnings value)


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
                Ok (R.ROk freeLocals warnings1 value) ->
                    let
                        outerFreeLocals : Dict String T.CDN_Name Uses
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
                    Ok (R.ROk info warnings2 ( value, outerFreeLocals ))

                Err (R.RErr _ warnings1 err) ->
                    Err (R.RErr info warnings1 err)
        )


addUnusedWarning : W.Context -> T.CDN_Name -> T.CRA_Region -> List W.Warning -> List W.Warning
addUnusedWarning context name region warnings =
    W.UnusedVariable region context name :: warnings


directUsage : EResult () w ( expr, FreeLocals ) -> EResult FreeLocals w expr
directUsage (R.RResult k) =
    R.RResult
        (\freeLocals warnings ->
            case k () warnings of
                Ok (R.ROk () ws ( value, newFreeLocals )) ->
                    Ok (R.ROk (Utils.mapUnionWith identity compare combineUses freeLocals newFreeLocals) ws value)

                Err (R.RErr () ws es) ->
                    Err (R.RErr freeLocals ws es)
        )


delayedUsage : EResult () w ( expr, FreeLocals ) -> EResult FreeLocals w expr
delayedUsage (R.RResult k) =
    R.RResult
        (\freeLocals warnings ->
            case k () warnings of
                Ok (R.ROk () ws ( value, newFreeLocals )) ->
                    let
                        delayedLocals : Dict String T.CDN_Name Uses
                        delayedLocals =
                            Dict.map (\_ -> delayUse) newFreeLocals
                    in
                    Ok (R.ROk (Utils.mapUnionWith identity compare combineUses freeLocals delayedLocals) ws value)

                Err (R.RErr () ws es) ->
                    Err (R.RErr freeLocals ws es)
        )



-- FIND VARIABLE


findVar : T.CRA_Region -> Env.Env -> T.CDN_Name -> EResult FreeLocals w T.CASTC_Expr_
findVar region env name =
    case Dict.get identity name env.vars of
        Just var ->
            case var of
                Env.Local _ ->
                    logVar name (T.CASTC_VarLocal name)

                Env.TopLevel _ ->
                    logVar name (T.CASTC_VarTopLevel env.home name)

                Env.Foreign home annotation ->
                    R.ok
                        (if home == ModuleName.debug then
                            T.CASTC_VarDebug env.home name annotation

                         else
                            T.CASTC_VarForeign home name annotation
                        )

                Env.Foreigns h hs ->
                    R.throw (T.CREC_AmbiguousVar region Nothing name h hs)

        Nothing ->
            R.throw (T.CREC_NotFoundVar region Nothing name (toPossibleNames env.vars env.q_vars))


findVarQual : T.CRA_Region -> Env.Env -> T.CDN_Name -> T.CDN_Name -> EResult FreeLocals w T.CASTC_Expr_
findVarQual region env prefix name =
    case Dict.get identity prefix env.q_vars of
        Just qualified ->
            case Dict.get identity name qualified of
                Just (Env.Specific home annotation) ->
                    R.ok <|
                        if home == ModuleName.debug then
                            T.CASTC_VarDebug env.home name annotation

                        else
                            T.CASTC_VarForeign home name annotation

                Just (Env.Ambiguous h hs) ->
                    R.throw (T.CREC_AmbiguousVar region (Just prefix) name h hs)

                Nothing ->
                    R.throw (T.CREC_NotFoundVar region (Just prefix) name (toPossibleNames env.vars env.q_vars))

        Nothing ->
            let
                (T.CEMN_Canonical pkg _) =
                    env.home
            in
            if Name.isKernel prefix && Pkg.isKernel pkg then
                R.ok <| T.CASTC_VarKernel (Name.getKernel prefix) name

            else
                R.throw (T.CREC_NotFoundVar region (Just prefix) name (toPossibleNames env.vars env.q_vars))


toPossibleNames : Dict String T.CDN_Name Env.Var -> Env.Qualified T.CASTC_Annotation -> T.CREC_PossibleNames
toPossibleNames exposed qualified =
    T.CREC_PossibleNames (Utils.keysSet identity compare exposed) (Dict.map (\_ -> Utils.keysSet identity compare) qualified)



-- FIND CTOR


toVarCtor : T.CDN_Name -> Env.Ctor -> T.CASTC_Expr_
toVarCtor name ctor =
    case ctor of
        Env.Ctor home typeName (T.CASTC_Union vars _ _ opts) index args ->
            let
                freeVars : Dict String T.CDN_Name ()
                freeVars =
                    Dict.fromList identity (List.map (\v -> ( v, () )) vars)

                result : T.CASTC_Type
                result =
                    T.CASTC_TType home typeName (List.map T.CASTC_TVar vars)

                tipe : T.CASTC_Type
                tipe =
                    List.foldr T.CASTC_TLambda result args
            in
            T.CASTC_VarCtor opts home name index (T.CASTC_Forall freeVars tipe)

        Env.RecordCtor home vars tipe ->
            let
                freeVars : Dict String T.CDN_Name ()
                freeVars =
                    Dict.fromList identity (List.map (\v -> ( v, () )) vars)
            in
            T.CASTC_VarCtor T.CASTC_Normal home name Index.first (T.CASTC_Forall freeVars tipe)
