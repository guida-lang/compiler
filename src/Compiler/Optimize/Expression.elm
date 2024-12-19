module Compiler.Optimize.Expression exposing
    ( Cycle
    , destructArgs
    , optimize
    , optimizePotentialTailCall
    )

import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Optimize.Case as Case
import Compiler.Optimize.Names as Names
import Compiler.Reporting.Annotation as A
import Data.Map as Dict
import Data.Set as EverySet exposing (EverySet)
import Types as T



-- OPTIMIZE


type alias Cycle =
    EverySet String T.CDN_Name


optimize : Cycle -> T.CASTC_Expr -> Names.Tracker T.CASTO_Expr
optimize cycle (T.CRA_At region expression) =
    case expression of
        T.CASTC_VarLocal name ->
            Names.pure (T.CASTO_VarLocal name)

        T.CASTC_VarTopLevel home name ->
            if EverySet.member identity name cycle then
                Names.pure (T.CASTO_VarCycle home name)

            else
                Names.registerGlobal home name

        T.CASTC_VarKernel home name ->
            Names.registerKernel home (T.CASTO_VarKernel home name)

        T.CASTC_VarForeign home name _ ->
            Names.registerGlobal home name

        T.CASTC_VarCtor opts home name index _ ->
            Names.registerCtor home name index opts

        T.CASTC_VarDebug home name _ ->
            Names.registerDebug name home region

        T.CASTC_VarOperator _ home name _ ->
            Names.registerGlobal home name

        T.CASTC_Chr chr ->
            Names.registerKernel Name.utils (T.CASTO_Chr chr)

        T.CASTC_Str str ->
            Names.pure (T.CASTO_Str str)

        T.CASTC_Int int ->
            Names.pure (T.CASTO_Int int)

        T.CASTC_Float float ->
            Names.pure (T.CASTO_Float float)

        T.CASTC_List entries ->
            Names.traverse (optimize cycle) entries
                |> Names.bind (Names.registerKernel Name.list << T.CASTO_List)

        T.CASTC_Negate expr ->
            Names.registerGlobal ModuleName.basics Name.negate
                |> Names.bind
                    (\func ->
                        optimize cycle expr
                            |> Names.fmap
                                (\arg ->
                                    T.CASTO_Call func [ arg ]
                                )
                    )

        T.CASTC_Binop _ home name _ left right ->
            Names.registerGlobal home name
                |> Names.bind
                    (\optFunc ->
                        optimize cycle left
                            |> Names.bind
                                (\optLeft ->
                                    optimize cycle right
                                        |> Names.fmap
                                            (\optRight ->
                                                T.CASTO_Call optFunc [ optLeft, optRight ]
                                            )
                                )
                    )

        T.CASTC_Lambda args body ->
            destructArgs args
                |> Names.bind
                    (\( argNames, destructors ) ->
                        optimize cycle body
                            |> Names.fmap
                                (\obody ->
                                    T.CASTO_Function argNames (List.foldr T.CASTO_Destruct obody destructors)
                                )
                    )

        T.CASTC_Call func args ->
            optimize cycle func
                |> Names.bind
                    (\optimizeExpr ->
                        Names.traverse (optimize cycle) args
                            |> Names.fmap (T.CASTO_Call optimizeExpr)
                    )

        T.CASTC_If branches finally ->
            let
                optimizeBranch : ( T.CASTC_Expr, T.CASTC_Expr ) -> Names.Tracker ( T.CASTO_Expr, T.CASTO_Expr )
                optimizeBranch ( condition, branch ) =
                    optimize cycle condition
                        |> Names.bind
                            (\expr ->
                                optimize cycle branch
                                    |> Names.fmap (Tuple.pair expr)
                            )
            in
            Names.traverse optimizeBranch branches
                |> Names.bind
                    (\optimizedBranches ->
                        optimize cycle finally
                            |> Names.fmap (T.CASTO_If optimizedBranches)
                    )

        T.CASTC_Let def body ->
            optimize cycle body
                |> Names.bind (optimizeDef cycle def)

        T.CASTC_LetRec defs body ->
            case defs of
                [ def ] ->
                    optimizePotentialTailCallDef cycle def
                        |> Names.bind
                            (\tailCallDef ->
                                optimize cycle body
                                    |> Names.fmap (T.CASTO_Let tailCallDef)
                            )

                _ ->
                    List.foldl
                        (\def bod ->
                            Names.bind (optimizeDef cycle def) bod
                        )
                        (optimize cycle body)
                        defs

        T.CASTC_LetDestruct pattern expr body ->
            destruct pattern
                |> Names.bind
                    (\( name, destructs ) ->
                        optimize cycle expr
                            |> Names.bind
                                (\oexpr ->
                                    optimize cycle body
                                        |> Names.fmap
                                            (\obody ->
                                                T.CASTO_Let (T.CASTO_Def name oexpr) (List.foldr T.CASTO_Destruct obody destructs)
                                            )
                                )
                    )

        T.CASTC_Case expr branches ->
            let
                optimizeBranch : T.CDN_Name -> T.CASTC_CaseBranch -> Names.Tracker ( T.CASTC_Pattern, T.CASTO_Expr )
                optimizeBranch root (T.CASTC_CaseBranch pattern branch) =
                    destructCase root pattern
                        |> Names.bind
                            (\destructors ->
                                optimize cycle branch
                                    |> Names.fmap
                                        (\obranch ->
                                            ( pattern, List.foldr T.CASTO_Destruct obranch destructors )
                                        )
                            )
            in
            Names.generate
                |> Names.bind
                    (\temp ->
                        optimize cycle expr
                            |> Names.bind
                                (\oexpr ->
                                    case oexpr of
                                        T.CASTO_VarLocal root ->
                                            Names.traverse (optimizeBranch root) branches
                                                |> Names.fmap (Case.optimize temp root)

                                        _ ->
                                            Names.traverse (optimizeBranch temp) branches
                                                |> Names.fmap
                                                    (\obranches ->
                                                        T.CASTO_Let (T.CASTO_Def temp oexpr) (Case.optimize temp temp obranches)
                                                    )
                                )
                    )

        T.CASTC_Accessor field ->
            Names.registerField field (T.CASTO_Accessor field)

        T.CASTC_Access record (T.CRA_At _ field) ->
            optimize cycle record
                |> Names.bind
                    (\optRecord ->
                        Names.registerField field (T.CASTO_Access optRecord field)
                    )

        T.CASTC_Update _ record updates ->
            Names.mapTraverse identity compare (optimizeUpdate cycle) updates
                |> Names.bind
                    (\optUpdates ->
                        optimize cycle record
                            |> Names.bind
                                (\optRecord ->
                                    Names.registerFieldDict updates (T.CASTO_Update optRecord optUpdates)
                                )
                    )

        T.CASTC_Record fields ->
            Names.mapTraverse identity compare (optimize cycle) fields
                |> Names.bind
                    (\optFields ->
                        Names.registerFieldDict fields (T.CASTO_Record optFields)
                    )

        T.CASTC_Unit ->
            Names.registerKernel Name.utils T.CASTO_Unit

        T.CASTC_Tuple a b maybeC ->
            optimize cycle a
                |> Names.bind
                    (\optA ->
                        optimize cycle b
                            |> Names.bind
                                (\optB ->
                                    case maybeC of
                                        Just c ->
                                            optimize cycle c
                                                |> Names.bind
                                                    (\optC ->
                                                        Names.registerKernel Name.utils
                                                            (T.CASTO_Tuple optA optB (Just optC))
                                                    )

                                        Nothing ->
                                            Names.registerKernel Name.utils (T.CASTO_Tuple optA optB Nothing)
                                )
                    )

        T.CASTC_Shader src (T.CASTUS_Types attributes uniforms _) ->
            Names.pure (T.CASTO_Shader src (EverySet.fromList identity (Dict.keys compare attributes)) (EverySet.fromList identity (Dict.keys compare uniforms)))



-- UPDATE


optimizeUpdate : Cycle -> T.CASTC_FieldUpdate -> Names.Tracker T.CASTO_Expr
optimizeUpdate cycle (T.CASTC_FieldUpdate _ expr) =
    optimize cycle expr



-- DEFINITION


optimizeDef : Cycle -> T.CASTC_Def -> T.CASTO_Expr -> Names.Tracker T.CASTO_Expr
optimizeDef cycle def body =
    case def of
        T.CASTC_Def (T.CRA_At _ name) args expr ->
            optimizeDefHelp cycle name args expr body

        T.CASTC_TypedDef (T.CRA_At _ name) _ typedArgs expr _ ->
            optimizeDefHelp cycle name (List.map Tuple.first typedArgs) expr body


optimizeDefHelp : Cycle -> T.CDN_Name -> List T.CASTC_Pattern -> T.CASTC_Expr -> T.CASTO_Expr -> Names.Tracker T.CASTO_Expr
optimizeDefHelp cycle name args expr body =
    case args of
        [] ->
            optimize cycle expr
                |> Names.fmap (\oexpr -> T.CASTO_Let (T.CASTO_Def name oexpr) body)

        _ ->
            optimize cycle expr
                |> Names.bind
                    (\oexpr ->
                        destructArgs args
                            |> Names.fmap
                                (\( argNames, destructors ) ->
                                    let
                                        ofunc : T.CASTO_Expr
                                        ofunc =
                                            T.CASTO_Function argNames (List.foldr T.CASTO_Destruct oexpr destructors)
                                    in
                                    T.CASTO_Let (T.CASTO_Def name ofunc) body
                                )
                    )



-- DESTRUCTURING


destructArgs : List T.CASTC_Pattern -> Names.Tracker ( List T.CDN_Name, List T.CASTO_Destructor )
destructArgs args =
    Names.traverse destruct args
        |> Names.fmap List.unzip
        |> Names.fmap
            (\( argNames, destructorLists ) ->
                ( argNames, List.concat destructorLists )
            )


destructCase : T.CDN_Name -> T.CASTC_Pattern -> Names.Tracker (List T.CASTO_Destructor)
destructCase rootName pattern =
    destructHelp (T.CASTO_Root rootName) pattern []
        |> Names.fmap List.reverse


destruct : T.CASTC_Pattern -> Names.Tracker ( T.CDN_Name, List T.CASTO_Destructor )
destruct ((T.CRA_At _ ptrn) as pattern) =
    case ptrn of
        T.CASTC_PVar name ->
            Names.pure ( name, [] )

        T.CASTC_PAlias subPattern name ->
            destructHelp (T.CASTO_Root name) subPattern []
                |> Names.fmap (\revDs -> ( name, List.reverse revDs ))

        _ ->
            Names.generate
                |> Names.bind
                    (\name ->
                        destructHelp (T.CASTO_Root name) pattern []
                            |> Names.fmap
                                (\revDs ->
                                    ( name, List.reverse revDs )
                                )
                    )


destructHelp : T.CASTO_Path -> T.CASTC_Pattern -> List T.CASTO_Destructor -> Names.Tracker (List T.CASTO_Destructor)
destructHelp path (T.CRA_At region pattern) revDs =
    case pattern of
        T.CASTC_PAnything ->
            Names.pure revDs

        T.CASTC_PVar name ->
            Names.pure (T.CASTO_Destructor name path :: revDs)

        T.CASTC_PRecord fields ->
            let
                toDestruct : T.CDN_Name -> T.CASTO_Destructor
                toDestruct name =
                    T.CASTO_Destructor name (T.CASTO_Field name path)
            in
            Names.registerFieldList fields (List.map toDestruct fields ++ revDs)

        T.CASTC_PAlias subPattern name ->
            destructHelp (T.CASTO_Root name) subPattern <|
                (T.CASTO_Destructor name path :: revDs)

        T.CASTC_PUnit ->
            Names.pure revDs

        T.CASTC_PTuple a b Nothing ->
            destructTwo path a b revDs

        T.CASTC_PTuple a b (Just c) ->
            case path of
                T.CASTO_Root _ ->
                    destructHelp (T.CASTO_Index Index.first path) a revDs
                        |> Names.bind (destructHelp (T.CASTO_Index Index.second path) b)
                        |> Names.bind (destructHelp (T.CASTO_Index Index.third path) c)

                _ ->
                    Names.generate
                        |> Names.bind
                            (\name ->
                                let
                                    newRoot : T.CASTO_Path
                                    newRoot =
                                        T.CASTO_Root name
                                in
                                destructHelp (T.CASTO_Index Index.first newRoot) a (T.CASTO_Destructor name path :: revDs)
                                    |> Names.bind (destructHelp (T.CASTO_Index Index.second newRoot) b)
                                    |> Names.bind (destructHelp (T.CASTO_Index Index.third newRoot) c)
                            )

        T.CASTC_PList [] ->
            Names.pure revDs

        T.CASTC_PList (hd :: tl) ->
            destructTwo path hd (T.CRA_At region (T.CASTC_PList tl)) revDs

        T.CASTC_PCons hd tl ->
            destructTwo path hd tl revDs

        T.CASTC_PChr _ ->
            Names.pure revDs

        T.CASTC_PStr _ ->
            Names.pure revDs

        T.CASTC_PInt _ ->
            Names.pure revDs

        T.CASTC_PBool _ _ ->
            Names.pure revDs

        T.CASTC_PCtor { union, args } ->
            case args of
                [ T.CASTC_PatternCtorArg _ _ arg ] ->
                    let
                        (T.CASTC_Union _ _ _ opts) =
                            union
                    in
                    case opts of
                        T.CASTC_Normal ->
                            destructHelp (T.CASTO_Index Index.first path) arg revDs

                        T.CASTC_Unbox ->
                            destructHelp (T.CASTO_Unbox path) arg revDs

                        T.CASTC_Enum ->
                            destructHelp (T.CASTO_Index Index.first path) arg revDs

                _ ->
                    case path of
                        T.CASTO_Root _ ->
                            List.foldl (\arg -> Names.bind (\revDs_ -> destructCtorArg path revDs_ arg))
                                (Names.pure revDs)
                                args

                        _ ->
                            Names.generate
                                |> Names.bind
                                    (\name ->
                                        List.foldl (\arg -> Names.bind (\revDs_ -> destructCtorArg (T.CASTO_Root name) revDs_ arg))
                                            (Names.pure (T.CASTO_Destructor name path :: revDs))
                                            args
                                    )


destructTwo : T.CASTO_Path -> T.CASTC_Pattern -> T.CASTC_Pattern -> List T.CASTO_Destructor -> Names.Tracker (List T.CASTO_Destructor)
destructTwo path a b revDs =
    case path of
        T.CASTO_Root _ ->
            destructHelp (T.CASTO_Index Index.first path) a revDs
                |> Names.bind (destructHelp (T.CASTO_Index Index.second path) b)

        _ ->
            Names.generate
                |> Names.bind
                    (\name ->
                        let
                            newRoot : T.CASTO_Path
                            newRoot =
                                T.CASTO_Root name
                        in
                        destructHelp (T.CASTO_Index Index.first newRoot) a (T.CASTO_Destructor name path :: revDs)
                            |> Names.bind (destructHelp (T.CASTO_Index Index.second newRoot) b)
                    )


destructCtorArg : T.CASTO_Path -> List T.CASTO_Destructor -> T.CASTC_PatternCtorArg -> Names.Tracker (List T.CASTO_Destructor)
destructCtorArg path revDs (T.CASTC_PatternCtorArg index _ arg) =
    destructHelp (T.CASTO_Index index path) arg revDs



-- TAIL CALL


optimizePotentialTailCallDef : Cycle -> T.CASTC_Def -> Names.Tracker T.CASTO_Def
optimizePotentialTailCallDef cycle def =
    case def of
        T.CASTC_Def (T.CRA_At _ name) args expr ->
            optimizePotentialTailCall cycle name args expr

        T.CASTC_TypedDef (T.CRA_At _ name) _ typedArgs expr _ ->
            optimizePotentialTailCall cycle name (List.map Tuple.first typedArgs) expr


optimizePotentialTailCall : Cycle -> T.CDN_Name -> List T.CASTC_Pattern -> T.CASTC_Expr -> Names.Tracker T.CASTO_Def
optimizePotentialTailCall cycle name args expr =
    destructArgs args
        |> Names.bind
            (\( argNames, destructors ) ->
                optimizeTail cycle name argNames expr
                    |> Names.fmap (toTailDef name argNames destructors)
            )


optimizeTail : Cycle -> T.CDN_Name -> List T.CDN_Name -> T.CASTC_Expr -> Names.Tracker T.CASTO_Expr
optimizeTail cycle rootName argNames ((T.CRA_At _ expression) as locExpr) =
    case expression of
        T.CASTC_Call func args ->
            Names.traverse (optimize cycle) args
                |> Names.bind
                    (\oargs ->
                        let
                            isMatchingName : Bool
                            isMatchingName =
                                case A.toValue func of
                                    T.CASTC_VarLocal name ->
                                        rootName == name

                                    T.CASTC_VarTopLevel _ name ->
                                        rootName == name

                                    _ ->
                                        False
                        in
                        if isMatchingName then
                            case Index.indexedZipWith (\_ a b -> ( a, b )) argNames oargs of
                                Index.LengthMatch pairs ->
                                    Names.pure (T.CASTO_TailCall rootName pairs)

                                Index.LengthMismatch _ _ ->
                                    optimize cycle func
                                        |> Names.fmap (\ofunc -> T.CASTO_Call ofunc oargs)

                        else
                            optimize cycle func
                                |> Names.fmap (\ofunc -> T.CASTO_Call ofunc oargs)
                    )

        T.CASTC_If branches finally ->
            let
                optimizeBranch : ( T.CASTC_Expr, T.CASTC_Expr ) -> Names.Tracker ( T.CASTO_Expr, T.CASTO_Expr )
                optimizeBranch ( condition, branch ) =
                    optimize cycle condition
                        |> Names.bind
                            (\optimizeCondition ->
                                optimizeTail cycle rootName argNames branch
                                    |> Names.fmap (Tuple.pair optimizeCondition)
                            )
            in
            Names.traverse optimizeBranch branches
                |> Names.bind
                    (\obranches ->
                        optimizeTail cycle rootName argNames finally
                            |> Names.fmap (T.CASTO_If obranches)
                    )

        T.CASTC_Let def body ->
            optimizeTail cycle rootName argNames body
                |> Names.bind (optimizeDef cycle def)

        T.CASTC_LetRec defs body ->
            case defs of
                [ def ] ->
                    optimizePotentialTailCallDef cycle def
                        |> Names.bind
                            (\obody ->
                                optimizeTail cycle rootName argNames body
                                    |> Names.fmap (T.CASTO_Let obody)
                            )

                _ ->
                    List.foldl
                        (\def bod ->
                            Names.bind (optimizeDef cycle def) bod
                        )
                        (optimize cycle body)
                        defs

        T.CASTC_LetDestruct pattern expr body ->
            destruct pattern
                |> Names.bind
                    (\( dname, destructors ) ->
                        optimize cycle expr
                            |> Names.bind
                                (\oexpr ->
                                    optimizeTail cycle rootName argNames body
                                        |> Names.fmap
                                            (\obody ->
                                                T.CASTO_Let (T.CASTO_Def dname oexpr) (List.foldr T.CASTO_Destruct obody destructors)
                                            )
                                )
                    )

        T.CASTC_Case expr branches ->
            let
                optimizeBranch : T.CDN_Name -> T.CASTC_CaseBranch -> Names.Tracker ( T.CASTC_Pattern, T.CASTO_Expr )
                optimizeBranch root (T.CASTC_CaseBranch pattern branch) =
                    destructCase root pattern
                        |> Names.bind
                            (\destructors ->
                                optimizeTail cycle rootName argNames branch
                                    |> Names.fmap
                                        (\obranch ->
                                            ( pattern, List.foldr T.CASTO_Destruct obranch destructors )
                                        )
                            )
            in
            Names.generate
                |> Names.bind
                    (\temp ->
                        optimize cycle expr
                            |> Names.bind
                                (\oexpr ->
                                    case oexpr of
                                        T.CASTO_VarLocal root ->
                                            Names.traverse (optimizeBranch root) branches
                                                |> Names.fmap (Case.optimize temp root)

                                        _ ->
                                            Names.traverse (optimizeBranch temp) branches
                                                |> Names.fmap
                                                    (\obranches ->
                                                        T.CASTO_Let (T.CASTO_Def temp oexpr) (Case.optimize temp temp obranches)
                                                    )
                                )
                    )

        _ ->
            optimize cycle locExpr



-- DETECT TAIL CALLS


toTailDef : T.CDN_Name -> List T.CDN_Name -> List T.CASTO_Destructor -> T.CASTO_Expr -> T.CASTO_Def
toTailDef name argNames destructors body =
    if hasTailCall body then
        T.CASTO_TailDef name argNames (List.foldr T.CASTO_Destruct body destructors)

    else
        T.CASTO_Def name (T.CASTO_Function argNames (List.foldr T.CASTO_Destruct body destructors))


hasTailCall : T.CASTO_Expr -> Bool
hasTailCall expression =
    case expression of
        T.CASTO_TailCall _ _ ->
            True

        T.CASTO_If branches finally ->
            hasTailCall finally || List.any (hasTailCall << Tuple.second) branches

        T.CASTO_Let _ body ->
            hasTailCall body

        T.CASTO_Destruct _ body ->
            hasTailCall body

        T.CASTO_Case _ _ decider jumps ->
            decidecHasTailCall decider || List.any (hasTailCall << Tuple.second) jumps

        _ ->
            False


decidecHasTailCall : T.CASTO_Decider T.CASTO_Choice -> Bool
decidecHasTailCall decider =
    case decider of
        T.CASTO_Leaf choice ->
            case choice of
                T.CASTO_Inline expr ->
                    hasTailCall expr

                T.CASTO_Jump _ ->
                    False

        T.CASTO_Chain _ success failure ->
            decidecHasTailCall success || decidecHasTailCall failure

        T.CASTO_FanOut _ tests fallback ->
            decidecHasTailCall fallback || List.any (decidecHasTailCall << Tuple.second) tests
