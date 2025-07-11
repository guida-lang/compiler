module Compiler.Optimize.Expression exposing
    ( Cycle
    , destructArgs
    , optimize
    , optimizePotentialTailCall
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Optimize.Case as Case
import Compiler.Optimize.Names as Names
import Compiler.Reporting.Annotation as A
import Data.Map as Dict
import Data.Set as EverySet exposing (EverySet)
import Utils.Main as Utils



-- OPTIMIZE


type alias Cycle =
    EverySet String Name.Name


optimize : Cycle -> Can.Expr -> Names.Tracker Opt.Expr
optimize cycle (A.At region expression) =
    case expression of
        Can.VarLocal name ->
            Names.pure (Opt.TrackedVarLocal region name)

        Can.VarTopLevel home name ->
            if EverySet.member identity name cycle then
                Names.pure (Opt.VarCycle region home name)

            else
                Names.registerGlobal region home name

        Can.VarKernel home name ->
            Names.registerKernel home (Opt.VarKernel region home name)

        Can.VarForeign home name _ ->
            Names.registerGlobal region home name

        Can.VarCtor opts home name index _ ->
            Names.registerCtor region home (A.At region name) index opts

        Can.VarDebug home name _ ->
            Names.registerDebug name home region

        Can.VarOperator _ home name _ ->
            Names.registerGlobal region home name

        Can.Chr chr ->
            Names.registerKernel Name.utils (Opt.Chr region chr)

        Can.Str str ->
            Names.pure (Opt.Str region str)

        Can.Int int ->
            Names.pure (Opt.Int region int)

        Can.Float float ->
            Names.pure (Opt.Float region float)

        Can.List entries ->
            Names.traverse (optimize cycle) entries
                |> Names.bind (Names.registerKernel Name.list << Opt.List region)

        Can.Negate expr ->
            Names.registerGlobal region ModuleName.basics Name.negate
                |> Names.bind
                    (\func ->
                        optimize cycle expr
                            |> Names.fmap
                                (\arg ->
                                    Opt.Call region func [ arg ]
                                )
                    )

        Can.Binop _ home name _ left right ->
            Names.registerGlobal region home name
                |> Names.bind
                    (\optFunc ->
                        optimize cycle left
                            |> Names.bind
                                (\optLeft ->
                                    optimize cycle right
                                        |> Names.fmap
                                            (\optRight ->
                                                Opt.Call region optFunc [ optLeft, optRight ]
                                            )
                                )
                    )

        Can.Lambda args body ->
            destructArgs args
                |> Names.bind
                    (\( argNames, destructors ) ->
                        optimize cycle body
                            |> Names.fmap
                                (\obody ->
                                    Opt.TrackedFunction argNames (List.foldr Opt.Destruct obody destructors)
                                )
                    )

        Can.Call func args ->
            optimize cycle func
                |> Names.bind
                    (\optimizeExpr ->
                        Names.traverse (optimize cycle) args
                            |> Names.fmap (Opt.Call region optimizeExpr)
                    )

        Can.If branches finally ->
            let
                optimizeBranch : ( Can.Expr, Can.Expr ) -> Names.Tracker ( Opt.Expr, Opt.Expr )
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
                            |> Names.fmap (Opt.If optimizedBranches)
                    )

        Can.Let def body ->
            optimize cycle body
                |> Names.bind (optimizeDef cycle def)

        Can.LetRec defs body ->
            case defs of
                [ def ] ->
                    optimizePotentialTailCallDef cycle def
                        |> Names.bind
                            (\tailCallDef ->
                                optimize cycle body
                                    |> Names.fmap (Opt.Let tailCallDef)
                            )

                _ ->
                    List.foldl
                        (\def bod ->
                            Names.bind (optimizeDef cycle def) bod
                        )
                        (optimize cycle body)
                        defs

        Can.LetDestruct pattern expr body ->
            destruct pattern
                |> Names.bind
                    (\( A.At nameRegion name, destructs ) ->
                        optimize cycle expr
                            |> Names.bind
                                (\oexpr ->
                                    optimize cycle body
                                        |> Names.fmap
                                            (\obody ->
                                                Opt.Let (Opt.Def nameRegion name oexpr) (List.foldr Opt.Destruct obody destructs)
                                            )
                                )
                    )

        Can.Case expr branches ->
            let
                optimizeBranch : Name.Name -> Can.CaseBranch -> Names.Tracker ( Can.Pattern, Opt.Expr )
                optimizeBranch root (Can.CaseBranch pattern branch) =
                    destructCase root pattern
                        |> Names.bind
                            (\destructors ->
                                optimize cycle branch
                                    |> Names.fmap
                                        (\obranch ->
                                            ( pattern, List.foldr Opt.Destruct obranch destructors )
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
                                        Opt.VarLocal root ->
                                            Names.traverse (optimizeBranch root) branches
                                                |> Names.fmap (Case.optimize temp root)

                                        Opt.TrackedVarLocal _ root ->
                                            Names.traverse (optimizeBranch root) branches
                                                |> Names.fmap (Case.optimize temp root)

                                        _ ->
                                            Names.traverse (optimizeBranch temp) branches
                                                |> Names.fmap
                                                    (\obranches ->
                                                        Opt.Let (Opt.Def region temp oexpr) (Case.optimize temp temp obranches)
                                                    )
                                )
                    )

        Can.Accessor field ->
            Names.registerField field (Opt.Accessor region field)

        Can.Access record (A.At fieldPosition field) ->
            optimize cycle record
                |> Names.bind
                    (\optRecord ->
                        Names.registerField field (Opt.Access optRecord fieldPosition field)
                    )

        Can.Update record updates ->
            Names.mapTraverse A.toValue A.compareLocated (optimizeUpdate cycle) updates
                |> Names.bind
                    (\optUpdates ->
                        optimize cycle record
                            |> Names.bind
                                (\optRecord ->
                                    Names.registerFieldDict (Utils.mapMapKeys identity A.compareLocated A.toValue updates) (Opt.Update region optRecord optUpdates)
                                )
                    )

        Can.Record fields ->
            Names.mapTraverse A.toValue A.compareLocated (optimize cycle) fields
                |> Names.bind
                    (\optFields ->
                        Names.registerFieldDict (Utils.mapMapKeys identity A.compareLocated A.toValue fields) (Opt.TrackedRecord region optFields)
                    )

        Can.Unit ->
            Names.registerKernel Name.utils Opt.Unit

        Can.Tuple a b cs ->
            optimize cycle a
                |> Names.bind
                    (\optA ->
                        optimize cycle b
                            |> Names.bind
                                (\optB ->
                                    Names.traverse (optimize cycle) cs
                                        |> Names.bind (Names.registerKernel Name.utils << Opt.Tuple region optA optB)
                                )
                    )

        Can.Shader src (Shader.Types attributes uniforms _) ->
            Names.pure (Opt.Shader src (EverySet.fromList identity (Dict.keys compare attributes)) (EverySet.fromList identity (Dict.keys compare uniforms)))



-- UPDATE


optimizeUpdate : Cycle -> Can.FieldUpdate -> Names.Tracker Opt.Expr
optimizeUpdate cycle (Can.FieldUpdate _ expr) =
    optimize cycle expr



-- DEFINITION


optimizeDef : Cycle -> Can.Def -> Opt.Expr -> Names.Tracker Opt.Expr
optimizeDef cycle def body =
    case def of
        Can.Def (A.At region name) args expr ->
            optimizeDefHelp cycle region name args expr body

        Can.TypedDef (A.At region name) _ typedArgs expr _ ->
            optimizeDefHelp cycle region name (List.map Tuple.first typedArgs) expr body


optimizeDefHelp : Cycle -> A.Region -> Name.Name -> List Can.Pattern -> Can.Expr -> Opt.Expr -> Names.Tracker Opt.Expr
optimizeDefHelp cycle region name args expr body =
    case args of
        [] ->
            optimize cycle expr
                |> Names.fmap (\oexpr -> Opt.Let (Opt.Def region name oexpr) body)

        _ ->
            optimize cycle expr
                |> Names.bind
                    (\oexpr ->
                        destructArgs args
                            |> Names.fmap
                                (\( argNames, destructors ) ->
                                    let
                                        ofunc : Opt.Expr
                                        ofunc =
                                            Opt.TrackedFunction argNames (List.foldr Opt.Destruct oexpr destructors)
                                    in
                                    Opt.Let (Opt.Def region name ofunc) body
                                )
                    )



-- DESTRUCTURING


destructArgs : List Can.Pattern -> Names.Tracker ( List (A.Located Name.Name), List Opt.Destructor )
destructArgs args =
    Names.traverse destruct args
        |> Names.fmap List.unzip
        |> Names.fmap
            (\( argNames, destructorLists ) ->
                ( argNames, List.concat destructorLists )
            )


destructCase : Name.Name -> Can.Pattern -> Names.Tracker (List Opt.Destructor)
destructCase rootName pattern =
    destructHelp (Opt.Root rootName) pattern []
        |> Names.fmap List.reverse


destruct : Can.Pattern -> Names.Tracker ( A.Located Name.Name, List Opt.Destructor )
destruct ((A.At region ptrn) as pattern) =
    case ptrn of
        Can.PVar name ->
            Names.pure ( A.At region name, [] )

        Can.PAlias subPattern name ->
            destructHelp (Opt.Root name) subPattern []
                |> Names.fmap (\revDs -> ( A.At region name, List.reverse revDs ))

        _ ->
            Names.generate
                |> Names.bind
                    (\name ->
                        destructHelp (Opt.Root name) pattern []
                            |> Names.fmap
                                (\revDs ->
                                    ( A.At region name, List.reverse revDs )
                                )
                    )


destructHelp : Opt.Path -> Can.Pattern -> List Opt.Destructor -> Names.Tracker (List Opt.Destructor)
destructHelp path (A.At region pattern) revDs =
    case pattern of
        Can.PAnything ->
            Names.pure revDs

        Can.PVar name ->
            Names.pure (Opt.Destructor name path :: revDs)

        Can.PRecord fields ->
            let
                toDestruct : Name.Name -> Opt.Destructor
                toDestruct name =
                    Opt.Destructor name (Opt.Field name path)
            in
            Names.registerFieldList fields (List.map toDestruct fields ++ revDs)

        Can.PAlias subPattern name ->
            destructHelp (Opt.Root name) subPattern <|
                (Opt.Destructor name path :: revDs)

        Can.PUnit ->
            Names.pure revDs

        Can.PTuple a b [] ->
            destructTwo path a b revDs

        Can.PTuple a b [ c ] ->
            case path of
                Opt.Root _ ->
                    destructHelp (Opt.Index Index.first path) a revDs
                        |> Names.bind (destructHelp (Opt.Index Index.second path) b)
                        |> Names.bind (destructHelp (Opt.Index Index.third path) c)

                _ ->
                    Names.generate
                        |> Names.bind
                            (\name ->
                                let
                                    newRoot : Opt.Path
                                    newRoot =
                                        Opt.Root name
                                in
                                destructHelp (Opt.Index Index.first newRoot) a (Opt.Destructor name path :: revDs)
                                    |> Names.bind (destructHelp (Opt.Index Index.second newRoot) b)
                                    |> Names.bind (destructHelp (Opt.Index Index.third newRoot) c)
                            )

        Can.PTuple a b cs ->
            case path of
                Opt.Root _ ->
                    List.foldl (\( index, arg ) -> Names.bind (destructHelp (Opt.ArrayIndex index (Opt.Field "cs" path)) arg))
                        (destructHelp (Opt.Index Index.first path) a revDs
                            |> Names.bind (destructHelp (Opt.Index Index.second path) b)
                        )
                        (List.indexedMap Tuple.pair cs)

                _ ->
                    Names.generate
                        |> Names.bind
                            (\name ->
                                let
                                    newRoot : Opt.Path
                                    newRoot =
                                        Opt.Root name
                                in
                                List.foldl (\( index, arg ) -> Names.bind (destructHelp (Opt.ArrayIndex index (Opt.Field "cs" newRoot)) arg))
                                    (destructHelp (Opt.Index Index.first newRoot) a (Opt.Destructor name path :: revDs)
                                        |> Names.bind (destructHelp (Opt.Index Index.second newRoot) b)
                                    )
                                    (List.indexedMap Tuple.pair cs)
                            )

        Can.PList [] ->
            Names.pure revDs

        Can.PList (hd :: tl) ->
            destructTwo path hd (A.At region (Can.PList tl)) revDs

        Can.PCons hd tl ->
            destructTwo path hd tl revDs

        Can.PChr _ ->
            Names.pure revDs

        Can.PStr _ _ ->
            Names.pure revDs

        Can.PInt _ ->
            Names.pure revDs

        Can.PBool _ _ ->
            Names.pure revDs

        Can.PCtor { union, args } ->
            case args of
                [ Can.PatternCtorArg _ _ arg ] ->
                    let
                        (Can.Union _ _ _ opts) =
                            union
                    in
                    case opts of
                        Can.Normal ->
                            destructHelp (Opt.Index Index.first path) arg revDs

                        Can.Unbox ->
                            destructHelp (Opt.Unbox path) arg revDs

                        Can.Enum ->
                            destructHelp (Opt.Index Index.first path) arg revDs

                _ ->
                    case path of
                        Opt.Root _ ->
                            List.foldl (\arg -> Names.bind (\revDs_ -> destructCtorArg path revDs_ arg))
                                (Names.pure revDs)
                                args

                        _ ->
                            Names.generate
                                |> Names.bind
                                    (\name ->
                                        List.foldl (\arg -> Names.bind (\revDs_ -> destructCtorArg (Opt.Root name) revDs_ arg))
                                            (Names.pure (Opt.Destructor name path :: revDs))
                                            args
                                    )


destructTwo : Opt.Path -> Can.Pattern -> Can.Pattern -> List Opt.Destructor -> Names.Tracker (List Opt.Destructor)
destructTwo path a b revDs =
    case path of
        Opt.Root _ ->
            destructHelp (Opt.Index Index.first path) a revDs
                |> Names.bind (destructHelp (Opt.Index Index.second path) b)

        _ ->
            Names.generate
                |> Names.bind
                    (\name ->
                        let
                            newRoot : Opt.Path
                            newRoot =
                                Opt.Root name
                        in
                        destructHelp (Opt.Index Index.first newRoot) a (Opt.Destructor name path :: revDs)
                            |> Names.bind (destructHelp (Opt.Index Index.second newRoot) b)
                    )


destructCtorArg : Opt.Path -> List Opt.Destructor -> Can.PatternCtorArg -> Names.Tracker (List Opt.Destructor)
destructCtorArg path revDs (Can.PatternCtorArg index _ arg) =
    destructHelp (Opt.Index index path) arg revDs



-- TAIL CALL


optimizePotentialTailCallDef : Cycle -> Can.Def -> Names.Tracker Opt.Def
optimizePotentialTailCallDef cycle def =
    case def of
        Can.Def (A.At region name) args expr ->
            optimizePotentialTailCall cycle region name args expr

        Can.TypedDef (A.At region name) _ typedArgs expr _ ->
            optimizePotentialTailCall cycle region name (List.map Tuple.first typedArgs) expr


optimizePotentialTailCall : Cycle -> A.Region -> Name.Name -> List Can.Pattern -> Can.Expr -> Names.Tracker Opt.Def
optimizePotentialTailCall cycle region name args expr =
    destructArgs args
        |> Names.bind
            (\( argNames, destructors ) ->
                optimizeTail cycle name argNames expr
                    |> Names.fmap (toTailDef region name argNames destructors)
            )


optimizeTail : Cycle -> Name.Name -> List (A.Located Name.Name) -> Can.Expr -> Names.Tracker Opt.Expr
optimizeTail cycle rootName argNames ((A.At region expression) as locExpr) =
    case expression of
        Can.Call func args ->
            Names.traverse (optimize cycle) args
                |> Names.bind
                    (\oargs ->
                        let
                            isMatchingName : Bool
                            isMatchingName =
                                case A.toValue func of
                                    Can.VarLocal name ->
                                        rootName == name

                                    Can.VarTopLevel _ name ->
                                        rootName == name

                                    _ ->
                                        False
                        in
                        if isMatchingName then
                            case Index.indexedZipWith (\_ a b -> ( A.toValue a, b )) argNames oargs of
                                Index.LengthMatch pairs ->
                                    Names.pure (Opt.TailCall rootName pairs)

                                Index.LengthMismatch _ _ ->
                                    optimize cycle func
                                        |> Names.fmap (\ofunc -> Opt.Call region ofunc oargs)

                        else
                            optimize cycle func
                                |> Names.fmap (\ofunc -> Opt.Call region ofunc oargs)
                    )

        Can.If branches finally ->
            let
                optimizeBranch : ( Can.Expr, Can.Expr ) -> Names.Tracker ( Opt.Expr, Opt.Expr )
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
                            |> Names.fmap (Opt.If obranches)
                    )

        Can.Let def body ->
            optimizeTail cycle rootName argNames body
                |> Names.bind (optimizeDef cycle def)

        Can.LetRec defs body ->
            case defs of
                [ def ] ->
                    optimizePotentialTailCallDef cycle def
                        |> Names.bind
                            (\obody ->
                                optimizeTail cycle rootName argNames body
                                    |> Names.fmap (Opt.Let obody)
                            )

                _ ->
                    List.foldl
                        (\def bod ->
                            Names.bind (optimizeDef cycle def) bod
                        )
                        (optimize cycle body)
                        defs

        Can.LetDestruct pattern expr body ->
            destruct pattern
                |> Names.bind
                    (\( A.At dregion dname, destructors ) ->
                        optimize cycle expr
                            |> Names.bind
                                (\oexpr ->
                                    optimizeTail cycle rootName argNames body
                                        |> Names.fmap
                                            (\obody ->
                                                Opt.Let (Opt.Def dregion dname oexpr) (List.foldr Opt.Destruct obody destructors)
                                            )
                                )
                    )

        Can.Case expr branches ->
            let
                optimizeBranch : Name.Name -> Can.CaseBranch -> Names.Tracker ( Can.Pattern, Opt.Expr )
                optimizeBranch root (Can.CaseBranch pattern branch) =
                    destructCase root pattern
                        |> Names.bind
                            (\destructors ->
                                optimizeTail cycle rootName argNames branch
                                    |> Names.fmap
                                        (\obranch ->
                                            ( pattern, List.foldr Opt.Destruct obranch destructors )
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
                                        Opt.VarLocal root ->
                                            Names.traverse (optimizeBranch root) branches
                                                |> Names.fmap (Case.optimize temp root)

                                        Opt.TrackedVarLocal _ root ->
                                            Names.traverse (optimizeBranch root) branches
                                                |> Names.fmap (Case.optimize temp root)

                                        _ ->
                                            Names.traverse (optimizeBranch temp) branches
                                                |> Names.fmap
                                                    (\obranches ->
                                                        Opt.Let (Opt.Def region temp oexpr) (Case.optimize temp temp obranches)
                                                    )
                                )
                    )

        _ ->
            optimize cycle locExpr



-- DETECT TAIL CALLS


toTailDef : A.Region -> Name.Name -> List (A.Located Name.Name) -> List Opt.Destructor -> Opt.Expr -> Opt.Def
toTailDef region name argNames destructors body =
    if hasTailCall body then
        Opt.TailDef region name argNames (List.foldr Opt.Destruct body destructors)

    else
        Opt.Def region name (Opt.TrackedFunction argNames (List.foldr Opt.Destruct body destructors))


hasTailCall : Opt.Expr -> Bool
hasTailCall expression =
    case expression of
        Opt.TailCall _ _ ->
            True

        Opt.If branches finally ->
            hasTailCall finally || List.any (hasTailCall << Tuple.second) branches

        Opt.Let _ body ->
            hasTailCall body

        Opt.Destruct _ body ->
            hasTailCall body

        Opt.Case _ _ decider jumps ->
            decidecHasTailCall decider || List.any (hasTailCall << Tuple.second) jumps

        _ ->
            False


decidecHasTailCall : Opt.Decider Opt.Choice -> Bool
decidecHasTailCall decider =
    case decider of
        Opt.Leaf choice ->
            case choice of
                Opt.Inline expr ->
                    hasTailCall expr

                Opt.Jump _ ->
                    False

        Opt.Chain _ success failure ->
            decidecHasTailCall success || decidecHasTailCall failure

        Opt.FanOut _ tests fallback ->
            decidecHasTailCall fallback || List.any (decidecHasTailCall << Tuple.second) tests
