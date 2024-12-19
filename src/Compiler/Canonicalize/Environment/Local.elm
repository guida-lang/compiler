module Compiler.Canonicalize.Environment.Local exposing (LResult, add)

import Compiler.AST.Canonical as Can
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Canonicalize.Type as Type
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Result as R
import Data.Graph as Graph
import Data.Map as Dict exposing (Dict)
import Types as T
import Utils.Main as Utils



-- RESULT


type alias LResult i w a =
    R.RResult i w T.CREC_Error a


type alias Unions =
    Dict String T.CDN_Name T.CASTC_Union


type alias Aliases =
    Dict String T.CDN_Name T.CASTC_Alias


add : T.CASTS_Module -> Env.Env -> LResult i w ( Env.Env, Unions, Aliases )
add module_ env =
    addTypes module_ env
        |> R.bind (addVars module_)
        |> R.bind (addCtors module_)



-- ADD VARS


addVars : T.CASTS_Module -> Env.Env -> LResult i w Env.Env
addVars module_ env =
    collectVars module_
        |> R.fmap
            (\topLevelVars ->
                let
                    vs2 : Dict String T.CDN_Name Env.Var
                    vs2 =
                        Dict.union topLevelVars env.vars
                in
                -- Use union to overwrite foreign stuff.
                { env | vars = vs2 }
            )


collectVars : T.CASTS_Module -> LResult i w (Dict String T.CDN_Name Env.Var)
collectVars (T.CASTS_Module _ _ _ _ values _ _ _ effects) =
    let
        addDecl : T.CRA_Located T.CASTS_Value -> Dups.Tracker Env.Var -> Dups.Tracker Env.Var
        addDecl (T.CRA_At _ (T.CASTS_Value (T.CRA_At region name) _ _ _)) =
            Dups.insert name region (Env.TopLevel region)
    in
    Dups.detect T.CREC_DuplicateDecl <|
        List.foldl addDecl (toEffectDups effects) values


toEffectDups : T.CASTS_Effects -> Dups.Tracker Env.Var
toEffectDups effects =
    case effects of
        T.CASTS_NoEffects ->
            Dups.none

        T.CASTS_Ports ports ->
            let
                addPort : T.CASTS_Port -> Dups.Tracker Env.Var -> Dups.Tracker Env.Var
                addPort (T.CASTS_Port (T.CRA_At region name) _) =
                    Dups.insert name region (Env.TopLevel region)
            in
            List.foldl addPort Dups.none ports

        T.CASTS_Manager _ manager ->
            case manager of
                T.CASTS_Cmd (T.CRA_At region _) ->
                    Dups.one "command" region (Env.TopLevel region)

                T.CASTS_Sub (T.CRA_At region _) ->
                    Dups.one "subscription" region (Env.TopLevel region)

                T.CASTS_Fx (T.CRA_At regionCmd _) (T.CRA_At regionSub _) ->
                    Dups.union
                        (Dups.one "command" regionCmd (Env.TopLevel regionCmd))
                        (Dups.one "subscription" regionSub (Env.TopLevel regionSub))



-- ADD TYPES


addTypes : T.CASTS_Module -> Env.Env -> LResult i w Env.Env
addTypes (T.CASTS_Module _ _ _ _ _ unions aliases _ _) env =
    let
        addAliasDups : T.CRA_Located T.CASTS_Alias -> Dups.Tracker () -> Dups.Tracker ()
        addAliasDups (T.CRA_At _ (T.CASTS_Alias (T.CRA_At region name) _ _)) =
            Dups.insert name region ()

        addUnionDups : T.CRA_Located T.CASTS_Union -> Dups.Tracker () -> Dups.Tracker ()
        addUnionDups (T.CRA_At _ (T.CASTS_Union (T.CRA_At region name) _ _)) =
            Dups.insert name region ()

        typeNameDups : Dups.Tracker ()
        typeNameDups =
            List.foldl addUnionDups (List.foldl addAliasDups Dups.none aliases) unions
    in
    Dups.detect T.CREC_DuplicateType typeNameDups
        |> R.bind
            (\_ ->
                Utils.foldM (addUnion env.home) env.types unions
                    |> R.bind (\ts1 -> addAliases aliases <| { env | types = ts1 })
            )


addUnion : T.CEMN_Canonical -> Env.Exposed Env.Type -> T.CRA_Located T.CASTS_Union -> LResult i w (Env.Exposed Env.Type)
addUnion home types ((T.CRA_At _ (T.CASTS_Union (T.CRA_At _ name) _ _)) as union) =
    R.fmap
        (\arity ->
            let
                one : Env.Info Env.Type
                one =
                    Env.Specific home (Env.Union arity home)
            in
            Dict.insert identity name one types
        )
        (checkUnionFreeVars union)



-- ADD TYPE ALIASES


addAliases : List (T.CRA_Located T.CASTS_Alias) -> Env.Env -> LResult i w Env.Env
addAliases aliases env =
    let
        nodes : List ( T.CRA_Located T.CASTS_Alias, T.CDN_Name, List T.CDN_Name )
        nodes =
            List.map toNode aliases

        sccs : List (Graph.SCC (T.CRA_Located T.CASTS_Alias))
        sccs =
            Graph.stronglyConnComp nodes
    in
    Utils.foldM addAlias env sccs


addAlias : Env.Env -> Graph.SCC (T.CRA_Located T.CASTS_Alias) -> LResult i w Env.Env
addAlias ({ home, vars, types, ctors, binops, q_vars, q_types, q_ctors } as env) scc =
    case scc of
        Graph.AcyclicSCC ((T.CRA_At _ (T.CASTS_Alias (T.CRA_At _ name) _ tipe)) as alias) ->
            checkAliasFreeVars alias
                |> R.bind
                    (\args ->
                        Type.canonicalize env tipe
                            |> R.bind
                                (\ctype ->
                                    let
                                        one : Env.Info Env.Type
                                        one =
                                            Env.Specific home (Env.Alias (List.length args) home args ctype)

                                        ts1 : Dict String T.CDN_Name (Env.Info Env.Type)
                                        ts1 =
                                            Dict.insert identity name one types
                                    in
                                    R.ok (Env.Env home vars ts1 ctors binops q_vars q_types q_ctors)
                                )
                    )

        Graph.CyclicSCC [] ->
            R.ok env

        Graph.CyclicSCC (((T.CRA_At _ (T.CASTS_Alias (T.CRA_At region name1) _ tipe)) as alias) :: others) ->
            checkAliasFreeVars alias
                |> R.bind
                    (\args ->
                        let
                            toName : T.CRA_Located T.CASTS_Alias -> T.CDN_Name
                            toName (T.CRA_At _ (T.CASTS_Alias (T.CRA_At _ name) _ _)) =
                                name
                        in
                        R.throw (T.CREC_RecursiveAlias region name1 args tipe (List.map toName others))
                    )



-- DETECT TYPE ALIAS CYCLES


toNode : T.CRA_Located T.CASTS_Alias -> ( T.CRA_Located T.CASTS_Alias, T.CDN_Name, List T.CDN_Name )
toNode ((T.CRA_At _ (T.CASTS_Alias (T.CRA_At _ name) _ tipe)) as alias) =
    ( alias, name, getEdges tipe [] )


getEdges : T.CASTS_Type -> List T.CDN_Name -> List T.CDN_Name
getEdges (T.CRA_At _ tipe) edges =
    case tipe of
        T.CASTS_TLambda arg result ->
            getEdges result (getEdges arg edges)

        T.CASTS_TVar _ ->
            edges

        T.CASTS_TType _ name args ->
            List.foldl getEdges (name :: edges) args

        T.CASTS_TTypeQual _ _ _ args ->
            List.foldl getEdges edges args

        T.CASTS_TRecord fields _ ->
            List.foldl (\( _, t ) es -> getEdges t es) edges fields

        T.CASTS_TUnit ->
            edges

        T.CASTS_TTuple a b cs ->
            List.foldl getEdges (getEdges b (getEdges a edges)) cs



-- CHECK FREE VARIABLES


checkUnionFreeVars : T.CRA_Located T.CASTS_Union -> LResult i w Int
checkUnionFreeVars (T.CRA_At unionRegion (T.CASTS_Union (T.CRA_At _ name) args ctors)) =
    let
        addArg : T.CRA_Located T.CDN_Name -> Dups.Tracker T.CRA_Region -> Dups.Tracker T.CRA_Region
        addArg (T.CRA_At region arg) dict =
            Dups.insert arg region region dict

        addCtorFreeVars : ( a, List T.CASTS_Type ) -> Dict String T.CDN_Name T.CRA_Region -> Dict String T.CDN_Name T.CRA_Region
        addCtorFreeVars ( _, tipes ) freeVars =
            List.foldl addFreeVars freeVars tipes
    in
    Dups.detect (T.CREC_DuplicateUnionArg name) (List.foldr addArg Dups.none args)
        |> R.bind
            (\boundVars ->
                let
                    freeVars : Dict String T.CDN_Name T.CRA_Region
                    freeVars =
                        List.foldr addCtorFreeVars Dict.empty ctors
                in
                case Dict.toList compare (Dict.diff freeVars boundVars) of
                    [] ->
                        R.ok (List.length args)

                    unbound :: unbounds ->
                        R.throw <|
                            T.CREC_TypeVarsUnboundInUnion unionRegion name (List.map A.toValue args) unbound unbounds
            )


checkAliasFreeVars : T.CRA_Located T.CASTS_Alias -> LResult i w (List T.CDN_Name)
checkAliasFreeVars (T.CRA_At aliasRegion (T.CASTS_Alias (T.CRA_At _ name) args tipe)) =
    let
        addArg : T.CRA_Located T.CDN_Name -> Dups.Tracker T.CRA_Region -> Dups.Tracker T.CRA_Region
        addArg (T.CRA_At region arg) dict =
            Dups.insert arg region region dict
    in
    Dups.detect (T.CREC_DuplicateAliasArg name) (List.foldr addArg Dups.none args)
        |> R.bind
            (\boundVars ->
                let
                    freeVars : Dict String T.CDN_Name T.CRA_Region
                    freeVars =
                        addFreeVars tipe Dict.empty

                    overlap : Int
                    overlap =
                        Dict.size (Dict.intersection compare boundVars freeVars)
                in
                if Dict.size boundVars == overlap && Dict.size freeVars == overlap then
                    R.ok (List.map A.toValue args)

                else
                    R.throw <|
                        T.CREC_TypeVarsMessedUpInAlias aliasRegion
                            name
                            (List.map A.toValue args)
                            (Dict.toList compare (Dict.diff boundVars freeVars))
                            (Dict.toList compare (Dict.diff freeVars boundVars))
            )


addFreeVars : T.CASTS_Type -> Dict String T.CDN_Name T.CRA_Region -> Dict String T.CDN_Name T.CRA_Region
addFreeVars (T.CRA_At region tipe) freeVars =
    case tipe of
        T.CASTS_TLambda arg result ->
            addFreeVars result (addFreeVars arg freeVars)

        T.CASTS_TVar name ->
            Dict.insert identity name region freeVars

        T.CASTS_TType _ _ args ->
            List.foldl addFreeVars freeVars args

        T.CASTS_TTypeQual _ _ _ args ->
            List.foldl addFreeVars freeVars args

        T.CASTS_TRecord fields maybeExt ->
            let
                extFreeVars : Dict String T.CDN_Name T.CRA_Region
                extFreeVars =
                    case maybeExt of
                        Nothing ->
                            freeVars

                        Just (T.CRA_At extRegion ext) ->
                            Dict.insert identity ext extRegion freeVars
            in
            List.foldl (\( _, t ) fvs -> addFreeVars t fvs) extFreeVars fields

        T.CASTS_TUnit ->
            freeVars

        T.CASTS_TTuple a b cs ->
            List.foldl addFreeVars (addFreeVars b (addFreeVars a freeVars)) cs



-- ADD CTORS


addCtors : T.CASTS_Module -> Env.Env -> LResult i w ( Env.Env, Unions, Aliases )
addCtors (T.CASTS_Module _ _ _ _ _ unions aliases _ _) env =
    R.traverse (canonicalizeUnion env) unions
        |> R.bind
            (\unionInfo ->
                R.traverse (canonicalizeAlias env) aliases
                    |> R.bind
                        (\aliasInfo ->
                            (Dups.detect T.CREC_DuplicateCtor <|
                                Dups.union
                                    (Dups.unions (List.map Tuple.second unionInfo))
                                    (Dups.unions (List.map Tuple.second aliasInfo))
                            )
                                |> R.bind
                                    (\ctors ->
                                        let
                                            cs2 : Dict String T.CDN_Name (Env.Info Env.Ctor)
                                            cs2 =
                                                Dict.union ctors env.ctors
                                        in
                                        R.ok
                                            ( { env | ctors = cs2 }
                                            , Dict.fromList identity (List.map Tuple.first unionInfo)
                                            , Dict.fromList identity (List.map Tuple.first aliasInfo)
                                            )
                                    )
                        )
            )


type alias CtorDups =
    Dups.Tracker (Env.Info Env.Ctor)



-- CANONICALIZE ALIAS


canonicalizeAlias : Env.Env -> T.CRA_Located T.CASTS_Alias -> LResult i w ( ( T.CDN_Name, T.CASTC_Alias ), CtorDups )
canonicalizeAlias ({ home } as env) (T.CRA_At _ (T.CASTS_Alias (T.CRA_At region name) args tipe)) =
    let
        vars : List T.CDN_Name
        vars =
            List.map A.toValue args
    in
    Type.canonicalize env tipe
        |> R.bind
            (\ctipe ->
                R.ok
                    ( ( name, T.CASTC_Alias vars ctipe )
                    , case ctipe of
                        T.CASTC_TRecord fields Nothing ->
                            Dups.one name region (Env.Specific home (toRecordCtor home name vars fields))

                        _ ->
                            Dups.none
                    )
            )


toRecordCtor : T.CEMN_Canonical -> T.CDN_Name -> List T.CDN_Name -> Dict String T.CDN_Name T.CASTC_FieldType -> Env.Ctor
toRecordCtor home name vars fields =
    let
        avars : List ( T.CDN_Name, T.CASTC_Type )
        avars =
            List.map (\var -> ( var, T.CASTC_TVar var )) vars

        alias : T.CASTC_Type
        alias =
            List.foldr
                (\( _, t1 ) t2 -> T.CASTC_TLambda t1 t2)
                (T.CASTC_TAlias home name avars (T.CASTC_Filled (T.CASTC_TRecord fields Nothing)))
                (Can.fieldsToList fields)
    in
    Env.RecordCtor home vars alias



-- CANONICALIZE UNION


canonicalizeUnion : Env.Env -> T.CRA_Located T.CASTS_Union -> LResult i w ( ( T.CDN_Name, T.CASTC_Union ), CtorDups )
canonicalizeUnion ({ home } as env) (T.CRA_At _ (T.CASTS_Union (T.CRA_At _ name) avars ctors)) =
    R.indexedTraverse (canonicalizeCtor env) ctors
        |> R.bind
            (\cctors ->
                let
                    vars : List T.CDN_Name
                    vars =
                        List.map A.toValue avars

                    alts : List T.CASTC_Ctor
                    alts =
                        List.map A.toValue cctors

                    union : T.CASTC_Union
                    union =
                        T.CASTC_Union vars alts (List.length alts) (toOpts ctors)
                in
                R.ok ( ( name, union ), Dups.unions (List.map (toCtor home name union) cctors) )
            )


canonicalizeCtor : Env.Env -> T.CDI_ZeroBased -> ( T.CRA_Located T.CDN_Name, List T.CASTS_Type ) -> LResult i w (T.CRA_Located T.CASTC_Ctor)
canonicalizeCtor env index ( T.CRA_At region ctor, tipes ) =
    R.traverse (Type.canonicalize env) tipes
        |> R.bind
            (\ctipes ->
                R.ok <|
                    T.CRA_At region <|
                        T.CASTC_Ctor ctor index (List.length ctipes) ctipes
            )


toOpts : List ( T.CRA_Located T.CDN_Name, List T.CASTS_Type ) -> T.CASTC_CtorOpts
toOpts ctors =
    case ctors of
        [ ( _, [ _ ] ) ] ->
            T.CASTC_Unbox

        _ ->
            if List.all (List.isEmpty << Tuple.second) ctors then
                T.CASTC_Enum

            else
                T.CASTC_Normal


toCtor : T.CEMN_Canonical -> T.CDN_Name -> T.CASTC_Union -> T.CRA_Located T.CASTC_Ctor -> CtorDups
toCtor home typeName union (T.CRA_At region (T.CASTC_Ctor name index _ args)) =
    Dups.one name region <|
        Env.Specific home <|
            Env.Ctor home typeName union index args
