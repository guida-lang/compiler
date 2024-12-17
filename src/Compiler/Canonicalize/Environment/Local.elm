module Compiler.Canonicalize.Environment.Local exposing (LResult, add)

import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Canonicalize.Type as Type
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name exposing (CDN_Name)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as R
import Data.Graph as Graph
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO
import Utils.Main as Utils



-- RESULT


type alias LResult i w a =
    R.RResult i w Error.Error a


type alias Unions =
    Dict String CDN_Name Can.CASTC_Union


type alias Aliases =
    Dict String CDN_Name Can.CASTC_Alias


add : Src.CASTS_Module -> Env.Env -> LResult i w ( Env.Env, Unions, Aliases )
add module_ env =
    addTypes module_ env
        |> R.bind (addVars module_)
        |> R.bind (addCtors module_)



-- ADD VARS


addVars : Src.CASTS_Module -> Env.Env -> LResult i w Env.Env
addVars module_ env =
    collectVars module_
        |> R.fmap
            (\topLevelVars ->
                let
                    vs2 : Dict String CDN_Name Env.Var
                    vs2 =
                        Dict.union topLevelVars env.vars
                in
                -- Use union to overwrite foreign stuff.
                { env | vars = vs2 }
            )


collectVars : Src.CASTS_Module -> LResult i w (Dict String Name.CDN_Name Env.Var)
collectVars (Src.CASTS_Module _ _ _ _ values _ _ _ effects) =
    let
        addDecl : A.CRA_Located Src.CASTS_Value -> Dups.Tracker Env.Var -> Dups.Tracker Env.Var
        addDecl (A.CRA_At _ (Src.CASTS_Value (A.CRA_At region name) _ _ _)) =
            Dups.insert name region (Env.TopLevel region)
    in
    Dups.detect Error.DuplicateDecl <|
        List.foldl addDecl (toEffectDups effects) values


toEffectDups : Src.CASTS_Effects -> Dups.Tracker Env.Var
toEffectDups effects =
    case effects of
        Src.CASTS_NoEffects ->
            Dups.none

        Src.CASTS_Ports ports ->
            let
                addPort : Src.CASTS_Port -> Dups.Tracker Env.Var -> Dups.Tracker Env.Var
                addPort (Src.CASTS_Port (A.CRA_At region name) _) =
                    Dups.insert name region (Env.TopLevel region)
            in
            List.foldl addPort Dups.none ports

        Src.CASTS_Manager _ manager ->
            case manager of
                Src.CASTS_Cmd (A.CRA_At region _) ->
                    Dups.one "command" region (Env.TopLevel region)

                Src.CASTS_Sub (A.CRA_At region _) ->
                    Dups.one "subscription" region (Env.TopLevel region)

                Src.CASTS_Fx (A.CRA_At regionCmd _) (A.CRA_At regionSub _) ->
                    Dups.union
                        (Dups.one "command" regionCmd (Env.TopLevel regionCmd))
                        (Dups.one "subscription" regionSub (Env.TopLevel regionSub))



-- ADD TYPES


addTypes : Src.CASTS_Module -> Env.Env -> LResult i w Env.Env
addTypes (Src.CASTS_Module _ _ _ _ _ unions aliases _ _) env =
    let
        addAliasDups : A.CRA_Located Src.CASTS_Alias -> Dups.Tracker () -> Dups.Tracker ()
        addAliasDups (A.CRA_At _ (Src.CASTS_Alias (A.CRA_At region name) _ _)) =
            Dups.insert name region ()

        addUnionDups : A.CRA_Located Src.CASTS_Union -> Dups.Tracker () -> Dups.Tracker ()
        addUnionDups (A.CRA_At _ (Src.CASTS_Union (A.CRA_At region name) _ _)) =
            Dups.insert name region ()

        typeNameDups : Dups.Tracker ()
        typeNameDups =
            List.foldl addUnionDups (List.foldl addAliasDups Dups.none aliases) unions
    in
    Dups.detect Error.DuplicateType typeNameDups
        |> R.bind
            (\_ ->
                Utils.foldM (addUnion env.home) env.types unions
                    |> R.bind (\ts1 -> addAliases aliases <| { env | types = ts1 })
            )


addUnion : IO.CEMN_Canonical -> Env.Exposed Env.Type -> A.CRA_Located Src.CASTS_Union -> LResult i w (Env.Exposed Env.Type)
addUnion home types ((A.CRA_At _ (Src.CASTS_Union (A.CRA_At _ name) _ _)) as union) =
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


addAliases : List (A.CRA_Located Src.CASTS_Alias) -> Env.Env -> LResult i w Env.Env
addAliases aliases env =
    let
        nodes : List ( A.CRA_Located Src.CASTS_Alias, CDN_Name, List CDN_Name )
        nodes =
            List.map toNode aliases

        sccs : List (Graph.SCC (A.CRA_Located Src.CASTS_Alias))
        sccs =
            Graph.stronglyConnComp nodes
    in
    Utils.foldM addAlias env sccs


addAlias : Env.Env -> Graph.SCC (A.CRA_Located Src.CASTS_Alias) -> LResult i w Env.Env
addAlias ({ home, vars, types, ctors, binops, q_vars, q_types, q_ctors } as env) scc =
    case scc of
        Graph.AcyclicSCC ((A.CRA_At _ (Src.CASTS_Alias (A.CRA_At _ name) _ tipe)) as alias) ->
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

                                        ts1 : Dict String CDN_Name (Env.Info Env.Type)
                                        ts1 =
                                            Dict.insert identity name one types
                                    in
                                    R.ok (Env.Env home vars ts1 ctors binops q_vars q_types q_ctors)
                                )
                    )

        Graph.CyclicSCC [] ->
            R.ok env

        Graph.CyclicSCC (((A.CRA_At _ (Src.CASTS_Alias (A.CRA_At region name1) _ tipe)) as alias) :: others) ->
            checkAliasFreeVars alias
                |> R.bind
                    (\args ->
                        let
                            toName : A.CRA_Located Src.CASTS_Alias -> CDN_Name
                            toName (A.CRA_At _ (Src.CASTS_Alias (A.CRA_At _ name) _ _)) =
                                name
                        in
                        R.throw (Error.RecursiveAlias region name1 args tipe (List.map toName others))
                    )



-- DETECT TYPE ALIAS CYCLES


toNode : A.CRA_Located Src.CASTS_Alias -> ( A.CRA_Located Src.CASTS_Alias, Name.CDN_Name, List Name.CDN_Name )
toNode ((A.CRA_At _ (Src.CASTS_Alias (A.CRA_At _ name) _ tipe)) as alias) =
    ( alias, name, getEdges tipe [] )


getEdges : Src.CASTS_Type -> List Name.CDN_Name -> List Name.CDN_Name
getEdges (A.CRA_At _ tipe) edges =
    case tipe of
        Src.CASTS_TLambda arg result ->
            getEdges result (getEdges arg edges)

        Src.CASTS_TVar _ ->
            edges

        Src.CASTS_TType _ name args ->
            List.foldl getEdges (name :: edges) args

        Src.CASTS_TTypeQual _ _ _ args ->
            List.foldl getEdges edges args

        Src.CASTS_TRecord fields _ ->
            List.foldl (\( _, t ) es -> getEdges t es) edges fields

        Src.CASTS_TUnit ->
            edges

        Src.CASTS_TTuple a b cs ->
            List.foldl getEdges (getEdges b (getEdges a edges)) cs



-- CHECK FREE VARIABLES


checkUnionFreeVars : A.CRA_Located Src.CASTS_Union -> LResult i w Int
checkUnionFreeVars (A.CRA_At unionRegion (Src.CASTS_Union (A.CRA_At _ name) args ctors)) =
    let
        addArg : A.CRA_Located CDN_Name -> Dups.Tracker A.CRA_Region -> Dups.Tracker A.CRA_Region
        addArg (A.CRA_At region arg) dict =
            Dups.insert arg region region dict

        addCtorFreeVars : ( a, List Src.CASTS_Type ) -> Dict String CDN_Name A.CRA_Region -> Dict String CDN_Name A.CRA_Region
        addCtorFreeVars ( _, tipes ) freeVars =
            List.foldl addFreeVars freeVars tipes
    in
    Dups.detect (Error.DuplicateUnionArg name) (List.foldr addArg Dups.none args)
        |> R.bind
            (\boundVars ->
                let
                    freeVars : Dict String CDN_Name A.CRA_Region
                    freeVars =
                        List.foldr addCtorFreeVars Dict.empty ctors
                in
                case Dict.toList compare (Dict.diff freeVars boundVars) of
                    [] ->
                        R.ok (List.length args)

                    unbound :: unbounds ->
                        R.throw <|
                            Error.TypeVarsUnboundInUnion unionRegion name (List.map A.toValue args) unbound unbounds
            )


checkAliasFreeVars : A.CRA_Located Src.CASTS_Alias -> LResult i w (List Name.CDN_Name)
checkAliasFreeVars (A.CRA_At aliasRegion (Src.CASTS_Alias (A.CRA_At _ name) args tipe)) =
    let
        addArg : A.CRA_Located CDN_Name -> Dups.Tracker A.CRA_Region -> Dups.Tracker A.CRA_Region
        addArg (A.CRA_At region arg) dict =
            Dups.insert arg region region dict
    in
    Dups.detect (Error.DuplicateAliasArg name) (List.foldr addArg Dups.none args)
        |> R.bind
            (\boundVars ->
                let
                    freeVars : Dict String CDN_Name A.CRA_Region
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
                        Error.TypeVarsMessedUpInAlias aliasRegion
                            name
                            (List.map A.toValue args)
                            (Dict.toList compare (Dict.diff boundVars freeVars))
                            (Dict.toList compare (Dict.diff freeVars boundVars))
            )


addFreeVars : Src.CASTS_Type -> Dict String Name.CDN_Name A.CRA_Region -> Dict String Name.CDN_Name A.CRA_Region
addFreeVars (A.CRA_At region tipe) freeVars =
    case tipe of
        Src.CASTS_TLambda arg result ->
            addFreeVars result (addFreeVars arg freeVars)

        Src.CASTS_TVar name ->
            Dict.insert identity name region freeVars

        Src.CASTS_TType _ _ args ->
            List.foldl addFreeVars freeVars args

        Src.CASTS_TTypeQual _ _ _ args ->
            List.foldl addFreeVars freeVars args

        Src.CASTS_TRecord fields maybeExt ->
            let
                extFreeVars : Dict String CDN_Name A.CRA_Region
                extFreeVars =
                    case maybeExt of
                        Nothing ->
                            freeVars

                        Just (A.CRA_At extRegion ext) ->
                            Dict.insert identity ext extRegion freeVars
            in
            List.foldl (\( _, t ) fvs -> addFreeVars t fvs) extFreeVars fields

        Src.CASTS_TUnit ->
            freeVars

        Src.CASTS_TTuple a b cs ->
            List.foldl addFreeVars (addFreeVars b (addFreeVars a freeVars)) cs



-- ADD CTORS


addCtors : Src.CASTS_Module -> Env.Env -> LResult i w ( Env.Env, Unions, Aliases )
addCtors (Src.CASTS_Module _ _ _ _ _ unions aliases _ _) env =
    R.traverse (canonicalizeUnion env) unions
        |> R.bind
            (\unionInfo ->
                R.traverse (canonicalizeAlias env) aliases
                    |> R.bind
                        (\aliasInfo ->
                            (Dups.detect Error.DuplicateCtor <|
                                Dups.union
                                    (Dups.unions (List.map Tuple.second unionInfo))
                                    (Dups.unions (List.map Tuple.second aliasInfo))
                            )
                                |> R.bind
                                    (\ctors ->
                                        let
                                            cs2 : Dict String CDN_Name (Env.Info Env.Ctor)
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


canonicalizeAlias : Env.Env -> A.CRA_Located Src.CASTS_Alias -> LResult i w ( ( Name.CDN_Name, Can.CASTC_Alias ), CtorDups )
canonicalizeAlias ({ home } as env) (A.CRA_At _ (Src.CASTS_Alias (A.CRA_At region name) args tipe)) =
    let
        vars : List CDN_Name
        vars =
            List.map A.toValue args
    in
    Type.canonicalize env tipe
        |> R.bind
            (\ctipe ->
                R.ok
                    ( ( name, Can.CASTC_Alias vars ctipe )
                    , case ctipe of
                        Can.CASTC_TRecord fields Nothing ->
                            Dups.one name region (Env.Specific home (toRecordCtor home name vars fields))

                        _ ->
                            Dups.none
                    )
            )


toRecordCtor : IO.CEMN_Canonical -> Name.CDN_Name -> List Name.CDN_Name -> Dict String Name.CDN_Name Can.CASTC_FieldType -> Env.Ctor
toRecordCtor home name vars fields =
    let
        avars : List ( CDN_Name, Can.CASTC_Type )
        avars =
            List.map (\var -> ( var, Can.CASTC_TVar var )) vars

        alias : Can.CASTC_Type
        alias =
            List.foldr
                (\( _, t1 ) t2 -> Can.CASTC_TLambda t1 t2)
                (Can.CASTC_TAlias home name avars (Can.CASTC_Filled (Can.CASTC_TRecord fields Nothing)))
                (Can.fieldsToList fields)
    in
    Env.RecordCtor home vars alias



-- CANONICALIZE UNION


canonicalizeUnion : Env.Env -> A.CRA_Located Src.CASTS_Union -> LResult i w ( ( Name.CDN_Name, Can.CASTC_Union ), CtorDups )
canonicalizeUnion ({ home } as env) (A.CRA_At _ (Src.CASTS_Union (A.CRA_At _ name) avars ctors)) =
    R.indexedTraverse (canonicalizeCtor env) ctors
        |> R.bind
            (\cctors ->
                let
                    vars : List CDN_Name
                    vars =
                        List.map A.toValue avars

                    alts : List Can.CASTC_Ctor
                    alts =
                        List.map A.toValue cctors

                    union : Can.CASTC_Union
                    union =
                        Can.CASTC_Union vars alts (List.length alts) (toOpts ctors)
                in
                R.ok ( ( name, union ), Dups.unions (List.map (toCtor home name union) cctors) )
            )


canonicalizeCtor : Env.Env -> Index.CDI_ZeroBased -> ( A.CRA_Located Name.CDN_Name, List Src.CASTS_Type ) -> LResult i w (A.CRA_Located Can.CASTC_Ctor)
canonicalizeCtor env index ( A.CRA_At region ctor, tipes ) =
    R.traverse (Type.canonicalize env) tipes
        |> R.bind
            (\ctipes ->
                R.ok <|
                    A.CRA_At region <|
                        Can.CASTC_Ctor ctor index (List.length ctipes) ctipes
            )


toOpts : List ( A.CRA_Located Name.CDN_Name, List Src.CASTS_Type ) -> Can.CASTC_CtorOpts
toOpts ctors =
    case ctors of
        [ ( _, [ _ ] ) ] ->
            Can.CASTC_Unbox

        _ ->
            if List.all (List.isEmpty << Tuple.second) ctors then
                Can.CASTC_Enum

            else
                Can.CASTC_Normal


toCtor : IO.CEMN_Canonical -> Name.CDN_Name -> Can.CASTC_Union -> A.CRA_Located Can.CASTC_Ctor -> CtorDups
toCtor home typeName union (A.CRA_At region (Can.CASTC_Ctor name index _ args)) =
    Dups.one name region <|
        Env.Specific home <|
            Env.Ctor home typeName union index args
