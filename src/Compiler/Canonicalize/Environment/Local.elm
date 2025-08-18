module Compiler.Canonicalize.Environment.Local exposing (LResult, add)

import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Canonicalize.Type as Type
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Parse.SyntaxVersion exposing (SyntaxVersion)
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
    Dict String Name Can.Union


type alias Aliases =
    Dict String Name Can.Alias


add : Src.Module -> Env.Env -> LResult i w ( Env.Env, Unions, Aliases )
add module_ env =
    addTypes module_ env
        |> R.bind (addVars module_)
        |> R.bind (addCtors module_)



-- ADD VARS


addVars : Src.Module -> Env.Env -> LResult i w Env.Env
addVars module_ env =
    collectVars module_
        |> R.fmap
            (\topLevelVars ->
                let
                    vs2 : Dict String Name Env.Var
                    vs2 =
                        Dict.union topLevelVars env.vars
                in
                -- Use union to overwrite foreign stuff.
                { env | vars = vs2 }
            )


collectVars : Src.Module -> LResult i w (Dict String Name.Name Env.Var)
collectVars (Src.Module _ _ _ _ _ values _ _ _ effects) =
    let
        addDecl : A.Located Src.Value -> Dups.Tracker Env.Var -> Dups.Tracker Env.Var
        addDecl (A.At _ (Src.Value _ ( _, A.At region name ) _ _ _)) =
            Dups.insert name region (Env.TopLevel region)
    in
    Dups.detect Error.DuplicateDecl <|
        List.foldl addDecl (toEffectDups effects) values


toEffectDups : Src.Effects -> Dups.Tracker Env.Var
toEffectDups effects =
    case effects of
        Src.NoEffects ->
            Dups.none

        Src.Ports ports ->
            let
                addPort : Src.Port -> Dups.Tracker Env.Var -> Dups.Tracker Env.Var
                addPort (Src.Port _ ( _, A.At region name ) _) =
                    Dups.insert name region (Env.TopLevel region)
            in
            List.foldl addPort Dups.none ports

        Src.Manager _ manager ->
            case manager of
                Src.Cmd ( _, ( _, A.At region _ ) ) ->
                    Dups.one "command" region (Env.TopLevel region)

                Src.Sub ( _, ( _, A.At region _ ) ) ->
                    Dups.one "subscription" region (Env.TopLevel region)

                Src.Fx ( _, ( _, A.At regionCmd _ ) ) ( _, ( _, A.At regionSub _ ) ) ->
                    Dups.union
                        (Dups.one "command" regionCmd (Env.TopLevel regionCmd))
                        (Dups.one "subscription" regionSub (Env.TopLevel regionSub))



-- ADD TYPES


addTypes : Src.Module -> Env.Env -> LResult i w Env.Env
addTypes (Src.Module syntaxVersion _ _ _ _ _ unions aliases _ _) env =
    let
        addAliasDups : A.Located Src.Alias -> Dups.Tracker () -> Dups.Tracker ()
        addAliasDups (A.At _ (Src.Alias _ ( _, A.At region name ) _ _)) =
            Dups.insert name region ()

        addUnionDups : A.Located Src.Union -> Dups.Tracker () -> Dups.Tracker ()
        addUnionDups (A.At _ (Src.Union ( _, A.At region name ) _ _)) =
            Dups.insert name region ()

        typeNameDups : Dups.Tracker ()
        typeNameDups =
            List.foldl addUnionDups (List.foldl addAliasDups Dups.none aliases) unions
    in
    Dups.detect Error.DuplicateType typeNameDups
        |> R.bind
            (\_ ->
                Utils.foldM (addUnion env.home) env.types unions
                    |> R.bind (\ts1 -> addAliases syntaxVersion aliases <| { env | types = ts1 })
            )


addUnion : IO.Canonical -> Env.Exposed Env.Type -> A.Located Src.Union -> LResult i w (Env.Exposed Env.Type)
addUnion home types ((A.At _ (Src.Union ( _, A.At _ name ) _ _)) as union) =
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


addAliases : SyntaxVersion -> List (A.Located Src.Alias) -> Env.Env -> LResult i w Env.Env
addAliases syntaxVersion aliases env =
    let
        nodes : List ( A.Located Src.Alias, Name, List Name )
        nodes =
            List.map toNode aliases

        sccs : List (Graph.SCC (A.Located Src.Alias))
        sccs =
            Graph.stronglyConnComp nodes
    in
    Utils.foldM (addAlias syntaxVersion) env sccs


addAlias : SyntaxVersion -> Env.Env -> Graph.SCC (A.Located Src.Alias) -> LResult i w Env.Env
addAlias syntaxVersion ({ home, vars, types, ctors, binops, q_vars, q_types, q_ctors } as env) scc =
    case scc of
        Graph.AcyclicSCC ((A.At _ (Src.Alias _ ( _, A.At _ name ) _ ( _, tipe ))) as alias) ->
            checkAliasFreeVars alias
                |> R.bind
                    (\args ->
                        Type.canonicalize syntaxVersion env tipe
                            |> R.bind
                                (\ctype ->
                                    let
                                        one : Env.Info Env.Type
                                        one =
                                            Env.Specific home (Env.Alias (List.length args) home args ctype)

                                        ts1 : Dict String Name (Env.Info Env.Type)
                                        ts1 =
                                            Dict.insert identity name one types
                                    in
                                    R.ok (Env.Env home vars ts1 ctors binops q_vars q_types q_ctors)
                                )
                    )

        Graph.CyclicSCC [] ->
            R.ok env

        Graph.CyclicSCC (((A.At _ (Src.Alias _ ( _, A.At region name1 ) _ ( _, tipe ))) as alias) :: others) ->
            checkAliasFreeVars alias
                |> R.bind
                    (\args ->
                        let
                            toName : A.Located Src.Alias -> Name
                            toName (A.At _ (Src.Alias _ ( _, A.At _ name ) _ _)) =
                                name
                        in
                        R.throw (Error.RecursiveAlias region name1 args tipe (List.map toName others))
                    )



-- DETECT TYPE ALIAS CYCLES


toNode : A.Located Src.Alias -> ( A.Located Src.Alias, Name.Name, List Name.Name )
toNode ((A.At _ (Src.Alias _ ( _, A.At _ name ) _ ( _, tipe ))) as alias) =
    ( alias, name, getEdges tipe [] )


getEdges : Src.Type -> List Name.Name -> List Name.Name
getEdges (A.At _ tipe) edges =
    case tipe of
        Src.TLambda ( _, arg ) ( _, result ) ->
            getEdges result (getEdges arg edges)

        Src.TVar _ ->
            edges

        Src.TType _ name args ->
            List.foldl getEdges (name :: edges) (List.map Src.c1Value args)

        Src.TTypeQual _ _ _ args ->
            List.foldl getEdges edges (List.map Src.c1Value args)

        Src.TRecord fields _ _ ->
            List.foldl (\( _, ( _, ( _, t ) ) ) es -> getEdges t es) edges fields

        Src.TUnit ->
            edges

        Src.TTuple ( _, a ) ( _, b ) cs ->
            List.foldl getEdges (getEdges b (getEdges a edges)) (List.map Src.c2EolValue cs)

        Src.TParens ( _, tipe_ ) ->
            getEdges tipe_ edges



-- CHECK FREE VARIABLES


checkUnionFreeVars : A.Located Src.Union -> LResult i w Int
checkUnionFreeVars (A.At unionRegion (Src.Union ( _, A.At _ name ) args ctors)) =
    let
        addArg : A.Located Name -> Dups.Tracker A.Region -> Dups.Tracker A.Region
        addArg (A.At region arg) dict =
            Dups.insert arg region region dict

        addCtorFreeVars : ( a, List Src.Type ) -> Dict String Name A.Region -> Dict String Name A.Region
        addCtorFreeVars ( _, tipes ) freeVars =
            List.foldl addFreeVars freeVars tipes
    in
    Dups.detect (Error.DuplicateUnionArg name) (List.foldr addArg Dups.none (List.map Src.c1Value args))
        |> R.bind
            (\boundVars ->
                let
                    freeVars : Dict String Name A.Region
                    freeVars =
                        List.foldr addCtorFreeVars Dict.empty (List.map (Src.c2EolValue >> Tuple.mapSecond (List.map Src.c1Value)) ctors)
                in
                case Dict.toList compare (Dict.diff freeVars boundVars) of
                    [] ->
                        R.ok (List.length args)

                    unbound :: unbounds ->
                        R.throw <|
                            Error.TypeVarsUnboundInUnion unionRegion name (List.map (Src.c1Value >> A.toValue) args) unbound unbounds
            )


checkAliasFreeVars : A.Located Src.Alias -> LResult i w (List Name.Name)
checkAliasFreeVars (A.At aliasRegion (Src.Alias _ ( _, A.At _ name ) args ( _, tipe ))) =
    let
        addArg : Src.C1 (A.Located Name) -> Dups.Tracker A.Region -> Dups.Tracker A.Region
        addArg ( _, A.At region arg ) dict =
            Dups.insert arg region region dict
    in
    Dups.detect (Error.DuplicateAliasArg name) (List.foldr addArg Dups.none args)
        |> R.bind
            (\boundVars ->
                let
                    freeVars : Dict String Name A.Region
                    freeVars =
                        addFreeVars tipe Dict.empty

                    overlap : Int
                    overlap =
                        Dict.size (Dict.intersection compare boundVars freeVars)
                in
                if Dict.size boundVars == overlap && Dict.size freeVars == overlap then
                    R.ok (List.map (Src.c1Value >> A.toValue) args)

                else
                    R.throw <|
                        Error.TypeVarsMessedUpInAlias aliasRegion
                            name
                            (List.map (Src.c1Value >> A.toValue) args)
                            (Dict.toList compare (Dict.diff boundVars freeVars))
                            (Dict.toList compare (Dict.diff freeVars boundVars))
            )


addFreeVars : Src.Type -> Dict String Name.Name A.Region -> Dict String Name.Name A.Region
addFreeVars (A.At region tipe) freeVars =
    case tipe of
        Src.TLambda ( _, arg ) ( _, result ) ->
            addFreeVars result (addFreeVars arg freeVars)

        Src.TVar name ->
            Dict.insert identity name region freeVars

        Src.TType _ _ args ->
            List.foldl addFreeVars freeVars (List.map Src.c1Value args)

        Src.TTypeQual _ _ _ args ->
            List.foldl addFreeVars freeVars (List.map Src.c1Value args)

        Src.TRecord fields maybeExt _ ->
            let
                extFreeVars : Dict String Name A.Region
                extFreeVars =
                    case maybeExt of
                        Nothing ->
                            freeVars

                        Just ( _, A.At extRegion ext ) ->
                            Dict.insert identity ext extRegion freeVars
            in
            List.foldl (\( _, ( _, ( _, t ) ) ) fvs -> addFreeVars t fvs) extFreeVars fields

        Src.TUnit ->
            freeVars

        Src.TTuple ( _, a ) ( _, b ) cs ->
            List.foldl addFreeVars (addFreeVars b (addFreeVars a freeVars)) (List.map Src.c2EolValue cs)

        Src.TParens ( _, tipe_ ) ->
            addFreeVars tipe_ freeVars



-- ADD CTORS


addCtors : Src.Module -> Env.Env -> LResult i w ( Env.Env, Unions, Aliases )
addCtors (Src.Module syntaxVersion _ _ _ _ _ unions aliases _ _) env =
    R.traverse (canonicalizeUnion syntaxVersion env) unions
        |> R.bind
            (\unionInfo ->
                R.traverse (canonicalizeAlias syntaxVersion env) aliases
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
                                            cs2 : Dict String Name (Env.Info Env.Ctor)
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


canonicalizeAlias : SyntaxVersion -> Env.Env -> A.Located Src.Alias -> LResult i w ( ( Name.Name, Can.Alias ), CtorDups )
canonicalizeAlias syntaxVersion ({ home } as env) (A.At _ (Src.Alias _ ( _, A.At region name ) args ( _, tipe ))) =
    let
        vars : List Name
        vars =
            List.map (Src.c1Value >> A.toValue) args
    in
    Type.canonicalize syntaxVersion env tipe
        |> R.bind
            (\ctipe ->
                R.ok
                    ( ( name, Can.Alias vars ctipe )
                    , case ctipe of
                        Can.TRecord fields Nothing ->
                            Dups.one name region (Env.Specific home (toRecordCtor home name vars fields))

                        _ ->
                            Dups.none
                    )
            )


toRecordCtor : IO.Canonical -> Name.Name -> List Name.Name -> Dict String Name.Name Can.FieldType -> Env.Ctor
toRecordCtor home name vars fields =
    let
        avars : List ( Name, Can.Type )
        avars =
            List.map (\var -> ( var, Can.TVar var )) vars

        alias : Can.Type
        alias =
            List.foldr
                (\( _, t1 ) t2 -> Can.TLambda t1 t2)
                (Can.TAlias home name avars (Can.Filled (Can.TRecord fields Nothing)))
                (Can.fieldsToList fields)
    in
    Env.RecordCtor home vars alias



-- CANONICALIZE UNION


canonicalizeUnion : SyntaxVersion -> Env.Env -> A.Located Src.Union -> LResult i w ( ( Name.Name, Can.Union ), CtorDups )
canonicalizeUnion syntaxVersion ({ home } as env) (A.At _ (Src.Union ( _, A.At _ name ) avars ctors)) =
    R.indexedTraverse (canonicalizeCtor syntaxVersion env) (List.map (Tuple.mapSecond (List.map Src.c1Value)) (List.map Src.c2EolValue ctors))
        |> R.bind
            (\cctors ->
                let
                    vars : List Name
                    vars =
                        List.map (Src.c1Value >> A.toValue) avars

                    alts : List Can.Ctor
                    alts =
                        List.map A.toValue cctors

                    union : Can.Union
                    union =
                        Can.Union vars alts (List.length alts) (toOpts ctors)
                in
                R.ok ( ( name, union ), Dups.unions (List.map (toCtor home name union) cctors) )
            )


canonicalizeCtor : SyntaxVersion -> Env.Env -> Index.ZeroBased -> ( A.Located Name.Name, List Src.Type ) -> LResult i w (A.Located Can.Ctor)
canonicalizeCtor syntaxVersion env index ( A.At region ctor, tipes ) =
    R.traverse (Type.canonicalize syntaxVersion env) tipes
        |> R.bind
            (\ctipes ->
                R.ok <|
                    A.At region <|
                        Can.Ctor ctor index (List.length ctipes) ctipes
            )


toOpts : List (Src.C2Eol ( A.Located Name.Name, List (Src.C1 Src.Type) )) -> Can.CtorOpts
toOpts ctors =
    case ctors of
        [ ( _, ( _, [ _ ] ) ) ] ->
            Can.Unbox

        _ ->
            if List.all (List.isEmpty << Tuple.second) (List.map Src.c2EolValue ctors) then
                Can.Enum

            else
                Can.Normal


toCtor : IO.Canonical -> Name.Name -> Can.Union -> A.Located Can.Ctor -> CtorDups
toCtor home typeName union (A.At region (Can.Ctor name index _ args)) =
    Dups.one name region <|
        Env.Specific home <|
            Env.Ctor home typeName union index args
