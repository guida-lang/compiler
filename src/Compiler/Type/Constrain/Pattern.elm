module Compiler.Type.Constrain.Pattern exposing
    ( Header
    , State(..)
    , add
    , emptyState
    )

import Compiler.AST.Canonical as Can
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Type as E
import Compiler.Type.Instantiate as Instantiate
import Compiler.Type.Type as Type exposing (Type)
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO exposing (IO)



-- ACTUALLY ADD CONSTRAINTS
-- The constraints are stored in reverse order so that adding a new
-- constraint is O(1) and we can reverse it at some later time.


type State
    = State Header (List IO.Variable) (List Type.Constraint)


type alias Header =
    Dict String Name.Name (A.Located Type)


add : Can.Pattern -> E.PExpected Type -> State -> IO State
add (A.At region pattern) expectation state =
    case pattern of
        Can.PAnything ->
            IO.pure state

        Can.PVar name ->
            IO.pure (addToHeaders region name expectation state)

        Can.PAlias realPattern name ->
            add realPattern expectation (addToHeaders region name expectation state)

        Can.PUnit ->
            let
                (State headers vars revCons) =
                    state

                unitCon : Type.Constraint
                unitCon =
                    Type.CPattern region E.PUnit Type.UnitN expectation
            in
            IO.pure (State headers vars (unitCon :: revCons))

        Can.PTuple a b cs ->
            addTuple region a b cs expectation state

        Can.PCtor { home, type_, union, name, args } ->
            let
                (Can.Union typeVars _ _ _) =
                    union
            in
            addCtor region home type_ typeVars name args expectation state

        Can.PList patterns ->
            Type.mkFlexVar
                |> IO.bind
                    (\entryVar ->
                        let
                            entryType : Type
                            entryType =
                                Type.VarN entryVar

                            listType : Type
                            listType =
                                Type.AppN ModuleName.list Name.list [ entryType ]
                        in
                        IO.foldM (addEntry region entryType) state (Index.indexedMap Tuple.pair patterns)
                            |> IO.fmap
                                (\(State headers vars revCons) ->
                                    let
                                        listCon : Type.Constraint
                                        listCon =
                                            Type.CPattern region E.PList listType expectation
                                    in
                                    State headers (entryVar :: vars) (listCon :: revCons)
                                )
                    )

        Can.PCons headPattern tailPattern ->
            Type.mkFlexVar
                |> IO.bind
                    (\entryVar ->
                        let
                            entryType : Type
                            entryType =
                                Type.VarN entryVar

                            listType : Type
                            listType =
                                Type.AppN ModuleName.list Name.list [ entryType ]

                            headExpectation : E.PExpected Type
                            headExpectation =
                                E.PNoExpectation entryType

                            tailExpectation : E.PExpected Type
                            tailExpectation =
                                E.PFromContext region E.PTail listType
                        in
                        add tailPattern tailExpectation state
                            |> IO.bind (add headPattern headExpectation)
                            |> IO.fmap
                                (\(State headers vars revCons) ->
                                    let
                                        listCon : Type.Constraint
                                        listCon =
                                            Type.CPattern region E.PList listType expectation
                                    in
                                    State headers (entryVar :: vars) (listCon :: revCons)
                                )
                    )

        Can.PRecord fields ->
            Type.mkFlexVar
                |> IO.bind
                    (\extVar ->
                        let
                            extType : Type
                            extType =
                                Type.VarN extVar
                        in
                        IO.traverseList (\field -> IO.fmap (Tuple.pair field) Type.mkFlexVar) fields
                            |> IO.fmap
                                (\fieldVars ->
                                    let
                                        fieldTypes : Dict String Name.Name Type
                                        fieldTypes =
                                            Dict.fromList identity (List.map (Tuple.mapSecond Type.VarN) fieldVars)

                                        recordType : Type
                                        recordType =
                                            Type.RecordN fieldTypes extType

                                        (State headers vars revCons) =
                                            state

                                        recordCon : Type.Constraint
                                        recordCon =
                                            Type.CPattern region E.PRecord recordType expectation
                                    in
                                    State
                                        (Dict.union headers (Dict.map (\_ v -> A.At region v) fieldTypes))
                                        (List.map Tuple.second fieldVars ++ extVar :: vars)
                                        (recordCon :: revCons)
                                )
                    )

        Can.PInt _ ->
            let
                (State headers vars revCons) =
                    state

                intCon : Type.Constraint
                intCon =
                    Type.CPattern region E.PInt Type.int expectation
            in
            IO.pure (State headers vars (intCon :: revCons))

        Can.PStr _ _ ->
            let
                (State headers vars revCons) =
                    state

                strCon : Type.Constraint
                strCon =
                    Type.CPattern region E.PStr Type.string expectation
            in
            IO.pure (State headers vars (strCon :: revCons))

        Can.PChr _ ->
            let
                (State headers vars revCons) =
                    state

                chrCon : Type.Constraint
                chrCon =
                    Type.CPattern region E.PChr Type.char expectation
            in
            IO.pure (State headers vars (chrCon :: revCons))

        Can.PBool _ _ ->
            let
                (State headers vars revCons) =
                    state

                boolCon : Type.Constraint
                boolCon =
                    Type.CPattern region E.PBool Type.bool expectation
            in
            IO.pure (State headers vars (boolCon :: revCons))



-- STATE HELPERS


emptyState : State
emptyState =
    State Dict.empty [] []


addToHeaders : A.Region -> Name.Name -> E.PExpected Type -> State -> State
addToHeaders region name expectation (State headers vars revCons) =
    let
        tipe : Type
        tipe =
            getType expectation

        newHeaders : Dict String Name.Name (A.Located Type)
        newHeaders =
            Dict.insert identity name (A.At region tipe) headers
    in
    State newHeaders vars revCons


getType : E.PExpected Type -> Type
getType expectation =
    case expectation of
        E.PNoExpectation tipe ->
            tipe

        E.PFromContext _ _ tipe ->
            tipe



-- CONSTRAIN LIST


addEntry : A.Region -> Type -> State -> ( Index.ZeroBased, Can.Pattern ) -> IO State
addEntry listRegion tipe state ( index, pattern ) =
    let
        expectation : E.PExpected Type
        expectation =
            E.PFromContext listRegion (E.PListEntry index) tipe
    in
    add pattern expectation state



-- CONSTRAIN TUPLE


addTuple : A.Region -> Can.Pattern -> Can.Pattern -> List Can.Pattern -> E.PExpected Type -> State -> IO State
addTuple region a b cs expectation state =
    Type.mkFlexVar
        |> IO.bind
            (\aVar ->
                Type.mkFlexVar
                    |> IO.bind
                        (\bVar ->
                            let
                                aType : Type
                                aType =
                                    Type.VarN aVar

                                bType : Type
                                bType =
                                    Type.VarN bVar
                            in
                            simpleAdd a aType state
                                |> IO.bind (simpleAdd b bType)
                                |> IO.bind
                                    (\updatedState ->
                                        IO.foldM
                                            (\( cVars, s ) c ->
                                                Type.mkFlexVar
                                                    |> IO.bind
                                                        (\cVar ->
                                                            simpleAdd c (Type.VarN cVar) s
                                                                |> IO.fmap (Tuple.pair (cVar :: cVars))
                                                        )
                                            )
                                            ( [], updatedState )
                                            cs
                                            |> IO.fmap
                                                (\( cVars, State headers vars revCons ) ->
                                                    let
                                                        tupleCon : Type.Constraint
                                                        tupleCon =
                                                            Type.CPattern region E.PTuple (Type.TupleN aType bType (List.map Type.VarN cVars)) expectation
                                                    in
                                                    State headers (aVar :: bVar :: cVars ++ vars) (tupleCon :: revCons)
                                                )
                                    )
                        )
            )


simpleAdd : Can.Pattern -> Type -> State -> IO State
simpleAdd pattern patternType state =
    add pattern (E.PNoExpectation patternType) state



-- CONSTRAIN CONSTRUCTORS


addCtor : A.Region -> IO.Canonical -> Name.Name -> List Name.Name -> Name.Name -> List Can.PatternCtorArg -> E.PExpected Type -> State -> IO State
addCtor region home typeName typeVarNames ctorName args expectation state =
    IO.traverseList (\var -> IO.fmap (Tuple.pair var) (Type.nameToFlex var)) typeVarNames
        |> IO.bind
            (\varPairs ->
                let
                    typePairs : List ( Name.Name, Type )
                    typePairs =
                        List.map (Tuple.mapSecond Type.VarN) varPairs

                    freeVarDict : Dict String Name.Name Type
                    freeVarDict =
                        Dict.fromList identity typePairs
                in
                IO.foldM (addCtorArg region ctorName freeVarDict) state args
                    |> IO.bind
                        (\(State headers vars revCons) ->
                            let
                                ctorType : Type
                                ctorType =
                                    Type.AppN home typeName (List.map Tuple.second typePairs)

                                ctorCon : Type.Constraint
                                ctorCon =
                                    Type.CPattern region (E.PCtor ctorName) ctorType expectation
                            in
                            IO.pure <|
                                State headers
                                    (List.map Tuple.second varPairs ++ vars)
                                    (ctorCon :: revCons)
                        )
            )


addCtorArg : A.Region -> Name.Name -> Dict String Name.Name Type -> State -> Can.PatternCtorArg -> IO State
addCtorArg region ctorName freeVarDict state (Can.PatternCtorArg index srcType pattern) =
    Instantiate.fromSrcType freeVarDict srcType
        |> IO.bind
            (\tipe ->
                let
                    expectation : E.PExpected Type
                    expectation =
                        E.PFromContext region (E.PCtorArg ctorName index) tipe
                in
                add pattern expectation state
            )
