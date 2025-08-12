module Builder.Deps.Solver exposing
    ( AppSolution(..)
    , Connection(..)
    , Details(..)
    , Env(..)
    , Solver
    , SolverResult(..)
    , State
    , addToApp
    , addToTestApp
    , envDecoder
    , envEncoder
    , initEnv
    , removeFromApp
    , verify
    )

import Builder.Deps.Registry as Registry
import Builder.Deps.Website as Website
import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Http as Http
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.Elm.Constraint as C
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Json.Decode as D
import Data.Map as Dict exposing (Dict)
import Task exposing (Task)
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Crash exposing (crash)
import Utils.Main as Utils
import Utils.Task.Extra as Task



-- SOLVER


type Solver a
    = Solver (State -> Task Never (InnerSolver a))


type InnerSolver a
    = ISOk State a
    | ISBack State
    | ISErr Exit.Solver


type State
    = State Stuff.PackageCache Connection Registry.Registry (Dict ( ( String, String ), ( Int, Int, Int ) ) ( Pkg.Name, V.Version ) Constraints)


type Constraints
    = Constraints C.Constraint (Dict ( String, String ) Pkg.Name C.Constraint)


type Connection
    = Online Http.Manager
    | Offline



-- RESULT


type SolverResult a
    = SolverOk a
    | NoSolution
    | NoOfflineSolution
    | SolverErr Exit.Solver



-- VERIFY -- used by Elm.Details


type Details
    = Details V.Version (Dict ( String, String ) Pkg.Name C.Constraint)


verify : Stuff.PackageCache -> Connection -> Registry.Registry -> Dict ( String, String ) Pkg.Name C.Constraint -> Task Never (SolverResult (Dict ( String, String ) Pkg.Name Details))
verify cache connection registry constraints =
    Stuff.withRegistryLock cache <|
        case try constraints of
            Solver solver ->
                solver (State cache connection registry Dict.empty)
                    |> Task.fmap
                        (\result ->
                            case result of
                                ISOk s a ->
                                    SolverOk (Dict.map (addDeps s) a)

                                ISBack _ ->
                                    noSolution connection

                                ISErr e ->
                                    SolverErr e
                        )


addDeps : State -> Pkg.Name -> V.Version -> Details
addDeps (State _ _ _ constraints) name vsn =
    case Dict.get (Tuple.mapSecond V.toComparable) ( name, vsn ) constraints of
        Just (Constraints _ deps) ->
            Details vsn deps

        Nothing ->
            crash "compiler bug manifesting in Deps.Solver.addDeps"


noSolution : Connection -> SolverResult a
noSolution connection =
    case connection of
        Online _ ->
            NoSolution

        Offline ->
            NoOfflineSolution



-- APP SOLUTION


type AppSolution
    = AppSolution (Dict ( String, String ) Pkg.Name V.Version) (Dict ( String, String ) Pkg.Name V.Version) Outline.AppOutline


getTransitive : Dict ( ( String, String ), ( Int, Int, Int ) ) ( Pkg.Name, V.Version ) Constraints -> Dict ( String, String ) Pkg.Name V.Version -> List ( Pkg.Name, V.Version ) -> Dict ( String, String ) Pkg.Name V.Version -> Dict ( String, String ) Pkg.Name V.Version
getTransitive constraints solution unvisited visited =
    case unvisited of
        [] ->
            visited

        (( pkg, vsn ) as info) :: infos ->
            if Dict.member identity pkg visited then
                getTransitive constraints solution infos visited

            else
                let
                    (Constraints _ newDeps) =
                        Utils.find (Tuple.mapSecond V.toComparable) info constraints

                    newUnvisited : List ( Pkg.Name, V.Version )
                    newUnvisited =
                        Dict.toList compare (Dict.intersection Pkg.compareName solution (Dict.diff newDeps visited))

                    newVisited : Dict ( String, String ) Pkg.Name V.Version
                    newVisited =
                        Dict.insert identity pkg vsn visited
                in
                getTransitive constraints solution infos <|
                    getTransitive constraints solution newUnvisited newVisited



-- ADD TO APP - used in Install


addToApp : Stuff.PackageCache -> Connection -> Registry.Registry -> Pkg.Name -> Outline.AppOutline -> Bool -> Task Never (SolverResult AppSolution)
addToApp cache connection registry pkg (Outline.AppOutline elm srcDirs direct indirect testDirect testIndirect) forTest =
    Stuff.withRegistryLock cache <|
        let
            allIndirects : Dict ( String, String ) Pkg.Name V.Version
            allIndirects =
                Dict.union indirect testIndirect

            allDirects : Dict ( String, String ) Pkg.Name V.Version
            allDirects =
                Dict.union direct testDirect

            allDeps : Dict ( String, String ) Pkg.Name V.Version
            allDeps =
                Dict.union allDirects allIndirects

            attempt : (a -> C.Constraint) -> Dict ( String, String ) Pkg.Name a -> Solver (Dict ( String, String ) Pkg.Name V.Version)
            attempt toConstraint deps =
                try (Dict.insert identity pkg C.anything (Dict.map (\_ -> toConstraint) deps))
        in
        case
            oneOf
                (attempt C.exactly allDeps)
                [ attempt C.exactly allDirects
                , attempt C.untilNextMinor allDirects
                , attempt C.untilNextMajor allDirects
                , attempt (\_ -> C.anything) allDirects
                ]
        of
            Solver solver ->
                solver (State cache connection registry Dict.empty)
                    |> Task.fmap
                        (\result ->
                            case result of
                                ISOk (State _ _ _ constraints) new ->
                                    let
                                        d : Dict ( String, String ) Pkg.Name V.Version
                                        d =
                                            if forTest then
                                                Dict.intersection Pkg.compareName new direct

                                            else
                                                Dict.intersection Pkg.compareName new (Dict.insert identity pkg V.one direct)

                                        i : Dict ( String, String ) Pkg.Name V.Version
                                        i =
                                            Dict.diff (getTransitive constraints new (Dict.toList compare d) Dict.empty) d

                                        td : Dict ( String, String ) Pkg.Name V.Version
                                        td =
                                            if forTest then
                                                Dict.intersection Pkg.compareName new (Dict.insert identity pkg V.one testDirect)

                                            else
                                                Dict.intersection Pkg.compareName new (Dict.remove identity pkg testDirect)

                                        ti : Dict ( String, String ) Pkg.Name V.Version
                                        ti =
                                            Dict.diff new (Utils.mapUnions [ d, i, td ])
                                    in
                                    SolverOk (AppSolution allDeps new (Outline.AppOutline elm srcDirs d i td ti))

                                ISBack _ ->
                                    noSolution connection

                                ISErr e ->
                                    SolverErr e
                        )



-- ADD TO APP - used in Test


addToTestApp : Stuff.PackageCache -> Connection -> Registry.Registry -> Pkg.Name -> C.Constraint -> Outline.AppOutline -> Task Never (SolverResult AppSolution)
addToTestApp cache connection registry pkg con (Outline.AppOutline elm srcDirs direct indirect testDirect testIndirect) =
    Stuff.withRegistryLock cache <|
        let
            allIndirects : Dict ( String, String ) Pkg.Name V.Version
            allIndirects =
                Dict.union indirect testIndirect

            allDirects : Dict ( String, String ) Pkg.Name V.Version
            allDirects =
                Dict.union direct testDirect

            allDeps : Dict ( String, String ) Pkg.Name V.Version
            allDeps =
                Dict.union allDirects allIndirects

            attempt : (a -> C.Constraint) -> Dict ( String, String ) Pkg.Name a -> Solver (Dict ( String, String ) Pkg.Name V.Version)
            attempt toConstraint deps =
                try (Dict.insert identity pkg con (Dict.map (\_ -> toConstraint) deps))
        in
        case
            oneOf
                (attempt C.exactly allDeps)
                [ attempt C.exactly allDirects
                , attempt C.untilNextMinor allDirects
                , attempt C.untilNextMajor allDirects
                , attempt (\_ -> C.anything) allDirects
                ]
        of
            Solver solver ->
                solver (State cache connection registry Dict.empty)
                    |> Task.fmap
                        (\result ->
                            case result of
                                ISOk (State _ _ _ constraints) new ->
                                    let
                                        d : Dict ( String, String ) Pkg.Name V.Version
                                        d =
                                            Dict.intersection Pkg.compareName new (Dict.insert identity pkg V.one direct)

                                        i : Dict ( String, String ) Pkg.Name V.Version
                                        i =
                                            Dict.diff (getTransitive constraints new (Dict.toList compare d) Dict.empty) d

                                        td : Dict ( String, String ) Pkg.Name V.Version
                                        td =
                                            Dict.intersection Pkg.compareName new (Dict.remove identity pkg testDirect)

                                        ti : Dict ( String, String ) Pkg.Name V.Version
                                        ti =
                                            Dict.diff new (Utils.mapUnions [ d, i, td ])
                                    in
                                    SolverOk (AppSolution allDeps new (Outline.AppOutline elm srcDirs d i td ti))

                                ISBack _ ->
                                    noSolution connection

                                ISErr e ->
                                    SolverErr e
                        )



-- REMOVE FROM APP - used in Uninstall


removeFromApp : Stuff.PackageCache -> Connection -> Registry.Registry -> Pkg.Name -> Outline.AppOutline -> Task Never (SolverResult AppSolution)
removeFromApp cache connection registry pkg (Outline.AppOutline elm srcDirs direct indirect testDirect testIndirect) =
    Stuff.withRegistryLock cache <|
        let
            allDirects : Dict ( String, String ) Pkg.Name V.Version
            allDirects =
                Dict.union direct testDirect
        in
        case try (Dict.map (\_ -> C.exactly) (Dict.remove identity pkg allDirects)) of
            Solver solver ->
                solver (State cache connection registry Dict.empty)
                    |> Task.fmap
                        (\result ->
                            case result of
                                ISOk (State _ _ _ constraints) new ->
                                    let
                                        allIndirects : Dict ( String, String ) Pkg.Name V.Version
                                        allIndirects =
                                            Dict.union indirect testIndirect

                                        allDeps : Dict ( String, String ) Pkg.Name V.Version
                                        allDeps =
                                            Dict.union allDirects allIndirects

                                        d : Dict ( String, String ) Pkg.Name V.Version
                                        d =
                                            Dict.remove identity pkg direct

                                        i : Dict ( String, String ) Pkg.Name V.Version
                                        i =
                                            Dict.diff (getTransitive constraints new (Dict.toList compare d) Dict.empty) d

                                        td : Dict ( String, String ) Pkg.Name V.Version
                                        td =
                                            Dict.remove identity pkg testDirect

                                        ti : Dict ( String, String ) Pkg.Name V.Version
                                        ti =
                                            Dict.diff new (Utils.mapUnions [ d, i, td ])
                                    in
                                    SolverOk (AppSolution allDeps new (Outline.AppOutline elm srcDirs d i td ti))

                                ISBack _ ->
                                    noSolution connection

                                ISErr e ->
                                    SolverErr e
                        )



-- TRY


try : Dict ( String, String ) Pkg.Name C.Constraint -> Solver (Dict ( String, String ) Pkg.Name V.Version)
try constraints =
    exploreGoals (Goals constraints Dict.empty)



-- EXPLORE GOALS


type Goals
    = Goals (Dict ( String, String ) Pkg.Name C.Constraint) (Dict ( String, String ) Pkg.Name V.Version)


exploreGoals : Goals -> Solver (Dict ( String, String ) Pkg.Name V.Version)
exploreGoals (Goals pending solved) =
    let
        compare : ( Pkg.Name, C.Constraint ) -> Pkg.Name
        compare =
            Tuple.first
    in
    case Utils.mapMinViewWithKey identity Basics.compare compare pending of
        Nothing ->
            pure solved

        Just ( ( name, constraint ), otherPending ) ->
            let
                goals1 : Goals
                goals1 =
                    Goals otherPending solved

                addVsn : V.Version -> Solver Goals
                addVsn =
                    addVersion goals1 name
            in
            getRelevantVersions name constraint
                |> bind (\( v, vs ) -> oneOf (addVsn v) (List.map addVsn vs))
                |> bind (\goals2 -> exploreGoals goals2)


addVersion : Goals -> Pkg.Name -> V.Version -> Solver Goals
addVersion (Goals pending solved) name version =
    getConstraints name version
        |> bind
            (\(Constraints elm deps) ->
                if C.goodElm elm then
                    foldM (addConstraint solved) pending (Dict.toList compare deps)
                        |> fmap
                            (\newPending ->
                                Goals newPending (Dict.insert identity name version solved)
                            )

                else
                    backtrack
            )


addConstraint : Dict ( String, String ) Pkg.Name V.Version -> Dict ( String, String ) Pkg.Name C.Constraint -> ( Pkg.Name, C.Constraint ) -> Solver (Dict ( String, String ) Pkg.Name C.Constraint)
addConstraint solved unsolved ( name, newConstraint ) =
    case Dict.get identity name solved of
        Just version ->
            if C.satisfies newConstraint version then
                pure unsolved

            else
                backtrack

        Nothing ->
            case Dict.get identity name unsolved of
                Nothing ->
                    pure (Dict.insert identity name newConstraint unsolved)

                Just oldConstraint ->
                    case C.intersect oldConstraint newConstraint of
                        Nothing ->
                            backtrack

                        Just mergedConstraint ->
                            if oldConstraint == mergedConstraint then
                                pure unsolved

                            else
                                pure (Dict.insert identity name mergedConstraint unsolved)



-- GET RELEVANT VERSIONS


getRelevantVersions : Pkg.Name -> C.Constraint -> Solver ( V.Version, List V.Version )
getRelevantVersions name constraint =
    Solver <|
        \((State _ _ registry _) as state) ->
            case Registry.getVersions name registry of
                Just (Registry.KnownVersions newest previous) ->
                    case List.filter (C.satisfies constraint) (newest :: previous) of
                        [] ->
                            Task.pure (ISBack state)

                        v :: vs ->
                            Task.pure (ISOk state ( v, vs ))

                Nothing ->
                    Task.pure (ISBack state)



-- GET CONSTRAINTS


getConstraints : Pkg.Name -> V.Version -> Solver Constraints
getConstraints pkg vsn =
    Solver <|
        \((State cache connection registry cDict) as state) ->
            let
                key : ( Pkg.Name, V.Version )
                key =
                    ( pkg, vsn )
            in
            case Dict.get (Tuple.mapSecond V.toComparable) key cDict of
                Just cs ->
                    Task.pure (ISOk state cs)

                Nothing ->
                    let
                        toNewState : Constraints -> State
                        toNewState cs =
                            State cache connection registry (Dict.insert (Tuple.mapSecond V.toComparable) key cs cDict)

                        home : String
                        home =
                            Stuff.package cache pkg vsn

                        path : String
                        path =
                            home ++ "/elm.json"
                    in
                    File.exists path
                        |> Task.bind
                            (\outlineExists ->
                                if outlineExists then
                                    File.readUtf8 path
                                        |> Task.bind
                                            (\bytes ->
                                                case D.fromByteString constraintsDecoder bytes of
                                                    Ok cs ->
                                                        case connection of
                                                            Online _ ->
                                                                Task.pure (ISOk (toNewState cs) cs)

                                                            Offline ->
                                                                Utils.dirDoesDirectoryExist (Stuff.package cache pkg vsn ++ "/src")
                                                                    |> Task.fmap
                                                                        (\srcExists ->
                                                                            if srcExists then
                                                                                ISOk (toNewState cs) cs

                                                                            else
                                                                                ISBack state
                                                                        )

                                                    Err _ ->
                                                        File.remove path
                                                            |> Task.fmap (\_ -> ISErr (Exit.SolverBadCacheData pkg vsn))
                                            )

                                else
                                    case connection of
                                        Offline ->
                                            Task.pure (ISBack state)

                                        Online manager ->
                                            Website.metadata pkg vsn "elm.json"
                                                |> Task.bind
                                                    (\url ->
                                                        Http.get manager url [] identity (Task.pure << Ok)
                                                            |> Task.bind
                                                                (\result ->
                                                                    case result of
                                                                        Err httpProblem ->
                                                                            Task.pure (ISErr (Exit.SolverBadHttp pkg vsn httpProblem))

                                                                        Ok body ->
                                                                            case D.fromByteString constraintsDecoder body of
                                                                                Ok cs ->
                                                                                    Utils.dirCreateDirectoryIfMissing True home
                                                                                        |> Task.bind (\_ -> File.writeUtf8 path body)
                                                                                        |> Task.fmap (\_ -> ISOk (toNewState cs) cs)

                                                                                Err _ ->
                                                                                    Task.pure (ISErr (Exit.SolverBadHttpData pkg vsn url))
                                                                )
                                                    )
                            )


constraintsDecoder : D.Decoder () Constraints
constraintsDecoder =
    D.mapError (\_ -> ()) Outline.decoder
        |> D.bind
            (\outline ->
                case outline of
                    Outline.Pkg (Outline.PkgOutline _ _ _ _ _ deps _ elmConstraint) ->
                        D.pure (Constraints elmConstraint deps)

                    Outline.App _ ->
                        D.failure ()
            )



-- ENVIRONMENT


type Env
    = Env Stuff.PackageCache Http.Manager Connection Registry.Registry


initEnv : Task Never (Result Exit.RegistryProblem Env)
initEnv =
    Utils.newEmptyMVar
        |> Task.bind
            (\mvar ->
                Utils.forkIO (Task.bind (Utils.putMVar Http.managerEncoder mvar) Http.getManager)
                    |> Task.bind
                        (\_ ->
                            Stuff.getPackageCache
                                |> Task.bind
                                    (\cache ->
                                        Stuff.withRegistryLock cache
                                            (Registry.read cache
                                                |> Task.bind
                                                    (\maybeRegistry ->
                                                        Utils.readMVar Http.managerDecoder mvar
                                                            |> Task.bind
                                                                (\manager ->
                                                                    case maybeRegistry of
                                                                        Nothing ->
                                                                            Registry.fetch manager cache
                                                                                |> Task.fmap
                                                                                    (\eitherRegistry ->
                                                                                        case eitherRegistry of
                                                                                            Ok latestRegistry ->
                                                                                                Ok <| Env cache manager (Online manager) latestRegistry

                                                                                            Err problem ->
                                                                                                Err problem
                                                                                    )

                                                                        Just cachedRegistry ->
                                                                            Registry.update manager cache cachedRegistry
                                                                                |> Task.fmap
                                                                                    (\eitherRegistry ->
                                                                                        case eitherRegistry of
                                                                                            Ok latestRegistry ->
                                                                                                Ok <| Env cache manager (Online manager) latestRegistry

                                                                                            Err _ ->
                                                                                                Ok <| Env cache manager Offline cachedRegistry
                                                                                    )
                                                                )
                                                    )
                                            )
                                    )
                        )
            )



-- INSTANCES


fmap : (a -> b) -> Solver a -> Solver b
fmap func (Solver solver) =
    Solver <|
        \state ->
            solver state
                |> Task.fmap
                    (\result ->
                        case result of
                            ISOk stateA arg ->
                                ISOk stateA (func arg)

                            ISBack stateA ->
                                ISBack stateA

                            ISErr e ->
                                ISErr e
                    )


pure : a -> Solver a
pure a =
    Solver (\state -> Task.pure (ISOk state a))


bind : (a -> Solver b) -> Solver a -> Solver b
bind callback (Solver solverA) =
    Solver <|
        \state ->
            solverA state
                |> Task.bind
                    (\resA ->
                        case resA of
                            ISOk stateA a ->
                                case callback a of
                                    Solver solverB ->
                                        solverB stateA

                            ISBack stateA ->
                                Task.pure (ISBack stateA)

                            ISErr e ->
                                Task.pure (ISErr e)
                    )


oneOf : Solver a -> List (Solver a) -> Solver a
oneOf ((Solver solverHead) as solver) solvers =
    case solvers of
        [] ->
            solver

        s :: ss ->
            Solver <|
                \state0 ->
                    solverHead state0
                        |> Task.bind
                            (\result ->
                                case result of
                                    ISOk stateA arg ->
                                        Task.pure (ISOk stateA arg)

                                    ISBack stateA ->
                                        let
                                            (Solver solverTail) =
                                                oneOf s ss
                                        in
                                        solverTail stateA

                                    ISErr e ->
                                        Task.pure (ISErr e)
                            )


backtrack : Solver a
backtrack =
    Solver <|
        \state ->
            Task.pure (ISBack state)


foldM : (b -> a -> Solver b) -> b -> List a -> Solver b
foldM f b =
    List.foldl (\a -> bind (\acc -> f acc a)) (pure b)



-- ENCODERS and DECODERS


envEncoder : Env -> BE.Encoder
envEncoder (Env cache manager connection registry) =
    BE.sequence
        [ Stuff.packageCacheEncoder cache
        , Http.managerEncoder manager
        , connectionEncoder connection
        , Registry.registryEncoder registry
        ]


envDecoder : BD.Decoder Env
envDecoder =
    BD.map4 Env
        Stuff.packageCacheDecoder
        Http.managerDecoder
        connectionDecoder
        Registry.registryDecoder


connectionEncoder : Connection -> BE.Encoder
connectionEncoder connection =
    case connection of
        Online manager ->
            BE.sequence
                [ BE.unsignedInt8 0
                , Http.managerEncoder manager
                ]

        Offline ->
            BE.unsignedInt8 1


connectionDecoder : BD.Decoder Connection
connectionDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map Online Http.managerDecoder

                    1 ->
                        BD.succeed Offline

                    _ ->
                        BD.fail
            )
