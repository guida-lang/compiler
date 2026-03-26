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
import Builder.File as File
import Builder.Guida.Outline as Outline
import Builder.Http as Http
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.Generate.Target as Target exposing (Target)
import Compiler.Guida.Constraint as C
import Compiler.Guida.Package as Pkg
import Compiler.Guida.Version as V
import Compiler.Json.Decode as D
import Control.Concurrent.MVar as MVar
import Data.Map as Dict exposing (Dict)
import Process
import System.Directory as Dir
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
    = GuidaConstraints C.Constraint (Dict ( String, String ) Pkg.Name C.Constraint)
    | ElmConstraints C.Constraint (Dict ( String, String ) Pkg.Name C.Constraint)


type Connection
    = Online Http.Manager
    | Offline



-- RESULT


type SolverResult a
    = SolverOk a
    | NoSolution
    | NoOfflineSolution
    | SolverErr Exit.Solver



-- VERIFY -- used by Guida.Details


type Details
    = Details V.Version (Dict ( String, String ) Pkg.Name C.Constraint)


verify : Target -> Stuff.PackageCache -> Connection -> Registry.Registry -> Dict ( String, String ) Pkg.Name C.Constraint -> Task Never (SolverResult (Dict ( String, String ) Pkg.Name Details))
verify target cache connection registry constraints =
    Stuff.withRegistryLock cache <|
        case try target constraints of
            Solver solver ->
                solver (State cache connection registry Dict.empty)
                    |> Task.map
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
        Just (GuidaConstraints _ deps) ->
            Details vsn deps

        Just (ElmConstraints _ deps) ->
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
                    newDeps : Dict ( String, String ) Pkg.Name C.Constraint
                    newDeps =
                        case Utils.find (Tuple.mapSecond V.toComparable) info constraints of
                            GuidaConstraints _ deps ->
                                deps

                            ElmConstraints _ deps ->
                                deps

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
addToApp cache connection registry pkg outline forTest =
    case outline of
        Outline.GuidaAppOutline guida srcDirs direct indirect testDirect testIndirect ->
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
                        try Target.GuidaTarget (Dict.insert identity pkg C.anything (Dict.map (\_ -> toConstraint) deps))
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
                            |> Task.map
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
                                            SolverOk (AppSolution allDeps new (Outline.GuidaAppOutline guida srcDirs d i td ti))

                                        ISBack _ ->
                                            noSolution connection

                                        ISErr e ->
                                            SolverErr e
                                )

        Outline.ElmAppOutline elm srcDirs direct indirect testDirect testIndirect ->
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
                        try Target.ElmTarget (Dict.insert identity pkg C.anything (Dict.map (\_ -> toConstraint) deps))
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
                            |> Task.map
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
                                            SolverOk (AppSolution allDeps new (Outline.ElmAppOutline elm srcDirs d i td ti))

                                        ISBack _ ->
                                            noSolution connection

                                        ISErr e ->
                                            SolverErr e
                                )



-- ADD TO APP - used in Test


addToTestApp : Stuff.PackageCache -> Connection -> Registry.Registry -> Pkg.Name -> C.Constraint -> Outline.AppOutline -> Task Never (SolverResult AppSolution)
addToTestApp cache connection registry pkg con outline =
    case outline of
        Outline.GuidaAppOutline guida srcDirs direct indirect testDirect testIndirect ->
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
                        try Target.GuidaTarget (Dict.insert identity pkg con (Dict.map (\_ -> toConstraint) deps))
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
                            |> Task.map
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
                                            SolverOk (AppSolution allDeps new (Outline.GuidaAppOutline guida srcDirs d i td ti))

                                        ISBack _ ->
                                            noSolution connection

                                        ISErr e ->
                                            SolverErr e
                                )

        Outline.ElmAppOutline elm srcDirs direct indirect testDirect testIndirect ->
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
                        try Target.ElmTarget (Dict.insert identity pkg con (Dict.map (\_ -> toConstraint) deps))
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
                            |> Task.map
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
                                            SolverOk (AppSolution allDeps new (Outline.ElmAppOutline elm srcDirs d i td ti))

                                        ISBack _ ->
                                            noSolution connection

                                        ISErr e ->
                                            SolverErr e
                                )



-- REMOVE FROM APP - used in Uninstall


removeFromApp : Stuff.PackageCache -> Connection -> Registry.Registry -> Pkg.Name -> Outline.AppOutline -> Task Never (SolverResult AppSolution)
removeFromApp cache connection registry pkg outline =
    case outline of
        Outline.GuidaAppOutline guida srcDirs direct indirect testDirect testIndirect ->
            Stuff.withRegistryLock cache <|
                let
                    allDirects : Dict ( String, String ) Pkg.Name V.Version
                    allDirects =
                        Dict.union direct testDirect
                in
                case try Target.GuidaTarget (Dict.map (\_ -> C.exactly) (Dict.remove identity pkg allDirects)) of
                    Solver solver ->
                        solver (State cache connection registry Dict.empty)
                            |> Task.map
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
                                            SolverOk (AppSolution allDeps new (Outline.GuidaAppOutline guida srcDirs d i td ti))

                                        ISBack _ ->
                                            noSolution connection

                                        ISErr e ->
                                            SolverErr e
                                )

        Outline.ElmAppOutline elm srcDirs direct indirect testDirect testIndirect ->
            Stuff.withRegistryLock cache <|
                let
                    allDirects : Dict ( String, String ) Pkg.Name V.Version
                    allDirects =
                        Dict.union direct testDirect
                in
                case try Target.ElmTarget (Dict.map (\_ -> C.exactly) (Dict.remove identity pkg allDirects)) of
                    Solver solver ->
                        solver (State cache connection registry Dict.empty)
                            |> Task.map
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
                                            SolverOk (AppSolution allDeps new (Outline.ElmAppOutline elm srcDirs d i td ti))

                                        ISBack _ ->
                                            noSolution connection

                                        ISErr e ->
                                            SolverErr e
                                )



-- TRY


try : Target -> Dict ( String, String ) Pkg.Name C.Constraint -> Solver (Dict ( String, String ) Pkg.Name V.Version)
try target constraints =
    exploreGoals target (Goals constraints Dict.empty)



-- EXPLORE GOALS


type Goals
    = Goals (Dict ( String, String ) Pkg.Name C.Constraint) (Dict ( String, String ) Pkg.Name V.Version)


exploreGoals : Target -> Goals -> Solver (Dict ( String, String ) Pkg.Name V.Version)
exploreGoals target (Goals pending solved) =
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
                    addVersion target goals1 name
            in
            getRelevantVersions name constraint
                |> bind (\( v, vs ) -> oneOf (addVsn v) (List.map addVsn vs))
                |> bind (\goals2 -> exploreGoals target goals2)


addVersion : Target -> Goals -> Pkg.Name -> V.Version -> Solver Goals
addVersion target (Goals pending solved) name version =
    getConstraints name version
        |> bind
            (\constraints ->
                case constraints of
                    GuidaConstraints guida deps ->
                        if C.goodGuida guida then
                            foldM (addConstraint solved) pending (Dict.toList compare deps)
                                |> fmap
                                    (\newPending ->
                                        Goals newPending (Dict.insert identity name version solved)
                                    )

                        else
                            backtrack

                    ElmConstraints elm deps ->
                        if C.goodElm elm then
                            foldM (addConstraint solved) pending (Dict.toList compare (Pkg.sanitizeElmDeps target deps))
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
            case Registry.getVersions Registry.KeepAllVersions name registry of
                Just (Registry.KnownVersions ( _, newest ) previous) ->
                    case List.filter (C.satisfies constraint) (newest :: List.map Tuple.second previous) of
                        [] ->
                            Task.succeed (ISBack state)

                        v :: vs ->
                            Task.succeed (ISOk state ( v, vs ))

                Nothing ->
                    Task.succeed (ISBack state)



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
                    Task.succeed (ISOk state cs)

                Nothing ->
                    let
                        toNewState : Constraints -> State
                        toNewState cs =
                            State cache connection registry (Dict.insert (Tuple.mapSecond V.toComparable) key cs cDict)

                        home : String
                        home =
                            Stuff.package cache pkg vsn

                        guidaPath : String
                        guidaPath =
                            home ++ "/guida.json"
                    in
                    File.exists guidaPath
                        |> Task.andThen
                            (\guidaOutlineExists ->
                                if guidaOutlineExists then
                                    File.readUtf8 guidaPath
                                        |> Task.andThen
                                            (\bytes ->
                                                case D.fromByteString constraintsDecoder bytes of
                                                    Ok cs ->
                                                        case connection of
                                                            Online _ ->
                                                                Task.succeed (ISOk (toNewState cs) cs)

                                                            Offline ->
                                                                Dir.doesDirectoryExist (Stuff.package cache pkg vsn ++ "/src")
                                                                    |> Task.map
                                                                        (\srcExists ->
                                                                            if srcExists then
                                                                                ISOk (toNewState cs) cs

                                                                            else
                                                                                ISBack state
                                                                        )

                                                    Err _ ->
                                                        File.remove guidaPath
                                                            |> Task.map (\_ -> ISErr (Exit.SolverBadCacheGuidaData pkg vsn))
                                            )

                                else
                                    let
                                        elmPath : String
                                        elmPath =
                                            home ++ "/elm.json"
                                    in
                                    File.exists guidaPath
                                        |> Task.andThen
                                            (\elmOutlineExists ->
                                                if elmOutlineExists then
                                                    File.readUtf8 elmPath
                                                        |> Task.andThen
                                                            (\bytes ->
                                                                case D.fromByteString constraintsDecoder bytes of
                                                                    Ok cs ->
                                                                        case connection of
                                                                            Online _ ->
                                                                                Task.succeed (ISOk (toNewState cs) cs)

                                                                            Offline ->
                                                                                Dir.doesDirectoryExist (Stuff.package cache pkg vsn ++ "/src")
                                                                                    |> Task.map
                                                                                        (\srcExists ->
                                                                                            if srcExists then
                                                                                                ISOk (toNewState cs) cs

                                                                                            else
                                                                                                ISBack state
                                                                                        )

                                                                    Err _ ->
                                                                        File.remove elmPath
                                                                            |> Task.map (\_ -> ISErr (Exit.SolverBadCacheElmData pkg vsn))
                                                            )

                                                else
                                                    case connection of
                                                        Offline ->
                                                            Task.succeed (ISBack state)

                                                        Online manager ->
                                                            Website.metadata pkg vsn "guida.json"
                                                                |> Task.andThen
                                                                    (\guidaUrl ->
                                                                        Http.get manager guidaUrl [] identity (Task.succeed << Ok)
                                                                            |> Task.andThen
                                                                                (\guidaResult ->
                                                                                    case guidaResult of
                                                                                        Err guidaHttpProblem ->
                                                                                            Task.succeed (ISErr (Exit.SolverBadHttp pkg vsn guidaHttpProblem))

                                                                                        Ok guidaBody ->
                                                                                            case D.fromByteString constraintsDecoder guidaBody of
                                                                                                Ok cs ->
                                                                                                    Dir.createDirectoryIfMissing True home
                                                                                                        |> Task.andThen (\_ -> File.writeUtf8 guidaPath guidaBody)
                                                                                                        |> Task.map (\_ -> ISOk (toNewState cs) cs)

                                                                                                Err _ ->
                                                                                                    Website.metadata pkg vsn "elm.json"
                                                                                                        |> Task.andThen
                                                                                                            (\elmUrl ->
                                                                                                                Http.get manager elmUrl [] identity (Task.succeed << Ok)
                                                                                                                    |> Task.andThen
                                                                                                                        (\elmResult ->
                                                                                                                            case elmResult of
                                                                                                                                Err elmHttpProblem ->
                                                                                                                                    Task.succeed (ISErr (Exit.SolverBadHttp pkg vsn elmHttpProblem))

                                                                                                                                Ok elmBody ->
                                                                                                                                    case D.fromByteString constraintsDecoder elmBody of
                                                                                                                                        Ok cs ->
                                                                                                                                            Dir.createDirectoryIfMissing True home
                                                                                                                                                |> Task.andThen (\_ -> File.writeUtf8 elmPath elmBody)
                                                                                                                                                |> Task.map (\_ -> ISOk (toNewState cs) cs)

                                                                                                                                        Err _ ->
                                                                                                                                            Task.succeed (ISErr (Exit.SolverBadHttpElmData pkg vsn elmUrl))
                                                                                                                        )
                                                                                                            )
                                                                                )
                                                                    )
                                            )
                            )


constraintsDecoder : D.Decoder () Constraints
constraintsDecoder =
    D.oneOf
        [ Outline.guidaDecoder
        , Outline.elmDecoder
        ]
        |> D.mapError (\_ -> ())
        |> D.bind
            (\outline ->
                case outline of
                    Outline.Pkg (Outline.GuidaPkgOutline _ _ _ _ _ deps _ guidaConstraint) ->
                        D.pure (GuidaConstraints guidaConstraint deps)

                    Outline.Pkg (Outline.ElmPkgOutline _ _ _ _ _ deps _ elmConstraint) ->
                        D.pure (ElmConstraints elmConstraint deps)

                    Outline.App _ ->
                        D.failure ()
            )



-- ENVIRONMENT


type Env
    = Env Stuff.PackageCache Http.Manager Connection Registry.Registry


initEnv : Task Never (Result Exit.RegistryProblem Env)
initEnv =
    MVar.newEmptyMVar
        |> Task.andThen
            (\mvar ->
                Process.spawn (Task.andThen (MVar.putMVar mvar) Http.getManager)
                    |> Task.andThen
                        (\_ ->
                            Stuff.getPackageCache
                                |> Task.andThen
                                    (\cache ->
                                        Stuff.withRegistryLock cache
                                            (Registry.read cache
                                                |> Task.andThen
                                                    (\maybeRegistry ->
                                                        MVar.readMVar mvar
                                                            |> Task.andThen
                                                                (\manager ->
                                                                    case maybeRegistry of
                                                                        Nothing ->
                                                                            Registry.fetch manager cache
                                                                                |> Task.map
                                                                                    (\eitherRegistry ->
                                                                                        case eitherRegistry of
                                                                                            Ok latestRegistry ->
                                                                                                Ok <| Env cache manager (Online manager) latestRegistry

                                                                                            Err problem ->
                                                                                                Err problem
                                                                                    )

                                                                        Just cachedRegistry ->
                                                                            Registry.update manager cache cachedRegistry
                                                                                |> Task.map
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
                |> Task.map
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
    Solver (\state -> Task.succeed (ISOk state a))


bind : (a -> Solver b) -> Solver a -> Solver b
bind callback (Solver solverA) =
    Solver <|
        \state ->
            solverA state
                |> Task.andThen
                    (\resA ->
                        case resA of
                            ISOk stateA a ->
                                case callback a of
                                    Solver solverB ->
                                        solverB stateA

                            ISBack stateA ->
                                Task.succeed (ISBack stateA)

                            ISErr e ->
                                Task.succeed (ISErr e)
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
                        |> Task.andThen
                            (\result ->
                                case result of
                                    ISOk stateA arg ->
                                        Task.succeed (ISOk stateA arg)

                                    ISBack stateA ->
                                        let
                                            (Solver solverTail) =
                                                oneOf s ss
                                        in
                                        solverTail stateA

                                    ISErr e ->
                                        Task.succeed (ISErr e)
                            )


backtrack : Solver a
backtrack =
    Solver <|
        \state ->
            Task.succeed (ISBack state)


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
