module Builder.Guida.Details exposing
    ( BuildID
    , Details(..)
    , Extras
    , Foreign(..)
    , Interfaces
    , Local(..)
    , Status
    , ValidOutline(..)
    , detailsEncoder
    , load
    , loadInterfaces
    , loadObjects
    , localDecoder
    , localEncoder
    , verifyInstall
    )

{-| Manage Guida project details, cached build artifacts, and dependency verification.

This module tracks build metadata for source files, resolves package and app
dependencies, loads cached interfaces and object graphs, and verifies project
install state for the Guida build tool.

-}

import Builder.BackgroundWriter as BW
import Builder.Deps.Registry as Registry
import Builder.Deps.Solver as Solver
import Builder.Deps.Website as Website
import Builder.File as File
import Builder.Guida.Outline as Outline
import Builder.Http as Http
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Source as Src
import Compiler.Compile as Compile
import Compiler.Data.Name as Name
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Generate.Target as Target exposing (Target)
import Compiler.Guida.Constraint as Con
import Compiler.Guida.Docs as Docs
import Compiler.Guida.Interface as I
import Compiler.Guida.Kernel as Kernel
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Guida.Package as Pkg
import Compiler.Guida.Version as V
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Module as Parse
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import System.TypeCheck.IO as TypeCheck
import Task exposing (Task)
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Crash exposing (crash)
import Utils.Main as Utils exposing (FilePath, MVar)
import Utils.Task.Extra as Task



-- DETAILS


{-| Build metadata for the current project, including source outline, dependency
state, local and foreign package references, and cached artifacts.
-}
type Details
    = Details File.Time ValidOutline BuildID (Dict String ModuleName.Raw Local) (Dict String ModuleName.Raw Foreign) Extras


{-| A monotonic integer used to track interface and compile change versions.
-}
type alias BuildID =
    Int


{-| A validated project outline that distinguishes between app and package
builds.
-}
type ValidOutline
    = ValidApp (NE.Nonempty Outline.SrcDir)
    | ValidPkg Pkg.Name (List ModuleName.Raw) (Dict ( String, String ) Pkg.Name V.Version {- for docs in reactor -})



-- NOTE: we need two ways to detect if a file must be recompiled:
--
-- (1) _time is the modification time from the last time we compiled the file.
-- By checking EQUALITY with the current modification time, we can detect file
-- saves and `git checkout` of previous versions. Both need a recompile.
--
-- (2) _lastChange is the BuildID from the last time a new interface file was
-- generated, and _lastCompile is the BuildID from the last time the file was
-- compiled. These may be different if a file is recompiled but the interface
-- stayed the same. When the _lastCompile is LESS THAN the _lastChange of any
-- imports, we need to recompile. This can happen when a project has multiple
-- entrypoints and some modules are compiled less often than their imports.
--


{-| Metadata about a local source module in the current project.

`Local` tracks the file path, last modified time, imports, main entrypoint
status, and build/change identifiers.

-}
type Local
    = Local FilePath File.Time (List ModuleName.Raw) Bool BuildID BuildID


{-| A foreign dependency reference used by this project.

The first package is the dependency itself and the second list contains its
public transitive dependencies.

-}
type Foreign
    = Foreign Pkg.Name (List Pkg.Name)


{-| Cached artifact state for a build.

`ArtifactsCached` means existing cached artifacts should be reused.
`ArtifactsFresh` means fresh interfaces and the object graph are available.

-}
type Extras
    = ArtifactsCached
    | ArtifactsFresh Interfaces Opt.GlobalGraph


{-| Cached dependency interfaces for the current project.
-}
type alias Interfaces =
    Dict (List String) TypeCheck.Canonical I.DependencyInterface



-- LOAD ARTIFACTS


{-| Load cached compiled objects for the given project root.

If the details already represent fresh artifacts, the object graph is returned
immediately; otherwise it is read from disk.

-}
loadObjects : FilePath -> Details -> Task Never (MVar (Maybe Opt.GlobalGraph))
loadObjects root (Details _ _ _ _ _ extras) =
    case extras of
        ArtifactsFresh _ o ->
            Utils.newMVar (Utils.maybeEncoder Opt.globalGraphEncoder) (Just o)

        ArtifactsCached ->
            fork (Utils.maybeEncoder Opt.globalGraphEncoder) (File.readBinary Opt.globalGraphDecoder (Stuff.objects root))


{-| Load cached dependency interfaces for the given project root.

Returns a mutable container with the interface map when the artifacts are fresh
or otherwise reads the serialized interfaces from disk.

-}
loadInterfaces : FilePath -> Details -> Task Never (MVar (Maybe Interfaces))
loadInterfaces root (Details _ _ _ _ _ extras) =
    case extras of
        ArtifactsFresh i _ ->
            Utils.newMVar (Utils.maybeEncoder interfacesEncoder) (Just i)

        ArtifactsCached ->
            fork (Utils.maybeEncoder interfacesEncoder) (File.readBinary interfacesDecoder (Stuff.interfaces root))



-- VERIFY INSTALL -- used by Install


{-| Verify that a project can be installed by validating its dependency graph,
outline, and current project state.
-}
verifyInstall : BW.Scope -> Stuff.Root -> Solver.Env -> Outline.Outline -> Task Never (Result Exit.Details ())
verifyInstall scope root (Solver.Env cache manager connection registry) outline =
    File.getTime (Stuff.rootProjectFilePath root)
        |> Task.andThen
            (\time ->
                let
                    key : Reporting.Key msg
                    key =
                        Reporting.ignorer

                    env : Env
                    env =
                        Env key scope root cache manager connection registry
                in
                case outline of
                    Outline.Pkg pkg ->
                        Task.run (Task.map (\_ -> ()) (verifyPkg env time pkg))

                    Outline.App app ->
                        Task.run (Task.map (\_ -> ()) (verifyApp env time app))
            )



-- LOAD -- used by Make, Repl, Reactor, Test


{-| Load build details for a project by checking cached artifacts and computing
whether the project has fresh interfaces or needs recompilation.
-}
load : Reporting.Style -> BW.Scope -> Stuff.Root -> Task Never (Result Exit.Details Details)
load style scope root =
    File.getTime (Stuff.rootProjectFilePath root)
        |> Task.andThen
            (\newTime ->
                File.readBinary detailsDecoder (Stuff.details (Stuff.rootPath root))
                    |> Task.andThen
                        (\maybeDetails ->
                            case maybeDetails of
                                Nothing ->
                                    generate style scope root newTime

                                Just (Details oldTime outline buildID locals foreigns extras) ->
                                    if oldTime == newTime then
                                        Task.succeed (Ok (Details oldTime outline (buildID + 1) locals foreigns extras))

                                    else
                                        generate style scope root newTime
                        )
            )



-- GENERATE


{-| Generate fresh build details for a project by preparing the verification
environment, converting the outline, and validating dependencies.
-}
generate : Reporting.Style -> BW.Scope -> Stuff.Root -> File.Time -> Task Never (Result Exit.Details Details)
generate style scope root time =
    Reporting.trackDetails style
        (\key ->
            initEnv key scope root
                |> Task.andThen
                    (\result ->
                        case result of
                            Err exit ->
                                Task.succeed (Err exit)

                            Ok ( env, outline ) ->
                                convertToGuidaOutline env outline
                                    |> Task.andThen
                                        (\convertedOutline ->
                                            case convertedOutline of
                                                Outline.Pkg pkg ->
                                                    verifyPkg env time pkg

                                                Outline.App app ->
                                                    verifyApp env time app
                                        )
                                    |> Task.run
                    )
        )


{-| Convert a raw outline into a validated Guida outline.

For `GuidaRoot` package outlines this resolves the standard library constraints
against the registry before verification proceeds.

-}
convertToGuidaOutline : Env -> Outline.Outline -> Task Exit.Details Outline.Outline
convertToGuidaOutline (Env _ _ root cache _ connection registry) outline =
    case ( root, outline ) of
        ( Stuff.GuidaRoot _, Outline.Pkg (Outline.ElmPkgOutline name summary license version exposed deps test elmVersion) ) ->
            case Registry.getVersions_ Registry.KeepAllVersions Pkg.stdlib registry of
                Err _ ->
                    Task.io Website.domain
                        |> Task.andThen
                            (\registryDomain ->
                                case connection of
                                    Solver.Online _ ->
                                        Task.fail (Exit.DetailsUnknownStdlibOnline registryDomain)

                                    Solver.Offline ->
                                        Task.fail (Exit.DetailsUnknownStdlibOffline registryDomain)
                            )

                Ok (Registry.KnownVersions _ _) ->
                    let
                        cons : Dict ( String, String ) Pkg.Name Con.Constraint
                        cons =
                            Dict.insert identity Pkg.stdlib Con.anything deps
                    in
                    Task.io (Solver.verify (Stuff.rootToTarget root) cache connection registry cons)
                        |> Task.andThen
                            (\result ->
                                case result of
                                    Solver.SolverOk solution ->
                                        let
                                            (Solver.Details vsn _) =
                                                Utils.find identity Pkg.stdlib solution

                                            con : Con.Constraint
                                            con =
                                                Con.untilNextMajor vsn
                                        in
                                        Task.succeed <|
                                            Outline.Pkg <|
                                                Outline.ElmPkgOutline name
                                                    summary
                                                    license
                                                    version
                                                    exposed
                                                    (Dict.filter (\( author, _ ) _ -> author /= Pkg.elm && author /= Pkg.elmExplorations) deps
                                                        |> Dict.insert identity Pkg.stdlib con
                                                    )
                                                    test
                                                    elmVersion

                                    Solver.NoSolution ->
                                        Task.fail (Exit.DetailsNoOnlinePkgSolution Pkg.stdlib)

                                    Solver.NoOfflineSolution ->
                                        Task.io Website.domain
                                            |> Task.andThen
                                                (\registryDomain ->
                                                    Task.fail (Exit.DetailsNoOfflinePkgSolution registryDomain Pkg.stdlib)
                                                )

                                    Solver.SolverErr exit ->
                                        Task.fail (Exit.DetailsSolverProblem exit)
                            )

        _ ->
            Task.succeed outline



-- ENV


{-| Runtime environment needed to verify project outlines and resolve package
dependencies.

It carries reporting state, the current project root, package cache, HTTP
manager, solver connection mode, and registry client.

-}
type Env
    = Env Reporting.DKey BW.Scope Stuff.Root Stuff.PackageCache Http.Manager Solver.Connection Registry.Registry


{-| Initialize the verification environment by reading the project outline and
setting up registry state.
-}
initEnv : Reporting.DKey -> BW.Scope -> Stuff.Root -> Task Never (Result Exit.Details ( Env, Outline.Outline ))
initEnv key scope root =
    fork resultRegistryProblemEnvEncoder Solver.initEnv
        |> Task.andThen
            (\mvar ->
                Outline.read root
                    |> Task.andThen
                        (\eitherOutline ->
                            case eitherOutline of
                                Err problem ->
                                    Task.succeed (Err (Exit.DetailsBadOutline problem))

                                Ok outline ->
                                    Utils.readMVar resultRegistryProblemEnvDecoder mvar
                                        |> Task.map
                                            (\maybeEnv ->
                                                case maybeEnv of
                                                    Err problem ->
                                                        Err (Exit.DetailsCannotGetRegistry problem)

                                                    Ok (Solver.Env cache manager connection registry) ->
                                                        Ok ( Env key scope root cache manager connection registry, outline )
                                            )
                        )
            )



-- VERIFY PROJECT


{-| Verify a package outline by checking package constraints, duplicate
dependency declarations, and package dependency resolution.
-}
verifyPkg : Env -> File.Time -> Outline.PkgOutline -> Task Exit.Details Details
verifyPkg env time outline =
    case outline of
        Outline.GuidaPkgOutline pkg _ _ _ exposed direct testDirect guida ->
            if Con.goodGuida guida then
                union identity Pkg.compareName noGuidaDups direct testDirect
                    |> Task.andThen (verifyConstraints env)
                    |> Task.andThen
                        (\solution ->
                            let
                                exposedList : List ModuleName.Raw
                                exposedList =
                                    Outline.flattenExposed exposed

                                exactDeps : Dict ( String, String ) Pkg.Name V.Version
                                exactDeps =
                                    -- for pkg docs in reactor
                                    Dict.map (\_ (Solver.Details v _) -> v) solution
                            in
                            verifyDependencies env time (ValidPkg pkg exposedList exactDeps) solution direct
                        )

            else
                Task.fail (Exit.DetailsBadGuidaInPkg guida)

        Outline.ElmPkgOutline pkg _ _ _ exposed direct testDirect elm ->
            if Con.goodElm elm then
                union identity Pkg.compareName noElmDups direct testDirect
                    |> Task.andThen (verifyConstraints env)
                    |> Task.andThen
                        (\solution ->
                            let
                                exposedList : List ModuleName.Raw
                                exposedList =
                                    Outline.flattenExposed exposed

                                exactDeps : Dict ( String, String ) Pkg.Name V.Version
                                exactDeps =
                                    -- for pkg docs in reactor
                                    Dict.map (\_ (Solver.Details v _) -> v) solution
                            in
                            verifyDependencies env time (ValidPkg pkg exposedList exactDeps) solution direct
                        )

            else
                Task.fail (Exit.DetailsBadElmInPkg elm)


{-| Verify an application outline by checking application dependency constraints
and dependency consistency for Guida or Elm apps.
-}
verifyApp : Env -> File.Time -> Outline.AppOutline -> Task Exit.Details Details
verifyApp env time outline =
    case outline of
        Outline.GuidaAppOutline guidaVersion srcDirs direct _ _ _ ->
            if guidaVersion == V.compiler then
                checkAppDeps outline
                    |> Task.andThen
                        (\stated ->
                            verifyConstraints env (Dict.map (\_ -> Con.exactly) stated)
                                |> Task.andThen
                                    (\actual ->
                                        if Dict.size stated == Dict.size actual then
                                            verifyDependencies env time (ValidApp srcDirs) actual direct

                                        else
                                            Task.fail Exit.DetailsHandEditedGuidaDependencies
                                    )
                        )

            else
                Task.fail (Exit.DetailsBadGuidaInAppOutline guidaVersion)

        Outline.ElmAppOutline elmVersion srcDirs direct _ _ _ ->
            if elmVersion == V.elmCompiler then
                checkAppDeps outline
                    |> Task.andThen
                        (\stated ->
                            verifyConstraints env (Dict.map (\_ -> Con.exactly) stated)
                                |> Task.andThen
                                    (\actual ->
                                        if Dict.size stated == Dict.size actual then
                                            verifyDependencies env time (ValidApp srcDirs) actual direct

                                        else
                                            Task.fail Exit.DetailsHandEditedElmDependencies
                                    )
                        )

            else
                Task.fail (Exit.DetailsBadElmInAppOutline elmVersion)


{-| Compute the exact app dependencies for the application outline, ensuring
that duplicate dependencies are either allowed or rejected appropriately.
-}
checkAppDeps : Outline.AppOutline -> Task Exit.Details (Dict ( String, String ) Pkg.Name V.Version)
checkAppDeps outline =
    case outline of
        Outline.GuidaAppOutline _ _ direct indirect testDirect testIndirect ->
            union identity Pkg.compareName allowEqualGuidaDups indirect testDirect
                |> Task.andThen
                    (\x ->
                        union identity Pkg.compareName noGuidaDups direct testIndirect
                            |> Task.andThen (\y -> union identity Pkg.compareName noGuidaDups x y)
                    )

        Outline.ElmAppOutline _ _ direct indirect testDirect testIndirect ->
            union identity Pkg.compareName allowEqualElmDups indirect testDirect
                |> Task.andThen
                    (\x ->
                        union identity Pkg.compareName noElmDups direct testIndirect
                            |> Task.andThen (\y -> union identity Pkg.compareName noElmDups x y)
                    )



-- VERIFY CONSTRAINTS


{-| Verify a set of package constraints against the package registry and solver.
-}
verifyConstraints : Env -> Dict ( String, String ) Pkg.Name Con.Constraint -> Task Exit.Details (Dict ( String, String ) Pkg.Name Solver.Details)
verifyConstraints (Env _ _ root cache _ connection registry) constraints =
    Task.io (Solver.verify (Stuff.rootToTarget root) cache connection registry constraints)
        |> Task.andThen
            (\result ->
                case result of
                    Solver.SolverOk details ->
                        Task.succeed details

                    Solver.NoSolution ->
                        Task.fail
                            (case root of
                                Stuff.GuidaRoot _ ->
                                    Exit.DetailsNoGuidaSolution

                                Stuff.ElmRoot _ _ ->
                                    Exit.DetailsNoElmSolution
                            )

                    Solver.NoOfflineSolution ->
                        Task.io Website.domain
                            |> Task.andThen
                                (\registryDomain ->
                                    Task.fail
                                        (case root of
                                            Stuff.GuidaRoot _ ->
                                                Exit.DetailsNoGuidaOfflineSolution registryDomain

                                            Stuff.ElmRoot _ _ ->
                                                Exit.DetailsNoElmOfflineSolution registryDomain
                                        )
                                )

                    Solver.SolverErr exit ->
                        Task.fail (Exit.DetailsSolverProblem exit)
            )



-- UNION


{-| Merge two dependency dictionaries while resolving conflicts using a
custom tie breaker.
-}
union : (k -> comparable) -> (k -> k -> Order) -> (k -> v -> v -> Task Exit.Details v) -> Dict comparable k v -> Dict comparable k v -> Task Exit.Details (Dict comparable k v)
union toComparable keyComparison tieBreaker deps1 deps2 =
    Dict.merge keyComparison
        (\k dep -> Task.map (Dict.insert toComparable k dep))
        (\k dep1 dep2 acc ->
            tieBreaker k dep1 dep2
                |> Task.andThen (\v -> Task.map (Dict.insert toComparable k v) acc)
        )
        (\k dep -> Task.map (Dict.insert toComparable k dep))
        deps1
        deps2
        (Task.succeed Dict.empty)


{-| Reject duplicate Guida dependencies as an error.
-}
noGuidaDups : k -> v -> v -> Task Exit.Details v
noGuidaDups _ _ _ =
    Task.fail Exit.DetailsHandEditedGuidaDependencies


{-| Reject duplicate Elm dependencies as an error.
-}
noElmDups : k -> v -> v -> Task Exit.Details v
noElmDups _ _ _ =
    Task.fail Exit.DetailsHandEditedElmDependencies


{-| Allow duplicate Guida dependencies only when they are identical.
-}
allowEqualGuidaDups : k -> v -> v -> Task Exit.Details v
allowEqualGuidaDups _ v1 v2 =
    if v1 == v2 then
        Task.succeed v1

    else
        Task.fail Exit.DetailsHandEditedGuidaDependencies


{-| Allow duplicate Elm dependencies only when they are identical.
-}
allowEqualElmDups : k -> v -> v -> Task Exit.Details v
allowEqualElmDups _ v1 v2 =
    if v1 == v2 then
        Task.succeed v1

    else
        Task.fail Exit.DetailsHandEditedElmDependencies



-- FORK


{-| Run a task in the background and store its result in an `MVar`.

This is used to execute long-running dependency checks concurrently while
still preserving deterministic result collection.

-}
fork : (a -> BE.Encoder) -> Task Never a -> Task Never (MVar a)
fork encoder work =
    Utils.newEmptyMVar
        |> Task.andThen
            (\mvar ->
                Utils.forkIO (Task.andThen (Utils.putMVar encoder mvar) work)
                    |> Task.map (\_ -> mvar)
            )



-- VERIFY DEPENDENCIES


{-| Verify package dependencies by building or loading artifacts for each direct
dependency and assembling the final project details.
-}
verifyDependencies : Env -> File.Time -> ValidOutline -> Dict ( String, String ) Pkg.Name Solver.Details -> Dict ( String, String ) Pkg.Name a -> Task Exit.Details Details
verifyDependencies ((Env key scope root cache _ _ _) as env) time outline solution directDeps =
    Task.eio identity
        (Reporting.report key (Reporting.DStart (Dict.size solution))
            |> Task.andThen (\_ -> Utils.newEmptyMVar)
            |> Task.andThen
                (\mvar ->
                    Stuff.withRegistryLock cache
                        (Utils.mapTraverseWithKey identity Pkg.compareName (\k v -> fork depEncoder (verifyDep env mvar solution k v)) solution)
                        |> Task.andThen
                            (\mvars ->
                                Utils.putMVar dictNameMVarDepEncoder mvar mvars
                                    |> Task.andThen
                                        (\_ ->
                                            Utils.mapTraverse identity Pkg.compareName (Utils.readMVar depDecoder) mvars
                                                |> Task.andThen
                                                    (\deps ->
                                                        case Utils.sequenceDictResult identity Pkg.compareName deps of
                                                            Err _ ->
                                                                Stuff.getGuidaHome
                                                                    |> Task.map
                                                                        (\home ->
                                                                            Err
                                                                                (Exit.DetailsBadDeps home
                                                                                    (List.filterMap identity (Utils.eitherLefts (Dict.values compare deps)))
                                                                                )
                                                                        )

                                                            Ok artifacts ->
                                                                let
                                                                    objs : Opt.GlobalGraph
                                                                    objs =
                                                                        Dict.foldr compare (\_ -> addObjects) Opt.empty artifacts

                                                                    ifaces : Interfaces
                                                                    ifaces =
                                                                        Dict.foldr compare (addInterfaces directDeps) Dict.empty artifacts

                                                                    foreigns : Dict String ModuleName.Raw Foreign
                                                                    foreigns =
                                                                        Dict.map (\_ -> OneOrMore.destruct Foreign) (Dict.foldr compare gatherForeigns Dict.empty (Dict.intersection compare artifacts directDeps))

                                                                    details : Details
                                                                    details =
                                                                        Details time outline 0 Dict.empty foreigns (ArtifactsFresh ifaces objs)
                                                                in
                                                                BW.writeBinary Opt.globalGraphEncoder scope (Stuff.objects (Stuff.rootPath root)) objs
                                                                    |> Task.andThen (\_ -> BW.writeBinary interfacesEncoder scope (Stuff.interfaces (Stuff.rootPath root)) ifaces)
                                                                    |> Task.andThen (\_ -> BW.writeBinary detailsEncoder scope (Stuff.details (Stuff.rootPath root)) details)
                                                                    |> Task.map (\_ -> Ok details)
                                                    )
                                        )
                            )
                )
        )


{-| Add the objects from a dependency artifact into the global object graph.
-}
addObjects : Artifacts -> Opt.GlobalGraph -> Opt.GlobalGraph
addObjects (Artifacts _ objs) graph =
    Opt.addGlobalGraph objs graph


{-| Add dependency interfaces for a package into the current interface map.

If the package is a direct dependency, its interfaces remain public; otherwise
they are privatized.

-}
addInterfaces : Dict ( String, String ) Pkg.Name a -> Pkg.Name -> Artifacts -> Interfaces -> Interfaces
addInterfaces directDeps pkg (Artifacts ifaces _) dependencyInterfaces =
    Dict.union
        dependencyInterfaces
        (Dict.fromList ModuleName.toComparableCanonical
            (List.map (Tuple.mapFirst (TypeCheck.Canonical pkg))
                (Dict.toList compare
                    (if Dict.member identity pkg directDeps then
                        ifaces

                     else
                        Dict.map (\_ -> I.privatize) ifaces
                    )
                )
            )
        )


{-| Gather public foreign package references from dependency interfaces.

Public interfaces contribute a package name to a foreign import slot.

-}
gatherForeigns : Pkg.Name -> Artifacts -> Dict String ModuleName.Raw (OneOrMore.OneOrMore Pkg.Name) -> Dict String ModuleName.Raw (OneOrMore.OneOrMore Pkg.Name)
gatherForeigns pkg (Artifacts ifaces _) foreigns =
    let
        isPublic : I.DependencyInterface -> Maybe (OneOrMore.OneOrMore Pkg.Name)
        isPublic di =
            case di of
                I.Public _ ->
                    Just (OneOrMore.one pkg)

                I.Private _ _ _ ->
                    Nothing
    in
    Utils.mapUnionWith identity compare OneOrMore.more foreigns (Utils.mapMapMaybe identity compare isPublic ifaces)



-- VERIFY DEPENDENCY


{-| Artifacts for a resolved dependency, including interface and object graph data.
-}
type Artifacts
    = Artifacts (Dict String ModuleName.Raw I.DependencyInterface) Opt.GlobalGraph


{-| Result of verifying a single dependency, either a bad dependency error or
artifact data.
-}
type alias Dep =
    Result (Maybe Exit.DetailsBadDep) Artifacts


{-| Verify a single package dependency by checking local cache state or building
it if necessary.
-}
verifyDep : Env -> MVar (Dict ( String, String ) Pkg.Name (MVar Dep)) -> Dict ( String, String ) Pkg.Name Solver.Details -> Pkg.Name -> Solver.Details -> Task Never Dep
verifyDep ((Env key _ root cache manager _ _) as env) depsMVar solution pkg ((Solver.Details vsn directDeps) as details) =
    Utils.dirDoesDirectoryExist (Stuff.package cache pkg vsn ++ "/src")
        |> Task.andThen
            (\exists ->
                let
                    fingerprint : Dict ( String, String ) Pkg.Name V.Version
                    fingerprint =
                        Utils.mapIntersectionWith identity Pkg.compareName (\(Solver.Details v _) _ -> v) solution directDeps
                in
                if exists then
                    Reporting.report key Reporting.DCached
                        |> Task.andThen
                            (\_ ->
                                File.readBinary artifactCacheDecoder (Stuff.package cache pkg vsn ++ "/artifacts.dat")
                                    |> Task.andThen
                                        (\maybeCache ->
                                            case maybeCache of
                                                Nothing ->
                                                    build (Stuff.rootToTarget root) env key cache depsMVar pkg details fingerprint EverySet.empty

                                                Just (ArtifactCache fingerprints artifacts) ->
                                                    if EverySet.member toComparableFingerprint fingerprint fingerprints then
                                                        Task.map (\_ -> Ok artifacts) (Reporting.report key Reporting.DBuilt)

                                                    else
                                                        build (Stuff.rootToTarget root) env key cache depsMVar pkg details fingerprint fingerprints
                                        )
                            )

                else
                    Reporting.report key Reporting.DRequested
                        |> Task.andThen
                            (\_ ->
                                downloadPackage cache manager pkg vsn
                                    |> Task.andThen
                                        (\result ->
                                            case result of
                                                Err problem ->
                                                    Reporting.report key (Reporting.DFailed pkg vsn)
                                                        |> Task.map (\_ -> Err (Just (Exit.BD_BadDownload pkg vsn problem)))

                                                Ok () ->
                                                    Reporting.report key (Reporting.DReceived pkg vsn)
                                                        |> Task.andThen (\_ -> build (Stuff.rootToTarget root) env key cache depsMVar pkg details fingerprint EverySet.empty)
                                        )
                            )
            )



-- ARTIFACT CACHE


{-| A cached artifact snapshot for a dependency, including fingerprints and
artifact contents.
-}
type ArtifactCache
    = ArtifactCache (EverySet (List ( ( String, String ), ( Int, Int, Int ) )) Fingerprint) Artifacts


{-| A dependency fingerprint that maps package names to exact versions.
-}
type alias Fingerprint =
    Dict ( String, String ) Pkg.Name V.Version


{-| Convert a fingerprint into a comparable list representation for set storage.
-}
toComparableFingerprint : Fingerprint -> List ( ( String, String ), ( Int, Int, Int ) )
toComparableFingerprint fingerprint =
    Dict.toList compare fingerprint
        |> List.map (Tuple.mapSecond V.toComparable)



-- BUILD


{-| Build or verify a dependency package, using cached artifacts when possible.

This function manages package download, artifact cache validation, and actual
package compilation when the cache is stale.

-}
build : Target -> Env -> Reporting.DKey -> Stuff.PackageCache -> MVar (Dict ( String, String ) Pkg.Name (MVar Dep)) -> Pkg.Name -> Solver.Details -> Fingerprint -> EverySet (List ( ( String, String ), ( Int, Int, Int ) )) Fingerprint -> Task Never Dep
build target (Env _ _ _ _ _ connection registry) key cache depsMVar pkg (Solver.Details vsn _) f fs =
    Stuff.findRootIn (Stuff.package cache pkg vsn)
        -- TODO/FIXME remove the need to default to GuidaRoot
        |> Task.map (Maybe.withDefault (Stuff.GuidaRoot (Stuff.package cache pkg vsn)))
        |> Task.andThen
            (\root ->
                Outline.read root
                    |> Task.andThen
                        (\eitherOutline ->
                            let
                                pkgBuild : Outline.Exposed -> Dict ( String, String ) Pkg.Name Con.Constraint -> Task Never Dep
                                pkgBuild exposed deps =
                                    Utils.readMVar dictPkgNameMVarDepDecoder depsMVar
                                        |> Task.andThen
                                            (\allDeps ->
                                                Utils.mapTraverse identity Pkg.compareName (Utils.readMVar depDecoder) (Dict.intersection compare allDeps (Pkg.sanitizeElmDeps target deps))
                                                    |> Task.andThen
                                                        (\directDeps ->
                                                            case Utils.sequenceDictResult identity Pkg.compareName directDeps of
                                                                Err _ ->
                                                                    Reporting.report key Reporting.DBroken
                                                                        |> Task.map (\_ -> Err Nothing)

                                                                Ok directArtifacts ->
                                                                    getDocsStatus cache pkg vsn
                                                                        |> Task.andThen
                                                                            (\docsStatus ->
                                                                                Utils.newEmptyMVar
                                                                                    |> Task.andThen
                                                                                        (\mvar ->
                                                                                            let
                                                                                                src : String
                                                                                                src =
                                                                                                    Stuff.package cache pkg vsn ++ "/src"

                                                                                                foreignDeps : Dict String ModuleName.Raw ForeignInterface
                                                                                                foreignDeps =
                                                                                                    gatherForeignInterfaces directArtifacts

                                                                                                exposedDict : Dict String ModuleName.Raw ()
                                                                                                exposedDict =
                                                                                                    Utils.mapFromKeys identity (\_ -> ()) (Outline.flattenExposed exposed)
                                                                                            in
                                                                                            Utils.mapTraverseWithKey identity compare (always << fork (BE.maybe statusEncoder) << crawlModule target root foreignDeps mvar pkg src docsStatus) exposedDict
                                                                                                |> Task.andThen
                                                                                                    (\mvars ->
                                                                                                        Utils.putMVar statusDictEncoder mvar mvars
                                                                                                            |> Task.andThen (\_ -> Utils.dictMapM_ compare (Utils.readMVar (BD.maybe statusDecoder)) mvars)
                                                                                                            |> Task.andThen (\_ -> Task.andThen (Utils.mapTraverse identity compare (Utils.readMVar (BD.maybe statusDecoder))) (Utils.readMVar statusDictDecoder mvar))
                                                                                                            |> Task.andThen
                                                                                                                (\maybeStatuses ->
                                                                                                                    case Utils.sequenceDictMaybe identity compare maybeStatuses of
                                                                                                                        Nothing ->
                                                                                                                            Reporting.report key Reporting.DBroken
                                                                                                                                |> Task.map (\_ -> Err (Just (Exit.BD_BadBuild target pkg vsn f)))

                                                                                                                        Just statuses ->
                                                                                                                            Utils.newEmptyMVar
                                                                                                                                |> Task.andThen
                                                                                                                                    (\rmvar ->
                                                                                                                                        Utils.mapTraverse identity compare (fork (BE.maybe dResultEncoder) << compile target root pkg rmvar) statuses
                                                                                                                                            |> Task.andThen
                                                                                                                                                (\rmvars ->
                                                                                                                                                    Utils.putMVar dictRawMVarMaybeDResultEncoder rmvar rmvars
                                                                                                                                                        |> Task.andThen (\_ -> Utils.mapTraverse identity compare (Utils.readMVar (BD.maybe dResultDecoder)) rmvars)
                                                                                                                                                        |> Task.andThen
                                                                                                                                                            (\maybeResults ->
                                                                                                                                                                case Utils.sequenceDictMaybe identity compare maybeResults of
                                                                                                                                                                    Nothing ->
                                                                                                                                                                        Reporting.report key Reporting.DBroken
                                                                                                                                                                            |> Task.map (\_ -> Err (Just (Exit.BD_BadBuild target pkg vsn f)))

                                                                                                                                                                    Just results ->
                                                                                                                                                                        let
                                                                                                                                                                            ifaces : Dict String ModuleName.Raw I.DependencyInterface
                                                                                                                                                                            ifaces =
                                                                                                                                                                                gatherInterfaces exposedDict results

                                                                                                                                                                            objects : Opt.GlobalGraph
                                                                                                                                                                            objects =
                                                                                                                                                                                gatherObjects target results

                                                                                                                                                                            artifacts : Artifacts
                                                                                                                                                                            artifacts =
                                                                                                                                                                                Artifacts ifaces objects
                                                                                                                                                                        in
                                                                                                                                                                        writeDocs cache pkg vsn docsStatus results
                                                                                                                                                                            |> Task.andThen
                                                                                                                                                                                (\_ ->
                                                                                                                                                                                    let
                                                                                                                                                                                        path : String
                                                                                                                                                                                        path =
                                                                                                                                                                                            Stuff.package cache pkg vsn ++ "/artifacts.dat"

                                                                                                                                                                                        fingerprints : EverySet (List ( ( String, String ), ( Int, Int, Int ) )) Fingerprint
                                                                                                                                                                                        fingerprints =
                                                                                                                                                                                            EverySet.insert toComparableFingerprint f fs
                                                                                                                                                                                    in
                                                                                                                                                                                    File.writeBinary artifactCacheEncoder path (ArtifactCache fingerprints artifacts)
                                                                                                                                                                                )
                                                                                                                                                                            |> Task.andThen (\_ -> Reporting.report key Reporting.DBuilt)
                                                                                                                                                                            |> Task.map (\_ -> Ok artifacts)
                                                                                                                                                            )
                                                                                                                                                )
                                                                                                                                    )
                                                                                                                )
                                                                                                    )
                                                                                        )
                                                                            )
                                                        )
                                            )
                            in
                            case eitherOutline of
                                Err _ ->
                                    Reporting.report key Reporting.DBroken
                                        |> Task.map (\_ -> Err (Just (Exit.BD_BadBuild target pkg vsn f)))

                                Ok (Outline.App (Outline.GuidaAppOutline _ _ _ _ _ _)) ->
                                    Reporting.report key Reporting.DBroken
                                        |> Task.map (\_ -> Err (Just (Exit.BD_BadBuild target pkg vsn f)))

                                Ok (Outline.App (Outline.ElmAppOutline _ _ _ _ _ _)) ->
                                    Reporting.report key Reporting.DBroken
                                        |> Task.map (\_ -> Err (Just (Exit.BD_BadBuild target pkg vsn f)))

                                Ok (Outline.Pkg (Outline.GuidaPkgOutline _ _ _ _ exposed deps _ _)) ->
                                    pkgBuild exposed deps

                                Ok (Outline.Pkg (Outline.ElmPkgOutline _ _ _ _ exposed deps _ _)) ->
                                    case target of
                                        Target.GuidaTarget ->
                                            case Registry.getVersions_ Registry.KeepAllVersions Pkg.stdlib registry of
                                                Err _ ->
                                                    Task.succeed (Err Nothing)

                                                Ok (Registry.KnownVersions _ _) ->
                                                    let
                                                        cons : Dict ( String, String ) Pkg.Name Con.Constraint
                                                        cons =
                                                            Dict.insert identity Pkg.stdlib Con.anything deps
                                                    in
                                                    Solver.verify target cache connection registry cons
                                                        |> Task.andThen
                                                            (\result ->
                                                                case result of
                                                                    Solver.SolverOk solution ->
                                                                        let
                                                                            (Solver.Details stdlibVsn _) =
                                                                                Utils.find identity Pkg.stdlib solution

                                                                            con : Con.Constraint
                                                                            con =
                                                                                Con.untilNextMajor stdlibVsn
                                                                        in
                                                                        pkgBuild exposed
                                                                            (Dict.filter (\( author, _ ) _ -> author /= Pkg.elm && author /= Pkg.elmExplorations) deps
                                                                                |> Dict.insert identity Pkg.stdlib con
                                                                            )

                                                                    Solver.NoSolution ->
                                                                        Task.succeed (Err Nothing)

                                                                    Solver.NoOfflineSolution ->
                                                                        Task.succeed (Err Nothing)

                                                                    Solver.SolverErr _ ->
                                                                        Task.succeed (Err Nothing)
                                                            )

                                        Target.ElmTarget ->
                                            pkgBuild exposed deps
                        )
            )



-- GATHER


{-| Collect global object graph data from module compilation results.
-}
gatherObjects : Target -> Dict String ModuleName.Raw DResult -> Opt.GlobalGraph
gatherObjects target results =
    Dict.foldr compare (addLocalGraph target) Opt.empty results


{-| Add the local compile output for a module into the global object graph.
-}
addLocalGraph : Target -> ModuleName.Raw -> DResult -> Opt.GlobalGraph -> Opt.GlobalGraph
addLocalGraph target name status graph =
    case status of
        RLocal _ objs _ ->
            Opt.addLocalGraph objs graph

        RForeign _ ->
            graph

        RKernelLocal cs ->
            Opt.addKernel (Name.getKernel target name) cs graph

        RKernelForeign ->
            graph


{-| Build the dependency interface set for exposed modules from compile results.
-}
gatherInterfaces : Dict String ModuleName.Raw () -> Dict String ModuleName.Raw DResult -> Dict String ModuleName.Raw I.DependencyInterface
gatherInterfaces exposed artifacts =
    let
        onLeft : a -> b -> c -> d
        onLeft _ _ _ =
            crash "compiler bug manifesting in Guida.Details.gatherInterfaces"

        onBoth : comparable -> () -> DResult -> Dict comparable comparable I.DependencyInterface -> Dict comparable comparable I.DependencyInterface
        onBoth k () iface =
            toLocalInterface I.public iface
                |> Maybe.map (Dict.insert identity k)
                |> Maybe.withDefault identity

        onRight : comparable -> DResult -> Dict comparable comparable I.DependencyInterface -> Dict comparable comparable I.DependencyInterface
        onRight k iface =
            toLocalInterface I.private iface
                |> Maybe.map (Dict.insert identity k)
                |> Maybe.withDefault identity
    in
    Dict.merge compare onLeft onBoth onRight exposed artifacts Dict.empty


{-| Extract a local package interface from a compile result, if available.
-}
toLocalInterface : (I.Interface -> a) -> DResult -> Maybe a
toLocalInterface func result =
    case result of
        RLocal iface _ _ ->
            Just (func iface)

        RForeign _ ->
            Nothing

        RKernelLocal _ ->
            Nothing

        RKernelForeign ->
            Nothing



-- GATHER FOREIGN INTERFACES


{-| Describes whether a foreign dependency interface is specific or ambiguous.
-}
type ForeignInterface
    = ForeignAmbiguous
    | ForeignSpecific I.Interface


{-| Gather the effective foreign dependency interface for each referenced module.
-}
gatherForeignInterfaces : Dict ( String, String ) Pkg.Name Artifacts -> Dict String ModuleName.Raw ForeignInterface
gatherForeignInterfaces directArtifacts =
    let
        finalize : I.Interface -> List I.Interface -> ForeignInterface
        finalize i is =
            case is of
                [] ->
                    ForeignSpecific i

                _ :: _ ->
                    ForeignAmbiguous

        gather : Pkg.Name -> Artifacts -> Dict String ModuleName.Raw (OneOrMore.OneOrMore I.Interface) -> Dict String ModuleName.Raw (OneOrMore.OneOrMore I.Interface)
        gather _ (Artifacts ifaces _) buckets =
            Utils.mapUnionWith identity compare OneOrMore.more buckets (Utils.mapMapMaybe identity compare isPublic ifaces)

        isPublic : I.DependencyInterface -> Maybe (OneOrMore.OneOrMore I.Interface)
        isPublic di =
            case di of
                I.Public iface ->
                    Just (OneOrMore.one iface)

                I.Private _ _ _ ->
                    Nothing
    in
    Dict.map (\_ -> OneOrMore.destruct finalize) <|
        Dict.foldr compare gather Dict.empty directArtifacts



-- CRAWL


{-| Map of module names to the status values produced while crawling a project.
-}
type alias StatusDict =
    Dict String ModuleName.Raw (MVar (Maybe Status))


{-| Status for a module discovered while crawling project dependencies.
-}
type Status
    = SLocal DocsStatus (Dict String ModuleName.Raw ()) Src.Module
    | SForeign I.Interface
    | SKernelLocal (List Kernel.Chunk)
    | SKernelForeign


{-| Crawl a module reference to determine whether it should be compiled,
resolved as a foreign interface, or treated as kernel code.
-}
crawlModule : Target -> Stuff.Root -> Dict String ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> ModuleName.Raw -> Task Never (Maybe Status)
crawlModule target root foreignDeps mvar pkg src docsStatus name =
    let
        path : String -> FilePath
        path extension =
            Utils.fpCombine src (Utils.fpAddExtension (ModuleName.toFilePath name) extension)

        guidaPath : FilePath
        guidaPath =
            path "guida"
    in
    File.exists guidaPath
        |> Task.andThen
            (\guidaExists ->
                let
                    elmPath : FilePath
                    elmPath =
                        path "elm"
                in
                File.exists elmPath
                    |> Task.andThen
                        (\elmExists ->
                            case Dict.get identity name foreignDeps of
                                Just ForeignAmbiguous ->
                                    Task.succeed Nothing

                                Just (ForeignSpecific iface) ->
                                    if guidaExists || elmExists then
                                        Task.succeed Nothing

                                    else
                                        Task.succeed (Just (SForeign iface))

                                Nothing ->
                                    if guidaExists then
                                        crawlFile target root SV.Guida foreignDeps mvar pkg src docsStatus name guidaPath

                                    else if elmExists then
                                        crawlFile target root SV.Elm foreignDeps mvar pkg src docsStatus name elmPath

                                    else if Pkg.isKernel pkg && Name.isKernel target name then
                                        crawlKernel target root foreignDeps mvar pkg src name

                                    else
                                        Task.succeed Nothing
                        )
            )


{-| Parse and crawl a source file to build its import status and determine whether
it contributes a local module status.
-}
crawlFile : Target -> Stuff.Root -> SyntaxVersion -> Dict String ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> ModuleName.Raw -> FilePath -> Task Never (Maybe Status)
crawlFile target root syntaxVersion foreignDeps mvar pkg src docsStatus expectedName path =
    File.readUtf8 path
        |> Task.andThen
            (\bytes ->
                case Parse.fromByteString target syntaxVersion (Parse.Package pkg) bytes of
                    Ok ((Src.Module _ (Just (A.At _ actualName)) _ _ imports _ _ _ _ _) as modul) ->
                        if expectedName == actualName then
                            crawlImports target root foreignDeps mvar pkg src imports
                                |> Task.map (\deps -> Just (SLocal docsStatus deps modul))

                        else
                            Task.succeed Nothing

                    _ ->
                        Task.succeed Nothing
            )


{-| Crawl a module's import list to resolve referenced modules and build the
module dependency status map.
-}
crawlImports : Target -> Stuff.Root -> Dict String ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> List Src.Import -> Task Never (Dict String ModuleName.Raw ())
crawlImports target root foreignDeps mvar pkg src imports =
    Utils.takeMVar statusDictDecoder mvar
        |> Task.andThen
            (\statusDict ->
                let
                    deps : Dict String Name.Name ()
                    deps =
                        Dict.fromList identity (List.map (\i -> ( Src.getImportName i, () )) imports)

                    news : Dict String Name.Name ()
                    news =
                        Dict.diff deps statusDict
                in
                Utils.mapTraverseWithKey identity compare (always << fork (BE.maybe statusEncoder) << crawlModule target root foreignDeps mvar pkg src DocsNotNeeded) news
                    |> Task.andThen
                        (\mvars ->
                            Utils.putMVar statusDictEncoder mvar (Dict.union mvars statusDict)
                                |> Task.andThen (\_ -> Utils.dictMapM_ compare (Utils.readMVar (BD.maybe statusDecoder)) mvars)
                                |> Task.map (\_ -> deps)
                        )
            )


{-| Crawl a kernel JavaScript dependency and treat it as either local kernel code
or a foreign kernel import.
-}
crawlKernel : Target -> Stuff.Root -> Dict String ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> ModuleName.Raw -> Task Never (Maybe Status)
crawlKernel target root foreignDeps mvar pkg src name =
    let
        path : FilePath
        path =
            Utils.fpCombine src (Utils.fpAddExtension (ModuleName.toFilePath name) "js")
    in
    File.exists path
        |> Task.andThen
            (\exists ->
                if exists then
                    File.readUtf8 path
                        |> Task.andThen
                            (\bytes ->
                                case Kernel.fromByteString target pkg (Utils.mapMapMaybe identity compare getDepHome foreignDeps) bytes of
                                    Nothing ->
                                        Task.succeed Nothing

                                    Just (Kernel.Content imports chunks) ->
                                        crawlImports target root foreignDeps mvar pkg src (List.map Src.c1Value imports)
                                            |> Task.map (\_ -> Just (SKernelLocal chunks))
                            )

                else
                    Task.succeed (Just SKernelForeign)
            )


{-| Extract the package name from a resolved foreign interface, if available.
-}
getDepHome : ForeignInterface -> Maybe Pkg.Name
getDepHome fi =
    case fi of
        ForeignSpecific (I.Interface pkg _ _ _ _) ->
            Just pkg

        ForeignAmbiguous ->
            Nothing



-- COMPILE


{-| A compile result for a source dependency, including interfaces, local
object graph fragments, or kernel data.
-}
type DResult
    = RLocal I.Interface Opt.LocalGraph (Maybe Docs.Module)
    | RForeign I.Interface
    | RKernelLocal (List Kernel.Chunk)
    | RKernelForeign


{-| Compile a module based on its status, producing a `DResult` when possible.
-}
compile : Target -> Stuff.Root -> Pkg.Name -> MVar (Dict String ModuleName.Raw (MVar (Maybe DResult))) -> Status -> Task Never (Maybe DResult)
compile target root pkg mvar status =
    case status of
        SLocal docsStatus deps modul ->
            Utils.readMVar moduleNameRawMVarMaybeDResultDecoder mvar
                |> Task.andThen
                    (\resultsDict ->
                        Utils.mapTraverse identity compare (Utils.readMVar (BD.maybe dResultDecoder)) (Dict.intersection compare resultsDict deps)
                            |> Task.andThen
                                (\maybeResults ->
                                    case Utils.sequenceDictMaybe identity compare maybeResults of
                                        Just results ->
                                            Compile.compile target root pkg (Utils.mapMapMaybe identity compare getInterface results) modul
                                                |> Task.map
                                                    (\( _, result ) ->
                                                        case result of
                                                            Err _ ->
                                                                Nothing

                                                            Ok (Compile.Artifacts canonical annotations objects) ->
                                                                let
                                                                    ifaces : I.Interface
                                                                    ifaces =
                                                                        I.fromModule pkg canonical annotations

                                                                    docs : Maybe Docs.Module
                                                                    docs =
                                                                        makeDocs target docsStatus canonical
                                                                in
                                                                Just (RLocal ifaces objects docs)
                                                    )

                                        Nothing ->
                                            Task.succeed Nothing
                                )
                    )

        SForeign iface ->
            Task.succeed (Just (RForeign iface))

        SKernelLocal chunks ->
            Task.succeed (Just (RKernelLocal chunks))

        SKernelForeign ->
            Task.succeed (Just RKernelForeign)


{-| Extract the interface from a compile result when present.
-}
getInterface : DResult -> Maybe I.Interface
getInterface result =
    case result of
        RLocal iface _ _ ->
            Just iface

        RForeign iface ->
            Just iface

        RKernelLocal _ ->
            Nothing

        RKernelForeign ->
            Nothing



-- MAKE DOCS


{-| Whether generated package docs are already present on disk.
-}
type DocsStatus
    = DocsNeeded
    | DocsNotNeeded


{-| Determine if documentation needs to be generated for the given package
version by checking for an existing `docs.json` file in package cache.
-}
getDocsStatus : Stuff.PackageCache -> Pkg.Name -> V.Version -> Task Never DocsStatus
getDocsStatus cache pkg vsn =
    File.exists (Stuff.package cache pkg vsn ++ "/docs.json")
        |> Task.map
            (\exists ->
                if exists then
                    DocsNotNeeded

                else
                    DocsNeeded
            )


{-| Generate docs for a module when documentation is required.
-}
makeDocs : Target -> DocsStatus -> Can.Module -> Maybe Docs.Module
makeDocs target status modul =
    case status of
        DocsNeeded ->
            case Docs.fromModule target modul of
                Ok docs ->
                    Just docs

                Err _ ->
                    Nothing

        DocsNotNeeded ->
            Nothing


{-| Write generated package docs to the package cache when needed.
-}
writeDocs : Stuff.PackageCache -> Pkg.Name -> V.Version -> DocsStatus -> Dict String ModuleName.Raw DResult -> Task Never ()
writeDocs cache pkg vsn status results =
    case status of
        DocsNeeded ->
            E.writeUgly (Stuff.package cache pkg vsn ++ "/docs.json")
                (Docs.encode (Utils.mapMapMaybe identity compare toDocs results))

        DocsNotNeeded ->
            Task.succeed ()


{-| Extract generated documentation from a compile result.
-}
toDocs : DResult -> Maybe Docs.Module
toDocs result =
    case result of
        RLocal _ _ docs ->
            docs

        RForeign _ ->
            Nothing

        RKernelLocal _ ->
            Nothing

        RKernelForeign ->
            Nothing



-- DOWNLOAD PACKAGE


{-| Download a package archive from the remote package endpoint and store it in
package cache.
-}
downloadPackage : Stuff.PackageCache -> Http.Manager -> Pkg.Name -> V.Version -> Task Never (Result Exit.PackageProblem ())
downloadPackage cache manager pkg vsn =
    Website.metadata pkg vsn "endpoint.json"
        |> Task.andThen
            (\url ->
                Http.get manager url [] identity (Task.succeed << Ok)
                    |> Task.andThen
                        (\eitherByteString ->
                            case eitherByteString of
                                Err err ->
                                    Task.succeed (Err (Exit.PP_BadEndpointRequest err))

                                Ok byteString ->
                                    case D.fromByteString endpointDecoder byteString of
                                        Err _ ->
                                            Task.succeed (Err (Exit.PP_BadEndpointContent url))

                                        Ok ( endpoint, expectedHash ) ->
                                            Http.getArchive manager endpoint Exit.PP_BadArchiveRequest (Exit.PP_BadArchiveContent endpoint) <|
                                                \( sha, archive ) ->
                                                    if expectedHash == Http.shaToChars sha then
                                                        Task.map Ok (File.writePackage (Stuff.package cache pkg vsn) archive)

                                                    else
                                                        Task.succeed (Err (Exit.PP_BadArchiveHash endpoint expectedHash (Http.shaToChars sha)))
                        )
            )


{-| Decode a package endpoint response containing the archive URL and
expected hash.
-}
endpointDecoder : D.Decoder e ( String, String )
endpointDecoder =
    D.field "url" D.string
        |> D.bind
            (\url ->
                D.field "hash" D.string
                    |> D.fmap (\hash -> ( url, hash ))
            )



-- ENCODERS and DECODERS


detailsEncoder : Details -> BE.Encoder
detailsEncoder (Details oldTime outline buildID locals foreigns extras) =
    BE.sequence
        [ File.timeEncoder oldTime
        , validOutlineEncoder outline
        , BE.int buildID
        , BE.assocListDict compare ModuleName.rawEncoder localEncoder locals
        , BE.assocListDict compare ModuleName.rawEncoder foreignEncoder foreigns
        , extrasEncoder extras
        ]


detailsDecoder : BD.Decoder Details
detailsDecoder =
    BD.map6 Details
        File.timeDecoder
        validOutlineDecoder
        BD.int
        (BD.assocListDict identity ModuleName.rawDecoder localDecoder)
        (BD.assocListDict identity ModuleName.rawDecoder foreignDecoder)
        extrasDecoder


interfacesEncoder : Interfaces -> BE.Encoder
interfacesEncoder =
    BE.assocListDict ModuleName.compareCanonical ModuleName.canonicalEncoder I.dependencyInterfaceEncoder


interfacesDecoder : BD.Decoder Interfaces
interfacesDecoder =
    BD.assocListDict ModuleName.toComparableCanonical ModuleName.canonicalDecoder I.dependencyInterfaceDecoder


resultRegistryProblemEnvEncoder : Result Exit.RegistryProblem Solver.Env -> BE.Encoder
resultRegistryProblemEnvEncoder =
    BE.result Exit.registryProblemEncoder Solver.envEncoder


resultRegistryProblemEnvDecoder : BD.Decoder (Result Exit.RegistryProblem Solver.Env)
resultRegistryProblemEnvDecoder =
    BD.result Exit.registryProblemDecoder Solver.envDecoder


depEncoder : Dep -> BE.Encoder
depEncoder dep =
    BE.result (BE.maybe Exit.detailsBadDepEncoder) artifactsEncoder dep


depDecoder : BD.Decoder Dep
depDecoder =
    BD.result (BD.maybe Exit.detailsBadDepDecoder) artifactsDecoder


artifactsEncoder : Artifacts -> BE.Encoder
artifactsEncoder (Artifacts ifaces objects) =
    BE.sequence
        [ BE.assocListDict compare ModuleName.rawEncoder I.dependencyInterfaceEncoder ifaces
        , Opt.globalGraphEncoder objects
        ]


artifactsDecoder : BD.Decoder Artifacts
artifactsDecoder =
    BD.map2 Artifacts
        (BD.assocListDict identity ModuleName.rawDecoder I.dependencyInterfaceDecoder)
        Opt.globalGraphDecoder


dictNameMVarDepEncoder : Dict ( String, String ) Pkg.Name (MVar Dep) -> BE.Encoder
dictNameMVarDepEncoder =
    BE.assocListDict compare Pkg.nameEncoder Utils.mVarEncoder


artifactCacheEncoder : ArtifactCache -> BE.Encoder
artifactCacheEncoder (ArtifactCache fingerprints artifacts) =
    BE.sequence
        [ BE.everySet (\_ _ -> EQ) fingerprintEncoder fingerprints
        , artifactsEncoder artifacts
        ]


artifactCacheDecoder : BD.Decoder ArtifactCache
artifactCacheDecoder =
    BD.map2 ArtifactCache
        (BD.everySet toComparableFingerprint fingerprintDecoder)
        artifactsDecoder


dictPkgNameMVarDepDecoder : BD.Decoder (Dict ( String, String ) Pkg.Name (MVar Dep))
dictPkgNameMVarDepDecoder =
    BD.assocListDict identity Pkg.nameDecoder Utils.mVarDecoder


statusEncoder : Status -> BE.Encoder
statusEncoder status =
    case status of
        SLocal docsStatus deps modul ->
            BE.sequence
                [ BE.unsignedInt8 0
                , docsStatusEncoder docsStatus
                , BE.list ModuleName.rawEncoder (Dict.keys compare deps)
                , Src.moduleEncoder modul
                ]

        SForeign iface ->
            BE.sequence
                [ BE.unsignedInt8 1
                , I.interfaceEncoder iface
                ]

        SKernelLocal chunks ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.list Kernel.chunkEncoder chunks
                ]

        SKernelForeign ->
            BE.unsignedInt8 3


statusDecoder : BD.Decoder Status
statusDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map3 SLocal
                            docsStatusDecoder
                            (BD.list ModuleName.rawDecoder
                                |> BD.map (Dict.fromList identity << List.map (\dep -> ( dep, () )))
                            )
                            Src.moduleDecoder

                    1 ->
                        BD.map SForeign I.interfaceDecoder

                    2 ->
                        BD.map SKernelLocal (BD.list Kernel.chunkDecoder)

                    3 ->
                        BD.succeed SKernelForeign

                    _ ->
                        BD.fail
            )


dictRawMVarMaybeDResultEncoder : Dict String ModuleName.Raw (MVar (Maybe DResult)) -> BE.Encoder
dictRawMVarMaybeDResultEncoder =
    BE.assocListDict compare ModuleName.rawEncoder Utils.mVarEncoder


moduleNameRawMVarMaybeDResultDecoder : BD.Decoder (Dict String ModuleName.Raw (MVar (Maybe DResult)))
moduleNameRawMVarMaybeDResultDecoder =
    BD.assocListDict identity ModuleName.rawDecoder Utils.mVarDecoder


dResultEncoder : DResult -> BE.Encoder
dResultEncoder dResult =
    case dResult of
        RLocal ifaces objects docs ->
            BE.sequence
                [ BE.unsignedInt8 0
                , I.interfaceEncoder ifaces
                , Opt.localGraphEncoder objects
                , BE.maybe Docs.bytesModuleEncoder docs
                ]

        RForeign iface ->
            BE.sequence
                [ BE.unsignedInt8 1
                , I.interfaceEncoder iface
                ]

        RKernelLocal chunks ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.list Kernel.chunkEncoder chunks
                ]

        RKernelForeign ->
            BE.unsignedInt8 3


dResultDecoder : BD.Decoder DResult
dResultDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map3 RLocal
                            I.interfaceDecoder
                            Opt.localGraphDecoder
                            (BD.maybe Docs.bytesModuleDecoder)

                    1 ->
                        BD.map RForeign I.interfaceDecoder

                    2 ->
                        BD.map RKernelLocal (BD.list Kernel.chunkDecoder)

                    3 ->
                        BD.succeed RKernelForeign

                    _ ->
                        BD.fail
            )


statusDictEncoder : StatusDict -> BE.Encoder
statusDictEncoder statusDict =
    BE.assocListDict compare ModuleName.rawEncoder Utils.mVarEncoder statusDict


statusDictDecoder : BD.Decoder StatusDict
statusDictDecoder =
    BD.assocListDict identity ModuleName.rawDecoder Utils.mVarDecoder


localEncoder : Local -> BE.Encoder
localEncoder (Local path time deps hasMain lastChange lastCompile) =
    BE.sequence
        [ BE.string path
        , File.timeEncoder time
        , BE.list ModuleName.rawEncoder deps
        , BE.bool hasMain
        , BE.int lastChange
        , BE.int lastCompile
        ]


localDecoder : BD.Decoder Local
localDecoder =
    BD.map6 Local
        BD.string
        File.timeDecoder
        (BD.list ModuleName.rawDecoder)
        BD.bool
        BD.int
        BD.int


validOutlineEncoder : ValidOutline -> BE.Encoder
validOutlineEncoder validOutline =
    case validOutline of
        ValidApp srcDirs ->
            BE.sequence
                [ BE.unsignedInt8 0
                , BE.nonempty Outline.srcDirEncoder srcDirs
                ]

        ValidPkg pkg exposedList exactDeps ->
            BE.sequence
                [ BE.unsignedInt8 1
                , Pkg.nameEncoder pkg
                , BE.list ModuleName.rawEncoder exposedList
                , BE.assocListDict compare Pkg.nameEncoder V.versionEncoder exactDeps
                ]


validOutlineDecoder : BD.Decoder ValidOutline
validOutlineDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map ValidApp (BD.nonempty Outline.srcDirDecoder)

                    1 ->
                        BD.map3 ValidPkg
                            Pkg.nameDecoder
                            (BD.list ModuleName.rawDecoder)
                            (BD.assocListDict identity Pkg.nameDecoder V.versionDecoder)

                    _ ->
                        BD.fail
            )


foreignEncoder : Foreign -> BE.Encoder
foreignEncoder (Foreign dep deps) =
    BE.sequence
        [ Pkg.nameEncoder dep
        , BE.list Pkg.nameEncoder deps
        ]


foreignDecoder : BD.Decoder Foreign
foreignDecoder =
    BD.map2 Foreign
        Pkg.nameDecoder
        (BD.list Pkg.nameDecoder)


extrasEncoder : Extras -> BE.Encoder
extrasEncoder extras =
    case extras of
        ArtifactsCached ->
            BE.unsignedInt8 0

        ArtifactsFresh ifaces objs ->
            BE.sequence
                [ BE.unsignedInt8 1
                , interfacesEncoder ifaces
                , Opt.globalGraphEncoder objs
                ]


extrasDecoder : BD.Decoder Extras
extrasDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed ArtifactsCached

                    1 ->
                        BD.map2 ArtifactsFresh
                            interfacesDecoder
                            Opt.globalGraphDecoder

                    _ ->
                        BD.fail
            )


fingerprintEncoder : Fingerprint -> BE.Encoder
fingerprintEncoder =
    BE.assocListDict compare Pkg.nameEncoder V.versionEncoder


fingerprintDecoder : BD.Decoder Fingerprint
fingerprintDecoder =
    BD.assocListDict identity Pkg.nameDecoder V.versionDecoder


docsStatusEncoder : DocsStatus -> BE.Encoder
docsStatusEncoder docsStatus =
    BE.unsignedInt8
        (case docsStatus of
            DocsNeeded ->
                0

            DocsNotNeeded ->
                1
        )


docsStatusDecoder : BD.Decoder DocsStatus
docsStatusDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed DocsNeeded

                    1 ->
                        BD.succeed DocsNotNeeded

                    _ ->
                        BD.fail
            )
