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
import Control.Concurrent.MVar as MVar exposing (MVar)
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Process
import System.TypeCheck.IO as TypeCheck
import Task exposing (Task)
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Crash exposing (crash)
import Utils.Main as Utils exposing (FilePath)
import Utils.Task.Extra as Task



-- DETAILS


type Details
    = Details File.Time ValidOutline BuildID (Dict String ModuleName.Raw Local) (Dict String ModuleName.Raw Foreign) Extras


type alias BuildID =
    Int


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


type Local
    = Local FilePath File.Time (List ModuleName.Raw) Bool BuildID BuildID


type Foreign
    = Foreign Pkg.Name (List Pkg.Name)


type Extras
    = ArtifactsCached
    | ArtifactsFresh Interfaces Opt.GlobalGraph


type alias Interfaces =
    Dict (List String) TypeCheck.Canonical I.DependencyInterface



-- LOAD ARTIFACTS


loadObjects : FilePath -> Details -> Task Never (MVar (Maybe Opt.GlobalGraph))
loadObjects root (Details _ _ _ _ _ extras) =
    case extras of
        ArtifactsFresh _ o ->
            MVar.newMVar (Just o)

        ArtifactsCached ->
            fork (File.readBinary Opt.globalGraphDecoder (Stuff.objects root))


loadInterfaces : FilePath -> Details -> Task Never (MVar (Maybe Interfaces))
loadInterfaces root (Details _ _ _ _ _ extras) =
    case extras of
        ArtifactsFresh i _ ->
            MVar.newMVar (Just i)

        ArtifactsCached ->
            fork (File.readBinary interfacesDecoder (Stuff.interfaces root))



-- VERIFY INSTALL -- used by Install


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


type Env
    = Env Reporting.DKey BW.Scope Stuff.Root Stuff.PackageCache Http.Manager Solver.Connection Registry.Registry


initEnv : Reporting.DKey -> BW.Scope -> Stuff.Root -> Task Never (Result Exit.Details ( Env, Outline.Outline ))
initEnv key scope root =
    fork Solver.initEnv
        |> Task.andThen
            (\mvar ->
                Outline.read root
                    |> Task.andThen
                        (\eitherOutline ->
                            case eitherOutline of
                                Err problem ->
                                    Task.succeed (Err (Exit.DetailsBadOutline problem))

                                Ok outline ->
                                    MVar.readMVar mvar
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


noGuidaDups : k -> v -> v -> Task Exit.Details v
noGuidaDups _ _ _ =
    Task.fail Exit.DetailsHandEditedGuidaDependencies


noElmDups : k -> v -> v -> Task Exit.Details v
noElmDups _ _ _ =
    Task.fail Exit.DetailsHandEditedElmDependencies


allowEqualGuidaDups : k -> v -> v -> Task Exit.Details v
allowEqualGuidaDups _ v1 v2 =
    if v1 == v2 then
        Task.succeed v1

    else
        Task.fail Exit.DetailsHandEditedGuidaDependencies


allowEqualElmDups : k -> v -> v -> Task Exit.Details v
allowEqualElmDups _ v1 v2 =
    if v1 == v2 then
        Task.succeed v1

    else
        Task.fail Exit.DetailsHandEditedElmDependencies



-- FORK


fork : Task Never a -> Task Never (MVar a)
fork work =
    MVar.newEmptyMVar
        |> Task.andThen
            (\mvar ->
                Process.spawn (Task.andThen (MVar.putMVar mvar) work)
                    |> Task.map (\_ -> mvar)
            )



-- VERIFY DEPENDENCIES


verifyDependencies : Env -> File.Time -> ValidOutline -> Dict ( String, String ) Pkg.Name Solver.Details -> Dict ( String, String ) Pkg.Name a -> Task Exit.Details Details
verifyDependencies ((Env key scope root cache _ _ _) as env) time outline solution directDeps =
    Task.eio identity
        (Reporting.report key (Reporting.DStart (Dict.size solution))
            |> Task.andThen (\_ -> MVar.newEmptyMVar)
            |> Task.andThen
                (\mvar ->
                    Stuff.withRegistryLock cache
                        (Utils.mapTraverseWithKey identity Pkg.compareName (\k v -> fork (verifyDep env mvar solution k v)) solution)
                        |> Task.andThen
                            (\mvars ->
                                MVar.putMVar mvar mvars
                                    |> Task.andThen
                                        (\_ ->
                                            Utils.mapTraverse identity Pkg.compareName MVar.readMVar mvars
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


addObjects : Artifacts -> Opt.GlobalGraph -> Opt.GlobalGraph
addObjects (Artifacts _ objs) graph =
    Opt.addGlobalGraph objs graph


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


type Artifacts
    = Artifacts (Dict String ModuleName.Raw I.DependencyInterface) Opt.GlobalGraph


type alias Dep =
    Result (Maybe Exit.DetailsBadDep) Artifacts


verifyDep : Env -> MVar (Dict ( String, String ) Pkg.Name (MVar Dep)) -> Dict ( String, String ) Pkg.Name Solver.Details -> Pkg.Name -> Solver.Details -> Task Never Dep
verifyDep ((Env key _ root cache manager _ _) as env) depsMVar solution pkg ((Solver.Details vsn directDeps) as details) =
    let
        fingerprint : Dict ( String, String ) Pkg.Name V.Version
        fingerprint =
            Utils.mapIntersectionWith identity Pkg.compareName (\(Solver.Details v _) _ -> v) solution directDeps
    in
    Utils.dirDoesDirectoryExist (Stuff.package cache pkg vsn ++ "/src")
        |> Task.andThen
            (\exists ->
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


type ArtifactCache
    = ArtifactCache (EverySet (List ( ( String, String ), ( Int, Int, Int ) )) Fingerprint) Artifacts


type alias Fingerprint =
    Dict ( String, String ) Pkg.Name V.Version


toComparableFingerprint : Fingerprint -> List ( ( String, String ), ( Int, Int, Int ) )
toComparableFingerprint fingerprint =
    Dict.toList compare fingerprint
        |> List.map (Tuple.mapSecond V.toComparable)



-- BUILD


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
                                    MVar.readMVar depsMVar
                                        |> Task.andThen
                                            (\allDeps ->
                                                Utils.mapTraverse identity Pkg.compareName MVar.readMVar (Dict.intersection compare allDeps (Pkg.sanitizeElmDeps target deps))
                                                    |> Task.andThen
                                                        (\directDeps ->
                                                            case Utils.sequenceDictResult identity Pkg.compareName directDeps of
                                                                Err _ ->
                                                                    Reporting.report key Reporting.DBroken
                                                                        |> Task.map (\_ -> Err Nothing)

                                                                Ok directArtifacts ->
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
                                                                    getDocsStatus cache pkg vsn
                                                                        |> Task.andThen
                                                                            (\docsStatus ->
                                                                                MVar.newEmptyMVar
                                                                                    |> Task.andThen
                                                                                        (\mvar ->
                                                                                            Utils.mapTraverseWithKey identity compare (always << fork << crawlModule target root foreignDeps mvar pkg src docsStatus) exposedDict
                                                                                                |> Task.andThen
                                                                                                    (\mvars ->
                                                                                                        MVar.putMVar mvar mvars
                                                                                                            |> Task.andThen (\_ -> Utils.dictMapM_ compare MVar.readMVar mvars)
                                                                                                            |> Task.andThen (\_ -> Task.andThen (Utils.mapTraverse identity compare MVar.readMVar) (MVar.readMVar mvar))
                                                                                                            |> Task.andThen
                                                                                                                (\maybeStatuses ->
                                                                                                                    case Utils.sequenceDictMaybe identity compare maybeStatuses of
                                                                                                                        Nothing ->
                                                                                                                            Reporting.report key Reporting.DBroken
                                                                                                                                |> Task.map (\_ -> Err (Just (Exit.BD_BadBuild target pkg vsn f)))

                                                                                                                        Just statuses ->
                                                                                                                            MVar.newEmptyMVar
                                                                                                                                |> Task.andThen
                                                                                                                                    (\rmvar ->
                                                                                                                                        Utils.mapTraverse identity compare (fork << compile target root pkg rmvar) statuses
                                                                                                                                            |> Task.andThen
                                                                                                                                                (\rmvars ->
                                                                                                                                                    MVar.putMVar rmvar rmvars
                                                                                                                                                        |> Task.andThen (\_ -> Utils.mapTraverse identity compare MVar.readMVar rmvars)
                                                                                                                                                        |> Task.andThen
                                                                                                                                                            (\maybeResults ->
                                                                                                                                                                case Utils.sequenceDictMaybe identity compare maybeResults of
                                                                                                                                                                    Nothing ->
                                                                                                                                                                        Reporting.report key Reporting.DBroken
                                                                                                                                                                            |> Task.map (\_ -> Err (Just (Exit.BD_BadBuild target pkg vsn f)))

                                                                                                                                                                    Just results ->
                                                                                                                                                                        let
                                                                                                                                                                            path : String
                                                                                                                                                                            path =
                                                                                                                                                                                Stuff.package cache pkg vsn ++ "/artifacts.dat"

                                                                                                                                                                            ifaces : Dict String ModuleName.Raw I.DependencyInterface
                                                                                                                                                                            ifaces =
                                                                                                                                                                                gatherInterfaces exposedDict results

                                                                                                                                                                            objects : Opt.GlobalGraph
                                                                                                                                                                            objects =
                                                                                                                                                                                gatherObjects target results

                                                                                                                                                                            artifacts : Artifacts
                                                                                                                                                                            artifacts =
                                                                                                                                                                                Artifacts ifaces objects

                                                                                                                                                                            fingerprints : EverySet (List ( ( String, String ), ( Int, Int, Int ) )) Fingerprint
                                                                                                                                                                            fingerprints =
                                                                                                                                                                                EverySet.insert toComparableFingerprint f fs
                                                                                                                                                                        in
                                                                                                                                                                        writeDocs cache pkg vsn docsStatus results
                                                                                                                                                                            |> Task.andThen (\_ -> File.writeBinary artifactCacheEncoder path (ArtifactCache fingerprints artifacts))
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


gatherObjects : Target -> Dict String ModuleName.Raw DResult -> Opt.GlobalGraph
gatherObjects target results =
    Dict.foldr compare (addLocalGraph target) Opt.empty results


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


type ForeignInterface
    = ForeignAmbiguous
    | ForeignSpecific I.Interface


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


type alias StatusDict =
    Dict String ModuleName.Raw (MVar (Maybe Status))


type Status
    = SLocal DocsStatus (Dict String ModuleName.Raw ()) Src.Module
    | SForeign I.Interface
    | SKernelLocal (List Kernel.Chunk)
    | SKernelForeign


crawlModule : Target -> Stuff.Root -> Dict String ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> ModuleName.Raw -> Task Never (Maybe Status)
crawlModule target root foreignDeps mvar pkg src docsStatus name =
    let
        path : String -> FilePath
        path extension =
            Utils.fpCombine src (Utils.fpAddExtension (ModuleName.toFilePath name) extension)

        guidaPath : FilePath
        guidaPath =
            path "guida"

        elmPath : FilePath
        elmPath =
            path "elm"
    in
    File.exists guidaPath
        |> Task.andThen
            (\guidaExists ->
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


crawlImports : Target -> Stuff.Root -> Dict String ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> List Src.Import -> Task Never (Dict String ModuleName.Raw ())
crawlImports target root foreignDeps mvar pkg src imports =
    MVar.takeMVar mvar
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
                Utils.mapTraverseWithKey identity compare (always << fork << crawlModule target root foreignDeps mvar pkg src DocsNotNeeded) news
                    |> Task.andThen
                        (\mvars ->
                            MVar.putMVar mvar (Dict.union mvars statusDict)
                                |> Task.andThen (\_ -> Utils.dictMapM_ compare MVar.readMVar mvars)
                                |> Task.map (\_ -> deps)
                        )
            )


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


getDepHome : ForeignInterface -> Maybe Pkg.Name
getDepHome fi =
    case fi of
        ForeignSpecific (I.Interface pkg _ _ _ _) ->
            Just pkg

        ForeignAmbiguous ->
            Nothing



-- COMPILE


type DResult
    = RLocal I.Interface Opt.LocalGraph (Maybe Docs.Module)
    | RForeign I.Interface
    | RKernelLocal (List Kernel.Chunk)
    | RKernelForeign


compile : Target -> Stuff.Root -> Pkg.Name -> MVar (Dict String ModuleName.Raw (MVar (Maybe DResult))) -> Status -> Task Never (Maybe DResult)
compile target root pkg mvar status =
    case status of
        SLocal docsStatus deps modul ->
            MVar.readMVar mvar
                |> Task.andThen
                    (\resultsDict ->
                        Utils.mapTraverse identity compare MVar.readMVar (Dict.intersection compare resultsDict deps)
                            |> Task.andThen
                                (\maybeResults ->
                                    case Utils.sequenceDictMaybe identity compare maybeResults of
                                        Just results ->
                                            Compile.compile target root pkg (Utils.mapMapMaybe identity compare getInterface results) modul
                                                |> Task.map
                                                    (\result ->
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


type DocsStatus
    = DocsNeeded
    | DocsNotNeeded


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


writeDocs : Stuff.PackageCache -> Pkg.Name -> V.Version -> DocsStatus -> Dict String ModuleName.Raw DResult -> Task Never ()
writeDocs cache pkg vsn status results =
    case status of
        DocsNeeded ->
            E.writeUgly (Stuff.package cache pkg vsn ++ "/docs.json")
                (Docs.encode (Utils.mapMapMaybe identity compare toDocs results))

        DocsNotNeeded ->
            Task.succeed ()


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
