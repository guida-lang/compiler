module Builder.Elm.Details exposing
    ( BED_BuildID
    , BED_Local(..)
    , Details(..)
    , Extras
    , Foreign(..)
    , Interfaces
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
import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Http as Http
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Source as Src
import Compiler.Compile as Compile
import Compiler.Data.Name as Name
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Elm.Constraint as Con
import Compiler.Elm.Docs as Docs
import Compiler.Elm.Interface as I
import Compiler.Elm.Kernel as Kernel
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Module as Parse
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO exposing (IO)
import Types as T
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- DETAILS


type Details
    = Details File.BF_Time ValidOutline BED_BuildID (Dict String T.CEMN_Raw BED_Local) (Dict String T.CEMN_Raw Foreign) Extras


type alias BED_BuildID =
    Int


type ValidOutline
    = ValidApp (NE.Nonempty Outline.SrcDir)
    | ValidPkg T.CEP_Name (List T.CEMN_Raw) (Dict ( String, String ) T.CEP_Name V.Version {- for docs in reactor -})



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


type BED_Local
    = BED_Local T.FilePath File.BF_Time (List T.CEMN_Raw) Bool BED_BuildID BED_BuildID


type Foreign
    = Foreign T.CEP_Name (List T.CEP_Name)


type Extras
    = ArtifactsCached
    | ArtifactsFresh Interfaces T.CASTO_GlobalGraph


type alias Interfaces =
    Dict (List String) T.CEMN_Canonical I.DependencyInterface



-- LOAD ARTIFACTS


loadObjects : T.FilePath -> Details -> IO (T.MVar (Maybe T.CASTO_GlobalGraph))
loadObjects root (Details _ _ _ _ _ extras) =
    case extras of
        ArtifactsFresh _ o ->
            Utils.newMVar_Maybe_CASTO_GlobalGraph (Just o)

        ArtifactsCached ->
            fork_Maybe_CASTO_GlobalGraph (File.readBinary Opt.globalGraphDecoder (Stuff.objects root))


loadInterfaces : T.FilePath -> Details -> IO (T.MVar (Maybe Interfaces))
loadInterfaces root (Details _ _ _ _ _ extras) =
    case extras of
        ArtifactsFresh i _ ->
            Utils.newMVar (Utils.maybeEncoder interfacesEncoder) (Just i)

        ArtifactsCached ->
            fork (Utils.maybeEncoder interfacesEncoder) (File.readBinary interfacesDecoder (Stuff.interfaces root))



-- VERIFY INSTALL -- used by Install


verifyInstall : BW.Scope -> T.FilePath -> Solver.Env -> Outline.Outline -> IO (Result Exit.Details ())
verifyInstall scope root (Solver.Env cache manager connection registry) outline =
    File.getTime (root ++ "/elm.json")
        |> IO.bind
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
                        Task.run (Task.fmap (\_ -> ()) (verifyPkg env time pkg))

                    Outline.App app ->
                        Task.run (Task.fmap (\_ -> ()) (verifyApp env time app))
            )



-- LOAD -- used by Make, Repl, Reactor


load : Reporting.Style -> BW.Scope -> T.FilePath -> IO (Result Exit.Details Details)
load style scope root =
    File.getTime (root ++ "/elm.json")
        |> IO.bind
            (\newTime ->
                File.readBinary detailsDecoder (Stuff.details root)
                    |> IO.bind
                        (\maybeDetails ->
                            case maybeDetails of
                                Nothing ->
                                    generate style scope root newTime

                                Just (Details oldTime outline buildID locals foreigns extras) ->
                                    if oldTime == newTime then
                                        IO.pure (Ok (Details oldTime outline (buildID + 1) locals foreigns extras))

                                    else
                                        generate style scope root newTime
                        )
            )



-- GENERATE


generate : Reporting.Style -> BW.Scope -> T.FilePath -> File.BF_Time -> IO (Result Exit.Details Details)
generate style scope root time =
    Reporting.trackDetails style
        (\key ->
            initEnv key scope root
                |> IO.bind
                    (\result ->
                        case result of
                            Err exit ->
                                IO.pure (Err exit)

                            Ok ( env, outline ) ->
                                case outline of
                                    Outline.Pkg pkg ->
                                        Task.run (verifyPkg env time pkg)

                                    Outline.App app ->
                                        Task.run (verifyApp env time app)
                    )
        )



-- ENV


type Env
    = Env Reporting.DKey BW.Scope T.FilePath Stuff.PackageCache Http.Manager Solver.Connection Registry.Registry


initEnv : Reporting.DKey -> BW.Scope -> T.FilePath -> IO (Result Exit.Details ( Env, Outline.Outline ))
initEnv key scope root =
    fork resultRegistryProblemEnvEncoder Solver.initEnv
        |> IO.bind
            (\mvar ->
                Outline.read root
                    |> IO.bind
                        (\eitherOutline ->
                            case eitherOutline of
                                Err problem ->
                                    IO.pure (Err (Exit.DetailsBadOutline problem))

                                Ok outline ->
                                    Utils.readMVar resultRegistryProblemEnvDecoder mvar
                                        |> IO.fmap
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


type alias Task a =
    Task.Task Exit.Details a


verifyPkg : Env -> File.BF_Time -> Outline.PkgOutline -> Task Details
verifyPkg env time (Outline.PkgOutline pkg _ _ _ exposed direct testDirect elm) =
    if Con.goodElm elm then
        union identity Pkg.compareName noDups direct testDirect
            |> Task.bind (verifyConstraints env)
            |> Task.bind
                (\solution ->
                    let
                        exposedList : List T.CEMN_Raw
                        exposedList =
                            Outline.flattenExposed exposed

                        exactDeps : Dict ( String, String ) T.CEP_Name V.Version
                        exactDeps =
                            Dict.map (\_ (Solver.Details v _) -> v) solution

                        -- for pkg docs in reactor
                    in
                    verifyDependencies env time (ValidPkg pkg exposedList exactDeps) solution direct
                )

    else
        Task.throw (Exit.DetailsBadElmInPkg elm)


verifyApp : Env -> File.BF_Time -> Outline.AppOutline -> Task Details
verifyApp env time ((Outline.AppOutline elmVersion srcDirs direct _ _ _) as outline) =
    if elmVersion == V.compiler then
        checkAppDeps outline
            |> Task.bind
                (\stated ->
                    verifyConstraints env (Dict.map (\_ -> Con.exactly) stated)
                        |> Task.bind
                            (\actual ->
                                if Dict.size stated == Dict.size actual then
                                    verifyDependencies env time (ValidApp srcDirs) actual direct

                                else
                                    Task.throw Exit.DetailsHandEditedDependencies
                            )
                )

    else
        Task.throw (Exit.DetailsBadElmInAppOutline elmVersion)


checkAppDeps : Outline.AppOutline -> Task (Dict ( String, String ) T.CEP_Name V.Version)
checkAppDeps (Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
    union identity Pkg.compareName allowEqualDups indirect testDirect
        |> Task.bind
            (\x ->
                union identity Pkg.compareName noDups direct testIndirect
                    |> Task.bind (\y -> union identity Pkg.compareName noDups x y)
            )



-- VERIFY CONSTRAINTS


verifyConstraints : Env -> Dict ( String, String ) T.CEP_Name Con.Constraint -> Task (Dict ( String, String ) T.CEP_Name Solver.Details)
verifyConstraints (Env _ _ _ cache _ connection registry) constraints =
    Task.io (Solver.verify cache connection registry constraints)
        |> Task.bind
            (\result ->
                case result of
                    Solver.SolverOk details ->
                        Task.pure details

                    Solver.NoSolution ->
                        Task.throw Exit.DetailsNoSolution

                    Solver.NoOfflineSolution ->
                        Task.throw Exit.DetailsNoOfflineSolution

                    Solver.SolverErr exit ->
                        Task.throw (Exit.DetailsSolverProblem exit)
            )



-- UNION


union : (k -> comparable) -> (k -> k -> Order) -> (k -> v -> v -> Task v) -> Dict comparable k v -> Dict comparable k v -> Task (Dict comparable k v)
union toComparable keyComparison tieBreaker deps1 deps2 =
    Dict.merge keyComparison
        (\k dep -> Task.fmap (Dict.insert toComparable k dep))
        (\k dep1 dep2 acc ->
            tieBreaker k dep1 dep2
                |> Task.bind (\v -> Task.fmap (Dict.insert toComparable k v) acc)
        )
        (\k dep -> Task.fmap (Dict.insert toComparable k dep))
        deps1
        deps2
        (Task.pure Dict.empty)


noDups : k -> v -> v -> Task v
noDups _ _ _ =
    Task.throw Exit.DetailsHandEditedDependencies


allowEqualDups : k -> v -> v -> Task v
allowEqualDups _ v1 v2 =
    if v1 == v2 then
        Task.pure v1

    else
        Task.throw Exit.DetailsHandEditedDependencies



-- FORK


fork : (a -> Encode.Value) -> IO a -> IO (T.MVar a)
fork encoder work =
    Utils.newEmptyMVar
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar encoder mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )


fork_Maybe_BED_Status : IO (Maybe T.BED_Status) -> IO (T.MVar (Maybe T.BED_Status))
fork_Maybe_BED_Status work =
    Utils.newEmptyMVar_Maybe_BED_Status
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar_Maybe_BED_Status mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )


fork_Maybe_CASTO_GlobalGraph : IO (Maybe T.CASTO_GlobalGraph) -> IO (T.MVar (Maybe T.CASTO_GlobalGraph))
fork_Maybe_CASTO_GlobalGraph work =
    Utils.newEmptyMVar_Maybe_CASTO_GlobalGraph
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar_Maybe_CASTO_GlobalGraph mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )



-- VERIFY DEPENDENCIES


verifyDependencies : Env -> File.BF_Time -> ValidOutline -> Dict ( String, String ) T.CEP_Name Solver.Details -> Dict ( String, String ) T.CEP_Name a -> Task Details
verifyDependencies ((Env key scope root cache _ _ _) as env) time outline solution directDeps =
    Task.eio identity
        (Reporting.report key (Reporting.DStart (Dict.size solution))
            |> IO.bind (\_ -> Utils.newEmptyMVar)
            |> IO.bind
                (\mvar ->
                    Stuff.withRegistryLock cache
                        (Utils.mapTraverseWithKey identity Pkg.compareName (\k v -> fork depEncoder (verifyDep env mvar solution k v)) solution)
                        |> IO.bind
                            (\mvars ->
                                Utils.putMVar dictNameMVarDepEncoder mvar mvars
                                    |> IO.bind
                                        (\_ ->
                                            Utils.mapTraverse identity Pkg.compareName (Utils.readMVar depDecoder) mvars
                                                |> IO.bind
                                                    (\deps ->
                                                        case Utils.sequenceDictResult identity Pkg.compareName deps of
                                                            Err _ ->
                                                                Stuff.getElmHome
                                                                    |> IO.fmap
                                                                        (\home ->
                                                                            Err
                                                                                (Exit.DetailsBadDeps home
                                                                                    (List.filterMap identity (Utils.eitherLefts (Dict.values compare deps)))
                                                                                )
                                                                        )

                                                            Ok artifacts ->
                                                                let
                                                                    objs : T.CASTO_GlobalGraph
                                                                    objs =
                                                                        Dict.foldr compare (\_ -> addObjects) Opt.empty artifacts

                                                                    ifaces : Interfaces
                                                                    ifaces =
                                                                        Dict.foldr compare (addInterfaces directDeps) Dict.empty artifacts

                                                                    foreigns : Dict String T.CEMN_Raw Foreign
                                                                    foreigns =
                                                                        Dict.map (\_ -> OneOrMore.destruct Foreign) (Dict.foldr compare gatherForeigns Dict.empty (Dict.intersection compare artifacts directDeps))

                                                                    details : Details
                                                                    details =
                                                                        Details time outline 0 Dict.empty foreigns (ArtifactsFresh ifaces objs)
                                                                in
                                                                BW.writeBinary Opt.globalGraphEncoder scope (Stuff.objects root) objs
                                                                    |> IO.bind (\_ -> BW.writeBinary interfacesEncoder scope (Stuff.interfaces root) ifaces)
                                                                    |> IO.bind (\_ -> BW.writeBinary detailsEncoder scope (Stuff.details root) details)
                                                                    |> IO.fmap (\_ -> Ok details)
                                                    )
                                        )
                            )
                )
        )


addObjects : Artifacts -> T.CASTO_GlobalGraph -> T.CASTO_GlobalGraph
addObjects (Artifacts _ objs) graph =
    Opt.addGlobalGraph objs graph


addInterfaces : Dict ( String, String ) T.CEP_Name a -> T.CEP_Name -> Artifacts -> Interfaces -> Interfaces
addInterfaces directDeps pkg (Artifacts ifaces _) dependencyInterfaces =
    Dict.union
        dependencyInterfaces
        (Dict.fromList ModuleName.toComparableCanonical
            (List.map (Tuple.mapFirst (T.CEMN_Canonical pkg))
                (Dict.toList compare
                    (if Dict.member identity pkg directDeps then
                        ifaces

                     else
                        Dict.map (\_ -> I.privatize) ifaces
                    )
                )
            )
        )


gatherForeigns : T.CEP_Name -> Artifacts -> Dict String T.CEMN_Raw (OneOrMore.OneOrMore T.CEP_Name) -> Dict String T.CEMN_Raw (OneOrMore.OneOrMore T.CEP_Name)
gatherForeigns pkg (Artifacts ifaces _) foreigns =
    let
        isPublic : I.DependencyInterface -> Maybe (OneOrMore.OneOrMore T.CEP_Name)
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
    = Artifacts (Dict String T.CEMN_Raw I.DependencyInterface) T.CASTO_GlobalGraph


type alias Dep =
    Result (Maybe Exit.DetailsBadDep) Artifacts


verifyDep : Env -> T.MVar (Dict ( String, String ) T.CEP_Name (T.MVar Dep)) -> Dict ( String, String ) T.CEP_Name Solver.Details -> T.CEP_Name -> Solver.Details -> IO Dep
verifyDep (Env key _ _ cache manager _ _) depsMVar solution pkg ((Solver.Details vsn directDeps) as details) =
    let
        fingerprint : Dict ( String, String ) T.CEP_Name V.Version
        fingerprint =
            Utils.mapIntersectionWith identity Pkg.compareName (\(Solver.Details v _) _ -> v) solution directDeps
    in
    Utils.dirDoesDirectoryExist (Stuff.package cache pkg vsn ++ "/src")
        |> IO.bind
            (\exists ->
                if exists then
                    Reporting.report key Reporting.DCached
                        |> IO.bind
                            (\_ ->
                                File.readBinary artifactCacheDecoder (Stuff.package cache pkg vsn ++ "/artifacts.json")
                                    |> IO.bind
                                        (\maybeCache ->
                                            case maybeCache of
                                                Nothing ->
                                                    build key cache depsMVar pkg details fingerprint EverySet.empty

                                                Just (ArtifactCache fingerprints artifacts) ->
                                                    if EverySet.member toComparableFingerprint fingerprint fingerprints then
                                                        IO.fmap (\_ -> Ok artifacts) (Reporting.report key Reporting.DBuilt)

                                                    else
                                                        build key cache depsMVar pkg details fingerprint fingerprints
                                        )
                            )

                else
                    Reporting.report key Reporting.DRequested
                        |> IO.bind
                            (\_ ->
                                downloadPackage cache manager pkg vsn
                                    |> IO.bind
                                        (\result ->
                                            case result of
                                                Err problem ->
                                                    Reporting.report key (Reporting.DFailed pkg vsn)
                                                        |> IO.fmap (\_ -> Err (Just (Exit.BD_BadDownload pkg vsn problem)))

                                                Ok () ->
                                                    Reporting.report key (Reporting.DReceived pkg vsn)
                                                        |> IO.bind (\_ -> build key cache depsMVar pkg details fingerprint EverySet.empty)
                                        )
                            )
            )



-- ARTIFACT CACHE


type ArtifactCache
    = ArtifactCache (EverySet (List ( ( String, String ), ( Int, Int, Int ) )) Fingerprint) Artifacts


type alias Fingerprint =
    Dict ( String, String ) T.CEP_Name V.Version


toComparableFingerprint : Fingerprint -> List ( ( String, String ), ( Int, Int, Int ) )
toComparableFingerprint fingerprint =
    Dict.toList compare fingerprint
        |> List.map (Tuple.mapSecond V.toComparable)



-- BUILD


build : Reporting.DKey -> Stuff.PackageCache -> T.MVar (Dict ( String, String ) T.CEP_Name (T.MVar Dep)) -> T.CEP_Name -> Solver.Details -> Fingerprint -> EverySet (List ( ( String, String ), ( Int, Int, Int ) )) Fingerprint -> IO Dep
build key cache depsMVar pkg (Solver.Details vsn _) f fs =
    Outline.read (Stuff.package cache pkg vsn)
        |> IO.bind
            (\eitherOutline ->
                case eitherOutline of
                    Err _ ->
                        Reporting.report key Reporting.DBroken
                            |> IO.fmap (\_ -> Err (Just (Exit.BD_BadBuild pkg vsn f)))

                    Ok (Outline.App _) ->
                        Reporting.report key Reporting.DBroken
                            |> IO.fmap (\_ -> Err (Just (Exit.BD_BadBuild pkg vsn f)))

                    Ok (Outline.Pkg (Outline.PkgOutline _ _ _ _ exposed deps _ _)) ->
                        Utils.readMVar dictPkgNameMVarDepDecoder depsMVar
                            |> IO.bind
                                (\allDeps ->
                                    Utils.mapTraverse identity Pkg.compareName (Utils.readMVar depDecoder) (Dict.intersection compare allDeps deps)
                                        |> IO.bind
                                            (\directDeps ->
                                                case Utils.sequenceDictResult identity Pkg.compareName directDeps of
                                                    Err _ ->
                                                        Reporting.report key Reporting.DBroken
                                                            |> IO.fmap (\_ -> Err Nothing)

                                                    Ok directArtifacts ->
                                                        let
                                                            src : String
                                                            src =
                                                                Stuff.package cache pkg vsn ++ "/src"

                                                            foreignDeps : Dict String T.CEMN_Raw ForeignInterface
                                                            foreignDeps =
                                                                gatherForeignInterfaces directArtifacts

                                                            exposedDict : Dict String T.CEMN_Raw ()
                                                            exposedDict =
                                                                Utils.mapFromKeys identity (\_ -> ()) (Outline.flattenExposed exposed)
                                                        in
                                                        getDocsStatus cache pkg vsn
                                                            |> IO.bind
                                                                (\docsStatus ->
                                                                    Utils.newEmptyMVar
                                                                        |> IO.bind
                                                                            (\mvar ->
                                                                                Utils.mapTraverseWithKey identity compare (always << fork_Maybe_BED_Status << crawlModule foreignDeps mvar pkg src docsStatus) exposedDict
                                                                                    |> IO.bind
                                                                                        (\mvars ->
                                                                                            Utils.putMVar statusDictEncoder mvar mvars
                                                                                                |> IO.bind (\_ -> Utils.dictMapM_ compare Utils.readMVar_Maybe_BED_Status mvars)
                                                                                                |> IO.bind (\_ -> IO.bind (Utils.mapTraverse identity compare Utils.readMVar_Maybe_BED_Status) (Utils.readMVar statusDictDecoder mvar))
                                                                                                |> IO.bind
                                                                                                    (\maybeStatuses ->
                                                                                                        case Utils.sequenceDictMaybe identity compare maybeStatuses of
                                                                                                            Nothing ->
                                                                                                                Reporting.report key Reporting.DBroken
                                                                                                                    |> IO.fmap (\_ -> Err (Just (Exit.BD_BadBuild pkg vsn f)))

                                                                                                            Just statuses ->
                                                                                                                Utils.newEmptyMVar
                                                                                                                    |> IO.bind
                                                                                                                        (\rmvar ->
                                                                                                                            Utils.mapTraverse identity compare (fork (E.maybe dResultEncoder) << compile pkg rmvar) statuses
                                                                                                                                |> IO.bind
                                                                                                                                    (\rmvars ->
                                                                                                                                        Utils.putMVar dictRawMVarMaybeDResultEncoder rmvar rmvars
                                                                                                                                            |> IO.bind (\_ -> Utils.mapTraverse identity compare (Utils.readMVar (Decode.maybe dResultDecoder)) rmvars)
                                                                                                                                            |> IO.bind
                                                                                                                                                (\maybeResults ->
                                                                                                                                                    case Utils.sequenceDictMaybe identity compare maybeResults of
                                                                                                                                                        Nothing ->
                                                                                                                                                            Reporting.report key Reporting.DBroken
                                                                                                                                                                |> IO.fmap (\_ -> Err (Just (Exit.BD_BadBuild pkg vsn f)))

                                                                                                                                                        Just results ->
                                                                                                                                                            let
                                                                                                                                                                path : String
                                                                                                                                                                path =
                                                                                                                                                                    Stuff.package cache pkg vsn ++ "/artifacts.json"

                                                                                                                                                                ifaces : Dict String T.CEMN_Raw I.DependencyInterface
                                                                                                                                                                ifaces =
                                                                                                                                                                    gatherInterfaces exposedDict results

                                                                                                                                                                objects : T.CASTO_GlobalGraph
                                                                                                                                                                objects =
                                                                                                                                                                    gatherObjects results

                                                                                                                                                                artifacts : Artifacts
                                                                                                                                                                artifacts =
                                                                                                                                                                    Artifacts ifaces objects

                                                                                                                                                                fingerprints : EverySet (List ( ( String, String ), ( Int, Int, Int ) )) Fingerprint
                                                                                                                                                                fingerprints =
                                                                                                                                                                    EverySet.insert toComparableFingerprint f fs
                                                                                                                                                            in
                                                                                                                                                            writeDocs cache pkg vsn docsStatus results
                                                                                                                                                                |> IO.bind (\_ -> File.writeBinary artifactCacheEncoder path (ArtifactCache fingerprints artifacts))
                                                                                                                                                                |> IO.bind (\_ -> Reporting.report key Reporting.DBuilt)
                                                                                                                                                                |> IO.fmap (\_ -> Ok artifacts)
                                                                                                                                                )
                                                                                                                                    )
                                                                                                                        )
                                                                                                    )
                                                                                        )
                                                                            )
                                                                )
                                            )
                                )
            )



-- GATHER


gatherObjects : Dict String T.CEMN_Raw DResult -> T.CASTO_GlobalGraph
gatherObjects results =
    Dict.foldr compare addLocalGraph Opt.empty results


addLocalGraph : T.CEMN_Raw -> DResult -> T.CASTO_GlobalGraph -> T.CASTO_GlobalGraph
addLocalGraph name status graph =
    case status of
        RLocal _ objs _ ->
            Opt.addLocalGraph objs graph

        RForeign _ ->
            graph

        RKernelLocal cs ->
            Opt.addKernel (Name.getKernel name) cs graph

        RKernelForeign ->
            graph


gatherInterfaces : Dict String T.CEMN_Raw () -> Dict String T.CEMN_Raw DResult -> Dict String T.CEMN_Raw I.DependencyInterface
gatherInterfaces exposed artifacts =
    let
        onLeft : a -> b -> c -> d
        onLeft _ _ _ =
            crash "compiler bug manifesting in Elm.Details.gatherInterfaces"

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


toLocalInterface : (T.CEI_Interface -> a) -> DResult -> Maybe a
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
    | ForeignSpecific T.CEI_Interface


gatherForeignInterfaces : Dict ( String, String ) T.CEP_Name Artifacts -> Dict String T.CEMN_Raw ForeignInterface
gatherForeignInterfaces directArtifacts =
    let
        finalize : T.CEI_Interface -> List T.CEI_Interface -> ForeignInterface
        finalize i is =
            case is of
                [] ->
                    ForeignSpecific i

                _ :: _ ->
                    ForeignAmbiguous

        gather : T.CEP_Name -> Artifacts -> Dict String T.CEMN_Raw (OneOrMore.OneOrMore T.CEI_Interface) -> Dict String T.CEMN_Raw (OneOrMore.OneOrMore T.CEI_Interface)
        gather _ (Artifacts ifaces _) buckets =
            Utils.mapUnionWith identity compare OneOrMore.more buckets (Utils.mapMapMaybe identity compare isPublic ifaces)

        isPublic : I.DependencyInterface -> Maybe (OneOrMore.OneOrMore T.CEI_Interface)
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


crawlModule : Dict String T.CEMN_Raw ForeignInterface -> T.MVar T.BED_StatusDict -> T.CEP_Name -> T.FilePath -> T.BED_DocsStatus -> T.CEMN_Raw -> IO (Maybe T.BED_Status)
crawlModule foreignDeps mvar pkg src docsStatus name =
    let
        path : T.FilePath
        path =
            Utils.fpForwardSlash src (Utils.fpAddExtension (ModuleName.toFilePath name) "elm")
    in
    File.exists path
        |> IO.bind
            (\exists ->
                case Dict.get identity name foreignDeps of
                    Just ForeignAmbiguous ->
                        IO.pure Nothing

                    Just (ForeignSpecific iface) ->
                        if exists then
                            IO.pure Nothing

                        else
                            IO.pure (Just (T.BED_SForeign iface))

                    Nothing ->
                        if exists then
                            crawlFile foreignDeps mvar pkg src docsStatus name path

                        else if Pkg.isKernel pkg && Name.isKernel name then
                            crawlKernel foreignDeps mvar pkg src name

                        else
                            IO.pure Nothing
            )


crawlFile : Dict String T.CEMN_Raw ForeignInterface -> T.MVar T.BED_StatusDict -> T.CEP_Name -> T.FilePath -> T.BED_DocsStatus -> T.CEMN_Raw -> T.FilePath -> IO (Maybe T.BED_Status)
crawlFile foreignDeps mvar pkg src docsStatus expectedName path =
    File.readUtf8 path
        |> IO.bind
            (\bytes ->
                case Parse.fromByteString (Parse.Package pkg) bytes of
                    Ok ((T.CASTS_Module (Just (T.CRA_At _ actualName)) _ _ imports _ _ _ _ _) as modul) ->
                        if expectedName == actualName then
                            crawlImports foreignDeps mvar pkg src imports
                                |> IO.fmap (\deps -> Just (T.BED_SLocal docsStatus deps modul))

                        else
                            IO.pure Nothing

                    _ ->
                        IO.pure Nothing
            )


crawlImports : Dict String T.CEMN_Raw ForeignInterface -> T.MVar T.BED_StatusDict -> T.CEP_Name -> T.FilePath -> List T.CASTS_Import -> IO (Dict String T.CEMN_Raw ())
crawlImports foreignDeps mvar pkg src imports =
    Utils.takeMVar statusDictDecoder mvar
        |> IO.bind
            (\statusDict ->
                let
                    deps : Dict String T.CDN_Name ()
                    deps =
                        Dict.fromList identity (List.map (\i -> ( Src.getImportName i, () )) imports)

                    news : Dict String T.CDN_Name ()
                    news =
                        Dict.diff deps statusDict
                in
                Utils.mapTraverseWithKey identity compare (always << fork_Maybe_BED_Status << crawlModule foreignDeps mvar pkg src T.BED_DocsNotNeeded) news
                    |> IO.bind
                        (\mvars ->
                            Utils.putMVar statusDictEncoder mvar (Dict.union mvars statusDict)
                                |> IO.bind (\_ -> Utils.dictMapM_ compare Utils.readMVar_Maybe_BED_Status mvars)
                                |> IO.fmap (\_ -> deps)
                        )
            )


crawlKernel : Dict String T.CEMN_Raw ForeignInterface -> T.MVar T.BED_StatusDict -> T.CEP_Name -> T.FilePath -> T.CEMN_Raw -> IO (Maybe T.BED_Status)
crawlKernel foreignDeps mvar pkg src name =
    let
        path : T.FilePath
        path =
            Utils.fpForwardSlash src (Utils.fpAddExtension (ModuleName.toFilePath name) "js")
    in
    File.exists path
        |> IO.bind
            (\exists ->
                if exists then
                    File.readUtf8 path
                        |> IO.bind
                            (\bytes ->
                                case Kernel.fromByteString pkg (Utils.mapMapMaybe identity compare getDepHome foreignDeps) bytes of
                                    Nothing ->
                                        IO.pure Nothing

                                    Just (Kernel.Content imports chunks) ->
                                        crawlImports foreignDeps mvar pkg src imports
                                            |> IO.fmap (\_ -> Just (T.BED_SKernelLocal chunks))
                            )

                else
                    IO.pure (Just T.BED_SKernelForeign)
            )


getDepHome : ForeignInterface -> Maybe T.CEP_Name
getDepHome fi =
    case fi of
        ForeignSpecific (T.CEI_Interface pkg _ _ _ _) ->
            Just pkg

        ForeignAmbiguous ->
            Nothing



-- COMPILE


type DResult
    = RLocal T.CEI_Interface T.CASTO_LocalGraph (Maybe Docs.CED_Module)
    | RForeign T.CEI_Interface
    | RKernelLocal (List T.CEK_Chunk)
    | RKernelForeign


compile : T.CEP_Name -> T.MVar (Dict String T.CEMN_Raw (T.MVar (Maybe DResult))) -> T.BED_Status -> IO (Maybe DResult)
compile pkg mvar status =
    case status of
        T.BED_SLocal docsStatus deps modul ->
            Utils.readMVar moduleNameRawMVarMaybeDResultDecoder mvar
                |> IO.bind
                    (\resultsDict ->
                        Utils.mapTraverse identity compare (Utils.readMVar (Decode.maybe dResultDecoder)) (Dict.intersection compare resultsDict deps)
                            |> IO.bind
                                (\maybeResults ->
                                    case Utils.sequenceDictMaybe identity compare maybeResults of
                                        Just results ->
                                            Compile.compile pkg (Utils.mapMapMaybe identity compare getInterface results) modul
                                                |> IO.fmap
                                                    (\result ->
                                                        case result of
                                                            Err _ ->
                                                                Nothing

                                                            Ok (Compile.Artifacts canonical annotations objects) ->
                                                                let
                                                                    ifaces : T.CEI_Interface
                                                                    ifaces =
                                                                        I.fromModule pkg canonical annotations

                                                                    docs : Maybe Docs.CED_Module
                                                                    docs =
                                                                        makeDocs docsStatus canonical
                                                                in
                                                                Just (RLocal ifaces objects docs)
                                                    )

                                        Nothing ->
                                            IO.pure Nothing
                                )
                    )

        T.BED_SForeign iface ->
            IO.pure (Just (RForeign iface))

        T.BED_SKernelLocal chunks ->
            IO.pure (Just (RKernelLocal chunks))

        T.BED_SKernelForeign ->
            IO.pure (Just RKernelForeign)


getInterface : DResult -> Maybe T.CEI_Interface
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


getDocsStatus : Stuff.PackageCache -> T.CEP_Name -> V.Version -> IO T.BED_DocsStatus
getDocsStatus cache pkg vsn =
    File.exists (Stuff.package cache pkg vsn ++ "/docs.json")
        |> IO.fmap
            (\exists ->
                if exists then
                    T.BED_DocsNotNeeded

                else
                    T.BED_DocsNeeded
            )


makeDocs : T.BED_DocsStatus -> Can.Module -> Maybe Docs.CED_Module
makeDocs status modul =
    case status of
        T.BED_DocsNeeded ->
            case Docs.fromModule modul of
                Ok docs ->
                    Just docs

                Err _ ->
                    Nothing

        T.BED_DocsNotNeeded ->
            Nothing


writeDocs : Stuff.PackageCache -> T.CEP_Name -> V.Version -> T.BED_DocsStatus -> Dict String T.CEMN_Raw DResult -> IO ()
writeDocs cache pkg vsn status results =
    case status of
        T.BED_DocsNeeded ->
            E.writeUgly (Stuff.package cache pkg vsn ++ "/docs.json")
                (Docs.encode (Utils.mapMapMaybe identity compare toDocs results))

        T.BED_DocsNotNeeded ->
            IO.pure ()


toDocs : DResult -> Maybe Docs.CED_Module
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


downloadPackage : Stuff.PackageCache -> Http.Manager -> T.CEP_Name -> V.Version -> IO (Result Exit.PackageProblem ())
downloadPackage cache manager pkg vsn =
    let
        url : String
        url =
            Website.metadata pkg vsn "endpoint.json"
    in
    Http.get manager url [] identity (IO.pure << Ok)
        |> IO.bind
            (\eitherByteString ->
                case eitherByteString of
                    Err err ->
                        IO.pure (Err (Exit.PP_BadEndpointRequest err))

                    Ok byteString ->
                        case D.fromByteString endpointDecoder byteString of
                            Err _ ->
                                IO.pure (Err (Exit.PP_BadEndpointContent url))

                            Ok ( endpoint, expectedHash ) ->
                                Http.getArchive manager endpoint Exit.PP_BadArchiveRequest (Exit.PP_BadArchiveContent endpoint) <|
                                    \( sha, archive ) ->
                                        if expectedHash == Http.shaToChars sha then
                                            IO.fmap Ok (File.writePackage (Stuff.package cache pkg vsn) archive)

                                        else
                                            IO.pure (Err (Exit.PP_BadArchiveHash endpoint expectedHash (Http.shaToChars sha)))
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


detailsEncoder : Details -> Encode.Value
detailsEncoder (Details oldTime outline buildID locals foreigns extras) =
    Encode.object
        [ ( "type", Encode.string "Details" )
        , ( "oldTime", File.timeEncoder oldTime )
        , ( "outline", validOutlineEncoder outline )
        , ( "buildID", Encode.int buildID )
        , ( "locals", E.assocListDict compare ModuleName.rawEncoder localEncoder locals )
        , ( "foreigns", E.assocListDict compare ModuleName.rawEncoder foreignEncoder foreigns )
        , ( "extras", extrasEncoder extras )
        ]


detailsDecoder : Decode.Decoder Details
detailsDecoder =
    Decode.map6 Details
        (Decode.field "oldTime" File.timeDecoder)
        (Decode.field "outline" validOutlineDecoder)
        (Decode.field "buildID" Decode.int)
        (Decode.field "locals" (D.assocListDict identity ModuleName.rawDecoder localDecoder))
        (Decode.field "foreigns" (D.assocListDict identity ModuleName.rawDecoder foreignDecoder))
        (Decode.field "extras" extrasDecoder)


interfacesEncoder : Interfaces -> Encode.Value
interfacesEncoder =
    E.assocListDict ModuleName.compareCanonical ModuleName.canonicalEncoder I.dependencyInterfaceEncoder


interfacesDecoder : Decode.Decoder Interfaces
interfacesDecoder =
    D.assocListDict ModuleName.toComparableCanonical ModuleName.canonicalDecoder I.dependencyInterfaceDecoder


resultRegistryProblemEnvEncoder : Result Exit.RegistryProblem Solver.Env -> Encode.Value
resultRegistryProblemEnvEncoder =
    E.result Exit.registryProblemEncoder Solver.envEncoder


resultRegistryProblemEnvDecoder : Decode.Decoder (Result Exit.RegistryProblem Solver.Env)
resultRegistryProblemEnvDecoder =
    D.result Exit.registryProblemDecoder Solver.envDecoder


depEncoder : Dep -> Encode.Value
depEncoder dep =
    E.result (E.maybe Exit.detailsBadDepEncoder) artifactsEncoder dep


depDecoder : Decode.Decoder Dep
depDecoder =
    D.result (Decode.maybe Exit.detailsBadDepDecoder) artifactsDecoder


artifactsEncoder : Artifacts -> Encode.Value
artifactsEncoder (Artifacts ifaces objects) =
    Encode.object
        [ ( "type", Encode.string "Artifacts" )
        , ( "ifaces", E.assocListDict compare ModuleName.rawEncoder I.dependencyInterfaceEncoder ifaces )
        , ( "objects", Opt.globalGraphEncoder objects )
        ]


artifactsDecoder : Decode.Decoder Artifacts
artifactsDecoder =
    Decode.map2 Artifacts
        (Decode.field "ifaces" (D.assocListDict identity ModuleName.rawDecoder I.dependencyInterfaceDecoder))
        (Decode.field "objects" Opt.globalGraphDecoder)


dictNameMVarDepEncoder : Dict ( String, String ) T.CEP_Name (T.MVar Dep) -> Encode.Value
dictNameMVarDepEncoder =
    E.assocListDict compare Pkg.nameEncoder Utils.mVarEncoder


artifactCacheEncoder : ArtifactCache -> Encode.Value
artifactCacheEncoder (ArtifactCache fingerprints artifacts) =
    Encode.object
        [ ( "type", Encode.string "ArtifactCache" )
        , ( "fingerprints", E.everySet (\_ _ -> EQ) fingerprintEncoder fingerprints )
        , ( "artifacts", artifactsEncoder artifacts )
        ]


artifactCacheDecoder : Decode.Decoder ArtifactCache
artifactCacheDecoder =
    Decode.map2 ArtifactCache
        (Decode.field "fingerprints" (D.everySet toComparableFingerprint fingerprintDecoder))
        (Decode.field "artifacts" artifactsDecoder)


dictPkgNameMVarDepDecoder : Decode.Decoder (Dict ( String, String ) T.CEP_Name (T.MVar Dep))
dictPkgNameMVarDepDecoder =
    D.assocListDict identity Pkg.nameDecoder Utils.mVarDecoder


dictRawMVarMaybeDResultEncoder : Dict String T.CEMN_Raw (T.MVar (Maybe DResult)) -> Encode.Value
dictRawMVarMaybeDResultEncoder =
    E.assocListDict compare ModuleName.rawEncoder Utils.mVarEncoder


moduleNameRawMVarMaybeDResultDecoder : Decode.Decoder (Dict String T.CEMN_Raw (T.MVar (Maybe DResult)))
moduleNameRawMVarMaybeDResultDecoder =
    D.assocListDict identity ModuleName.rawDecoder Utils.mVarDecoder


dResultEncoder : DResult -> Encode.Value
dResultEncoder dResult =
    case dResult of
        RLocal ifaces objects docs ->
            Encode.object
                [ ( "type", Encode.string "RLocal" )
                , ( "ifaces", I.interfaceEncoder ifaces )
                , ( "objects", Opt.localGraphEncoder objects )
                , ( "docs", E.maybe Docs.jsonModuleEncoder docs )
                ]

        RForeign iface ->
            Encode.object
                [ ( "type", Encode.string "RForeign" )
                , ( "iface", I.interfaceEncoder iface )
                ]

        RKernelLocal chunks ->
            Encode.object
                [ ( "type", Encode.string "RKernelLocal" )
                , ( "chunks", Encode.list Kernel.chunkEncoder chunks )
                ]

        RKernelForeign ->
            Encode.object
                [ ( "type", Encode.string "RKernelForeign" )
                ]


dResultDecoder : Decode.Decoder DResult
dResultDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "RLocal" ->
                        Decode.map3 RLocal
                            (Decode.field "ifaces" I.interfaceDecoder)
                            (Decode.field "objects" Opt.localGraphDecoder)
                            (Decode.field "docs" (Decode.maybe Docs.jsonModuleDecoder))

                    "RForeign" ->
                        Decode.map RForeign (Decode.field "iface" I.interfaceDecoder)

                    "RKernelLocal" ->
                        Decode.map RKernelLocal (Decode.field "chunks" (Decode.list Kernel.chunkDecoder))

                    "RKernelForeign" ->
                        Decode.succeed RKernelForeign

                    _ ->
                        Decode.fail ("Failed to decode DResult's type: " ++ type_)
            )


statusDictEncoder : T.BED_StatusDict -> Encode.Value
statusDictEncoder statusDict =
    E.assocListDict compare ModuleName.rawEncoder Utils.mVarEncoder statusDict


statusDictDecoder : Decode.Decoder T.BED_StatusDict
statusDictDecoder =
    D.assocListDict identity ModuleName.rawDecoder Utils.mVarDecoder


localEncoder : BED_Local -> Encode.Value
localEncoder (BED_Local path time deps hasMain lastChange lastCompile) =
    Encode.object
        [ ( "type", Encode.string "Local" )
        , ( "path", Encode.string path )
        , ( "time", File.timeEncoder time )
        , ( "deps", Encode.list ModuleName.rawEncoder deps )
        , ( "hasMain", Encode.bool hasMain )
        , ( "lastChange", Encode.int lastChange )
        , ( "lastCompile", Encode.int lastCompile )
        ]


localDecoder : Decode.Decoder BED_Local
localDecoder =
    Decode.map6 BED_Local
        (Decode.field "path" Decode.string)
        (Decode.field "time" File.timeDecoder)
        (Decode.field "deps" (Decode.list ModuleName.rawDecoder))
        (Decode.field "hasMain" Decode.bool)
        (Decode.field "lastChange" Decode.int)
        (Decode.field "lastCompile" Decode.int)


validOutlineEncoder : ValidOutline -> Encode.Value
validOutlineEncoder validOutline =
    case validOutline of
        ValidApp srcDirs ->
            Encode.object
                [ ( "type", Encode.string "ValidApp" )
                , ( "srcDirs", E.nonempty Outline.srcDirEncoder srcDirs )
                ]

        ValidPkg pkg exposedList exactDeps ->
            Encode.object
                [ ( "type", Encode.string "ValidPkg" )
                , ( "pkg", Pkg.nameEncoder pkg )
                , ( "exposedList", Encode.list ModuleName.rawEncoder exposedList )
                , ( "exactDeps", E.assocListDict compare Pkg.nameEncoder V.versionEncoder exactDeps )
                ]


validOutlineDecoder : Decode.Decoder ValidOutline
validOutlineDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "ValidApp" ->
                        Decode.map ValidApp (Decode.field "srcDirs" (D.nonempty Outline.srcDirDecoder))

                    "ValidPkg" ->
                        Decode.map3 ValidPkg
                            (Decode.field "pkg" Pkg.nameDecoder)
                            (Decode.field "exposedList" (Decode.list ModuleName.rawDecoder))
                            (Decode.field "exactDeps" (D.assocListDict identity Pkg.nameDecoder V.versionDecoder))

                    _ ->
                        Decode.fail ("Failed to decode ValidOutline's type: " ++ type_)
            )


foreignEncoder : Foreign -> Encode.Value
foreignEncoder (Foreign dep deps) =
    Encode.object
        [ ( "type", Encode.string "Foreign" )
        , ( "dep", Pkg.nameEncoder dep )
        , ( "deps", Encode.list Pkg.nameEncoder deps )
        ]


foreignDecoder : Decode.Decoder Foreign
foreignDecoder =
    Decode.map2 Foreign
        (Decode.field "dep" Pkg.nameDecoder)
        (Decode.field "deps" (Decode.list Pkg.nameDecoder))


extrasEncoder : Extras -> Encode.Value
extrasEncoder extras =
    case extras of
        ArtifactsCached ->
            Encode.object
                [ ( "type", Encode.string "ArtifactsCached" )
                ]

        ArtifactsFresh ifaces objs ->
            Encode.object
                [ ( "type", Encode.string "ArtifactsFresh" )
                , ( "ifaces", interfacesEncoder ifaces )
                , ( "objs", Opt.globalGraphEncoder objs )
                ]


extrasDecoder : Decode.Decoder Extras
extrasDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "ArtifactsCached" ->
                        Decode.succeed ArtifactsCached

                    "ArtifactsFresh" ->
                        Decode.map2 ArtifactsFresh
                            (Decode.field "ifaces" interfacesDecoder)
                            (Decode.field "objs" Opt.globalGraphDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Extras' type: " ++ type_)
            )


fingerprintEncoder : Fingerprint -> Encode.Value
fingerprintEncoder =
    E.assocListDict compare Pkg.nameEncoder V.versionEncoder


fingerprintDecoder : Decode.Decoder Fingerprint
fingerprintDecoder =
    D.assocListDict identity Pkg.nameDecoder V.versionDecoder
