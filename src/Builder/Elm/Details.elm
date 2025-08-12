module Builder.Elm.Details exposing
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
import Builder.Elm.Outline as Outline
import Builder.File as File
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
            Utils.newMVar (Utils.maybeEncoder Opt.globalGraphEncoder) (Just o)

        ArtifactsCached ->
            fork (Utils.maybeEncoder Opt.globalGraphEncoder) (File.readBinary Opt.globalGraphDecoder (Stuff.objects root))


loadInterfaces : FilePath -> Details -> Task Never (MVar (Maybe Interfaces))
loadInterfaces root (Details _ _ _ _ _ extras) =
    case extras of
        ArtifactsFresh i _ ->
            Utils.newMVar (Utils.maybeEncoder interfacesEncoder) (Just i)

        ArtifactsCached ->
            fork (Utils.maybeEncoder interfacesEncoder) (File.readBinary interfacesDecoder (Stuff.interfaces root))



-- VERIFY INSTALL -- used by Install


verifyInstall : BW.Scope -> FilePath -> Solver.Env -> Outline.Outline -> Task Never (Result Exit.Details ())
verifyInstall scope root (Solver.Env cache manager connection registry) outline =
    File.getTime (root ++ "/elm.json")
        |> Task.bind
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



-- LOAD -- used by Make, Repl, Reactor, Test


load : Reporting.Style -> BW.Scope -> FilePath -> Task Never (Result Exit.Details Details)
load style scope root =
    File.getTime (root ++ "/elm.json")
        |> Task.bind
            (\newTime ->
                File.readBinary detailsDecoder (Stuff.details root)
                    |> Task.bind
                        (\maybeDetails ->
                            case maybeDetails of
                                Nothing ->
                                    generate style scope root newTime

                                Just (Details oldTime outline buildID locals foreigns extras) ->
                                    if oldTime == newTime then
                                        Task.pure (Ok (Details oldTime outline (buildID + 1) locals foreigns extras))

                                    else
                                        generate style scope root newTime
                        )
            )



-- GENERATE


generate : Reporting.Style -> BW.Scope -> FilePath -> File.Time -> Task Never (Result Exit.Details Details)
generate style scope root time =
    Reporting.trackDetails style
        (\key ->
            initEnv key scope root
                |> Task.bind
                    (\result ->
                        case result of
                            Err exit ->
                                Task.pure (Err exit)

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
    = Env Reporting.DKey BW.Scope FilePath Stuff.PackageCache Http.Manager Solver.Connection Registry.Registry


initEnv : Reporting.DKey -> BW.Scope -> FilePath -> Task Never (Result Exit.Details ( Env, Outline.Outline ))
initEnv key scope root =
    fork resultRegistryProblemEnvEncoder Solver.initEnv
        |> Task.bind
            (\mvar ->
                Outline.read root
                    |> Task.bind
                        (\eitherOutline ->
                            case eitherOutline of
                                Err problem ->
                                    Task.pure (Err (Exit.DetailsBadOutline problem))

                                Ok outline ->
                                    Utils.readMVar resultRegistryProblemEnvDecoder mvar
                                        |> Task.fmap
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
verifyPkg env time (Outline.PkgOutline pkg _ _ _ exposed direct testDirect elm) =
    if Con.goodElm elm then
        union identity Pkg.compareName noDups direct testDirect
            |> Task.bind (verifyConstraints env)
            |> Task.bind
                (\solution ->
                    let
                        exposedList : List ModuleName.Raw
                        exposedList =
                            Outline.flattenExposed exposed

                        exactDeps : Dict ( String, String ) Pkg.Name V.Version
                        exactDeps =
                            Dict.map (\_ (Solver.Details v _) -> v) solution

                        -- for pkg docs in reactor
                    in
                    verifyDependencies env time (ValidPkg pkg exposedList exactDeps) solution direct
                )

    else
        Task.throw (Exit.DetailsBadElmInPkg elm)


verifyApp : Env -> File.Time -> Outline.AppOutline -> Task Exit.Details Details
verifyApp env time ((Outline.AppOutline elmVersion srcDirs direct _ _ _) as outline) =
    if elmVersion == V.elmCompiler then
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


checkAppDeps : Outline.AppOutline -> Task Exit.Details (Dict ( String, String ) Pkg.Name V.Version)
checkAppDeps (Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
    union identity Pkg.compareName allowEqualDups indirect testDirect
        |> Task.bind
            (\x ->
                union identity Pkg.compareName noDups direct testIndirect
                    |> Task.bind (\y -> union identity Pkg.compareName noDups x y)
            )



-- VERIFY CONSTRAINTS


verifyConstraints : Env -> Dict ( String, String ) Pkg.Name Con.Constraint -> Task Exit.Details (Dict ( String, String ) Pkg.Name Solver.Details)
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


union : (k -> comparable) -> (k -> k -> Order) -> (k -> v -> v -> Task Exit.Details v) -> Dict comparable k v -> Dict comparable k v -> Task Exit.Details (Dict comparable k v)
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


noDups : k -> v -> v -> Task Exit.Details v
noDups _ _ _ =
    Task.throw Exit.DetailsHandEditedDependencies


allowEqualDups : k -> v -> v -> Task Exit.Details v
allowEqualDups _ v1 v2 =
    if v1 == v2 then
        Task.pure v1

    else
        Task.throw Exit.DetailsHandEditedDependencies



-- FORK


fork : (a -> BE.Encoder) -> Task Never a -> Task Never (MVar a)
fork encoder work =
    Utils.newEmptyMVar
        |> Task.bind
            (\mvar ->
                Utils.forkIO (Task.bind (Utils.putMVar encoder mvar) work)
                    |> Task.fmap (\_ -> mvar)
            )



-- VERIFY DEPENDENCIES


verifyDependencies : Env -> File.Time -> ValidOutline -> Dict ( String, String ) Pkg.Name Solver.Details -> Dict ( String, String ) Pkg.Name a -> Task Exit.Details Details
verifyDependencies ((Env key scope root cache _ _ _) as env) time outline solution directDeps =
    Task.eio identity
        (Reporting.report key (Reporting.DStart (Dict.size solution))
            |> Task.bind (\_ -> Utils.newEmptyMVar)
            |> Task.bind
                (\mvar ->
                    Stuff.withRegistryLock cache
                        (Utils.mapTraverseWithKey identity Pkg.compareName (\k v -> fork depEncoder (verifyDep env mvar solution k v)) solution)
                        |> Task.bind
                            (\mvars ->
                                Utils.putMVar dictNameMVarDepEncoder mvar mvars
                                    |> Task.bind
                                        (\_ ->
                                            Utils.mapTraverse identity Pkg.compareName (Utils.readMVar depDecoder) mvars
                                                |> Task.bind
                                                    (\deps ->
                                                        case Utils.sequenceDictResult identity Pkg.compareName deps of
                                                            Err _ ->
                                                                Stuff.getElmHome
                                                                    |> Task.fmap
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
                                                                BW.writeBinary Opt.globalGraphEncoder scope (Stuff.objects root) objs
                                                                    |> Task.bind (\_ -> BW.writeBinary interfacesEncoder scope (Stuff.interfaces root) ifaces)
                                                                    |> Task.bind (\_ -> BW.writeBinary detailsEncoder scope (Stuff.details root) details)
                                                                    |> Task.fmap (\_ -> Ok details)
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
verifyDep (Env key _ _ cache manager _ _) depsMVar solution pkg ((Solver.Details vsn directDeps) as details) =
    let
        fingerprint : Dict ( String, String ) Pkg.Name V.Version
        fingerprint =
            Utils.mapIntersectionWith identity Pkg.compareName (\(Solver.Details v _) _ -> v) solution directDeps
    in
    Utils.dirDoesDirectoryExist (Stuff.package cache pkg vsn ++ "/src")
        |> Task.bind
            (\exists ->
                if exists then
                    Reporting.report key Reporting.DCached
                        |> Task.bind
                            (\_ ->
                                File.readBinary artifactCacheDecoder (Stuff.package cache pkg vsn ++ "/artifacts.dat")
                                    |> Task.bind
                                        (\maybeCache ->
                                            case maybeCache of
                                                Nothing ->
                                                    build key cache depsMVar pkg details fingerprint EverySet.empty

                                                Just (ArtifactCache fingerprints artifacts) ->
                                                    if EverySet.member toComparableFingerprint fingerprint fingerprints then
                                                        Task.fmap (\_ -> Ok artifacts) (Reporting.report key Reporting.DBuilt)

                                                    else
                                                        build key cache depsMVar pkg details fingerprint fingerprints
                                        )
                            )

                else
                    Reporting.report key Reporting.DRequested
                        |> Task.bind
                            (\_ ->
                                downloadPackage cache manager pkg vsn
                                    |> Task.bind
                                        (\result ->
                                            case result of
                                                Err problem ->
                                                    Reporting.report key (Reporting.DFailed pkg vsn)
                                                        |> Task.fmap (\_ -> Err (Just (Exit.BD_BadDownload pkg vsn problem)))

                                                Ok () ->
                                                    Reporting.report key (Reporting.DReceived pkg vsn)
                                                        |> Task.bind (\_ -> build key cache depsMVar pkg details fingerprint EverySet.empty)
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


build : Reporting.DKey -> Stuff.PackageCache -> MVar (Dict ( String, String ) Pkg.Name (MVar Dep)) -> Pkg.Name -> Solver.Details -> Fingerprint -> EverySet (List ( ( String, String ), ( Int, Int, Int ) )) Fingerprint -> Task Never Dep
build key cache depsMVar pkg (Solver.Details vsn _) f fs =
    Outline.read (Stuff.package cache pkg vsn)
        |> Task.bind
            (\eitherOutline ->
                case eitherOutline of
                    Err _ ->
                        Reporting.report key Reporting.DBroken
                            |> Task.fmap (\_ -> Err (Just (Exit.BD_BadBuild pkg vsn f)))

                    Ok (Outline.App _) ->
                        Reporting.report key Reporting.DBroken
                            |> Task.fmap (\_ -> Err (Just (Exit.BD_BadBuild pkg vsn f)))

                    Ok (Outline.Pkg (Outline.PkgOutline _ _ _ _ exposed deps _ _)) ->
                        Utils.readMVar dictPkgNameMVarDepDecoder depsMVar
                            |> Task.bind
                                (\allDeps ->
                                    Utils.mapTraverse identity Pkg.compareName (Utils.readMVar depDecoder) (Dict.intersection compare allDeps deps)
                                        |> Task.bind
                                            (\directDeps ->
                                                case Utils.sequenceDictResult identity Pkg.compareName directDeps of
                                                    Err _ ->
                                                        Reporting.report key Reporting.DBroken
                                                            |> Task.fmap (\_ -> Err Nothing)

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
                                                            |> Task.bind
                                                                (\docsStatus ->
                                                                    Utils.newEmptyMVar
                                                                        |> Task.bind
                                                                            (\mvar ->
                                                                                Utils.mapTraverseWithKey identity compare (always << fork (BE.maybe statusEncoder) << crawlModule foreignDeps mvar pkg src docsStatus) exposedDict
                                                                                    |> Task.bind
                                                                                        (\mvars ->
                                                                                            Utils.putMVar statusDictEncoder mvar mvars
                                                                                                |> Task.bind (\_ -> Utils.dictMapM_ compare (Utils.readMVar (BD.maybe statusDecoder)) mvars)
                                                                                                |> Task.bind (\_ -> Task.bind (Utils.mapTraverse identity compare (Utils.readMVar (BD.maybe statusDecoder))) (Utils.readMVar statusDictDecoder mvar))
                                                                                                |> Task.bind
                                                                                                    (\maybeStatuses ->
                                                                                                        case Utils.sequenceDictMaybe identity compare maybeStatuses of
                                                                                                            Nothing ->
                                                                                                                Reporting.report key Reporting.DBroken
                                                                                                                    |> Task.fmap (\_ -> Err (Just (Exit.BD_BadBuild pkg vsn f)))

                                                                                                            Just statuses ->
                                                                                                                Utils.newEmptyMVar
                                                                                                                    |> Task.bind
                                                                                                                        (\rmvar ->
                                                                                                                            Utils.mapTraverse identity compare (fork (BE.maybe dResultEncoder) << compile pkg rmvar) statuses
                                                                                                                                |> Task.bind
                                                                                                                                    (\rmvars ->
                                                                                                                                        Utils.putMVar dictRawMVarMaybeDResultEncoder rmvar rmvars
                                                                                                                                            |> Task.bind (\_ -> Utils.mapTraverse identity compare (Utils.readMVar (BD.maybe dResultDecoder)) rmvars)
                                                                                                                                            |> Task.bind
                                                                                                                                                (\maybeResults ->
                                                                                                                                                    case Utils.sequenceDictMaybe identity compare maybeResults of
                                                                                                                                                        Nothing ->
                                                                                                                                                            Reporting.report key Reporting.DBroken
                                                                                                                                                                |> Task.fmap (\_ -> Err (Just (Exit.BD_BadBuild pkg vsn f)))

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
                                                                                                                                                                    gatherObjects results

                                                                                                                                                                artifacts : Artifacts
                                                                                                                                                                artifacts =
                                                                                                                                                                    Artifacts ifaces objects

                                                                                                                                                                fingerprints : EverySet (List ( ( String, String ), ( Int, Int, Int ) )) Fingerprint
                                                                                                                                                                fingerprints =
                                                                                                                                                                    EverySet.insert toComparableFingerprint f fs
                                                                                                                                                            in
                                                                                                                                                            writeDocs cache pkg vsn docsStatus results
                                                                                                                                                                |> Task.bind (\_ -> File.writeBinary artifactCacheEncoder path (ArtifactCache fingerprints artifacts))
                                                                                                                                                                |> Task.bind (\_ -> Reporting.report key Reporting.DBuilt)
                                                                                                                                                                |> Task.fmap (\_ -> Ok artifacts)
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


gatherObjects : Dict String ModuleName.Raw DResult -> Opt.GlobalGraph
gatherObjects results =
    Dict.foldr compare addLocalGraph Opt.empty results


addLocalGraph : ModuleName.Raw -> DResult -> Opt.GlobalGraph -> Opt.GlobalGraph
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


gatherInterfaces : Dict String ModuleName.Raw () -> Dict String ModuleName.Raw DResult -> Dict String ModuleName.Raw I.DependencyInterface
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


crawlModule : Dict String ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> ModuleName.Raw -> Task Never (Maybe Status)
crawlModule foreignDeps mvar pkg src docsStatus name =
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
        |> Task.bind
            (\guidaExists ->
                File.exists elmPath
                    |> Task.bind
                        (\elmExists ->
                            case Dict.get identity name foreignDeps of
                                Just ForeignAmbiguous ->
                                    Task.pure Nothing

                                Just (ForeignSpecific iface) ->
                                    if guidaExists || elmExists then
                                        Task.pure Nothing

                                    else
                                        Task.pure (Just (SForeign iface))

                                Nothing ->
                                    if guidaExists then
                                        crawlFile SV.Guida foreignDeps mvar pkg src docsStatus name guidaPath

                                    else if elmExists then
                                        crawlFile SV.Elm foreignDeps mvar pkg src docsStatus name elmPath

                                    else if Pkg.isKernel pkg && Name.isKernel name then
                                        crawlKernel foreignDeps mvar pkg src name

                                    else
                                        Task.pure Nothing
                        )
            )


crawlFile : SyntaxVersion -> Dict String ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> ModuleName.Raw -> FilePath -> Task Never (Maybe Status)
crawlFile syntaxVersion foreignDeps mvar pkg src docsStatus expectedName path =
    File.readUtf8 path
        |> Task.bind
            (\bytes ->
                case Parse.fromByteString syntaxVersion (Parse.Package pkg) bytes of
                    Ok ((Src.Module _ (Just (A.At _ actualName)) _ _ imports _ _ _ _ _) as modul) ->
                        if expectedName == actualName then
                            crawlImports foreignDeps mvar pkg src imports
                                |> Task.fmap (\deps -> Just (SLocal docsStatus deps modul))

                        else
                            Task.pure Nothing

                    _ ->
                        Task.pure Nothing
            )


crawlImports : Dict String ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> List Src.Import -> Task Never (Dict String ModuleName.Raw ())
crawlImports foreignDeps mvar pkg src imports =
    Utils.takeMVar statusDictDecoder mvar
        |> Task.bind
            (\statusDict ->
                let
                    deps : Dict String Name.Name ()
                    deps =
                        Dict.fromList identity (List.map (\i -> ( Src.getImportName i, () )) imports)

                    news : Dict String Name.Name ()
                    news =
                        Dict.diff deps statusDict
                in
                Utils.mapTraverseWithKey identity compare (always << fork (BE.maybe statusEncoder) << crawlModule foreignDeps mvar pkg src DocsNotNeeded) news
                    |> Task.bind
                        (\mvars ->
                            Utils.putMVar statusDictEncoder mvar (Dict.union mvars statusDict)
                                |> Task.bind (\_ -> Utils.dictMapM_ compare (Utils.readMVar (BD.maybe statusDecoder)) mvars)
                                |> Task.fmap (\_ -> deps)
                        )
            )


crawlKernel : Dict String ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> ModuleName.Raw -> Task Never (Maybe Status)
crawlKernel foreignDeps mvar pkg src name =
    let
        path : FilePath
        path =
            Utils.fpCombine src (Utils.fpAddExtension (ModuleName.toFilePath name) "js")
    in
    File.exists path
        |> Task.bind
            (\exists ->
                if exists then
                    File.readUtf8 path
                        |> Task.bind
                            (\bytes ->
                                case Kernel.fromByteString pkg (Utils.mapMapMaybe identity compare getDepHome foreignDeps) bytes of
                                    Nothing ->
                                        Task.pure Nothing

                                    Just (Kernel.Content imports chunks) ->
                                        crawlImports foreignDeps mvar pkg src (List.map Src.c1Value imports)
                                            |> Task.fmap (\_ -> Just (SKernelLocal chunks))
                            )

                else
                    Task.pure (Just SKernelForeign)
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


compile : Pkg.Name -> MVar (Dict String ModuleName.Raw (MVar (Maybe DResult))) -> Status -> Task Never (Maybe DResult)
compile pkg mvar status =
    case status of
        SLocal docsStatus deps modul ->
            Utils.readMVar moduleNameRawMVarMaybeDResultDecoder mvar
                |> Task.bind
                    (\resultsDict ->
                        Utils.mapTraverse identity compare (Utils.readMVar (BD.maybe dResultDecoder)) (Dict.intersection compare resultsDict deps)
                            |> Task.bind
                                (\maybeResults ->
                                    case Utils.sequenceDictMaybe identity compare maybeResults of
                                        Just results ->
                                            Compile.compile pkg (Utils.mapMapMaybe identity compare getInterface results) modul
                                                |> Task.fmap
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
                                                                        makeDocs docsStatus canonical
                                                                in
                                                                Just (RLocal ifaces objects docs)
                                                    )

                                        Nothing ->
                                            Task.pure Nothing
                                )
                    )

        SForeign iface ->
            Task.pure (Just (RForeign iface))

        SKernelLocal chunks ->
            Task.pure (Just (RKernelLocal chunks))

        SKernelForeign ->
            Task.pure (Just RKernelForeign)


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
        |> Task.fmap
            (\exists ->
                if exists then
                    DocsNotNeeded

                else
                    DocsNeeded
            )


makeDocs : DocsStatus -> Can.Module -> Maybe Docs.Module
makeDocs status modul =
    case status of
        DocsNeeded ->
            case Docs.fromModule modul of
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
            Task.pure ()


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
        |> Task.bind
            (\url ->
                Http.get manager url [] identity (Task.pure << Ok)
                    |> Task.bind
                        (\eitherByteString ->
                            case eitherByteString of
                                Err err ->
                                    Task.pure (Err (Exit.PP_BadEndpointRequest err))

                                Ok byteString ->
                                    case D.fromByteString endpointDecoder byteString of
                                        Err _ ->
                                            Task.pure (Err (Exit.PP_BadEndpointContent url))

                                        Ok ( endpoint, expectedHash ) ->
                                            Http.getArchive manager endpoint Exit.PP_BadArchiveRequest (Exit.PP_BadArchiveContent endpoint) <|
                                                \( sha, archive ) ->
                                                    if expectedHash == Http.shaToChars sha then
                                                        Task.fmap Ok (File.writePackage (Stuff.package cache pkg vsn) archive)

                                                    else
                                                        Task.pure (Err (Exit.PP_BadArchiveHash endpoint expectedHash (Http.shaToChars sha)))
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
