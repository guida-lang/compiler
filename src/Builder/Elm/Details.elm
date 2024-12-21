module Builder.Elm.Details exposing
    ( Details(..)
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
    = Details T.BF_Time ValidOutline T.BED_BuildID (Dict String T.CEMN_Raw T.BED_Local) (Dict String T.CEMN_Raw Foreign) Extras


type ValidOutline
    = ValidApp (NE.Nonempty Outline.SrcDir)
    | ValidPkg T.CEP_Name (List T.CEMN_Raw) (Dict ( String, String ) T.CEP_Name T.CEV_Version {- for docs in reactor -})


type Foreign
    = Foreign T.CEP_Name (List T.CEP_Name)


type Extras
    = ArtifactsCached
    | ArtifactsFresh Interfaces T.CASTO_GlobalGraph


type alias Interfaces =
    Dict (List String) T.CEMN_Canonical T.CEI_DependencyInterface



-- LOAD ARTIFACTS


loadObjects : T.FilePath -> Details -> IO T.MVar_Maybe_CASTO_GlobalGraph
loadObjects root (Details _ _ _ _ _ extras) =
    case extras of
        ArtifactsFresh _ o ->
            Utils.newMVar_Maybe_CASTO_GlobalGraph (Just o)

        ArtifactsCached ->
            fork_Maybe_CASTO_GlobalGraph (File.readBinary Opt.globalGraphDecoder (Stuff.objects root))


loadInterfaces : T.FilePath -> Details -> IO T.MVar_Maybe_BB_Dependencies
loadInterfaces root (Details _ _ _ _ _ extras) =
    case extras of
        ArtifactsFresh i _ ->
            Utils.newMVar_Maybe_BB_Dependencies (Just i)

        ArtifactsCached ->
            fork_Maybe_BB_Dependencies (File.readBinary interfacesDecoder (Stuff.interfaces root))



-- VERIFY INSTALL -- used by Install


verifyInstall : T.BBW_Scope -> T.FilePath -> T.BDS_Env -> Outline.Outline -> IO (Result Exit.Details ())
verifyInstall scope root (T.BDS_Env cache manager connection registry) outline =
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


load : Reporting.Style -> T.BBW_Scope -> T.FilePath -> IO (Result Exit.Details Details)
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


generate : Reporting.Style -> T.BBW_Scope -> T.FilePath -> T.BF_Time -> IO (Result Exit.Details Details)
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
    = Env Reporting.DKey T.BBW_Scope T.FilePath T.BS_PackageCache T.BH_Manager T.BDS_Connection T.BDR_Registry


initEnv : Reporting.DKey -> T.BBW_Scope -> T.FilePath -> IO (Result Exit.Details ( Env, Outline.Outline ))
initEnv key scope root =
    fork_ResultRegistryProblemEnv Solver.initEnv
        |> IO.bind
            (\mvar ->
                Outline.read root
                    |> IO.bind
                        (\eitherOutline ->
                            case eitherOutline of
                                Err problem ->
                                    IO.pure (Err (Exit.DetailsBadOutline problem))

                                Ok outline ->
                                    Utils.readMVar_ResultRegistryProblemEnv mvar
                                        |> IO.fmap
                                            (\maybeEnv ->
                                                case maybeEnv of
                                                    Err problem ->
                                                        Err (Exit.DetailsCannotGetRegistry problem)

                                                    Ok (T.BDS_Env cache manager connection registry) ->
                                                        Ok ( Env key scope root cache manager connection registry, outline )
                                            )
                        )
            )



-- VERIFY PROJECT


type alias Task a =
    Task.Task Exit.Details a


verifyPkg : Env -> T.BF_Time -> Outline.PkgOutline -> Task Details
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

                        exactDeps : Dict ( String, String ) T.CEP_Name T.CEV_Version
                        exactDeps =
                            Dict.map (\_ (Solver.Details v _) -> v) solution

                        -- for pkg docs in reactor
                    in
                    verifyDependencies env time (ValidPkg pkg exposedList exactDeps) solution direct
                )

    else
        Task.throw (Exit.DetailsBadElmInPkg elm)


verifyApp : Env -> T.BF_Time -> Outline.AppOutline -> Task Details
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


checkAppDeps : Outline.AppOutline -> Task (Dict ( String, String ) T.CEP_Name T.CEV_Version)
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


fork_Maybe_BED_Status : IO (Maybe T.BED_Status) -> IO T.MVar_Maybe_BED_Status
fork_Maybe_BED_Status work =
    Utils.newEmptyMVar_Maybe_BED_Status
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar_Maybe_BED_Status mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )


fork_Maybe_BED_DResult : IO (Maybe T.BED_DResult) -> IO T.MVar_Maybe_BED_DResult
fork_Maybe_BED_DResult work =
    Utils.newEmptyMVar_Maybe_BED_DResult
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar_Maybe_BED_DResult mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )


fork_Maybe_CASTO_GlobalGraph : IO (Maybe T.CASTO_GlobalGraph) -> IO T.MVar_Maybe_CASTO_GlobalGraph
fork_Maybe_CASTO_GlobalGraph work =
    Utils.newEmptyMVar_Maybe_CASTO_GlobalGraph
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar_Maybe_CASTO_GlobalGraph mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )


fork_ResultRegistryProblemEnv : IO (Result T.BRE_RegistryProblem T.BDS_Env) -> IO T.MVar_ResultRegistryProblemEnv
fork_ResultRegistryProblemEnv work =
    Utils.newEmptyMVar_ResultRegistryProblemEnv
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar_ResultRegistryProblemEnv mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )


fork_CED_Dep : IO T.CED_Dep -> IO T.MVar_CED_Dep
fork_CED_Dep work =
    Utils.newEmptyMVar_CED_Dep
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar_CED_Dep mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )


fork_Maybe_BB_Dependencies : IO (Maybe T.BB_Dependencies) -> IO T.MVar_Maybe_BB_Dependencies
fork_Maybe_BB_Dependencies work =
    Utils.newEmptyMVar_Maybe_BB_Dependencies
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar_Maybe_BB_Dependencies mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )



-- VERIFY DEPENDENCIES


verifyDependencies : Env -> T.BF_Time -> ValidOutline -> Dict ( String, String ) T.CEP_Name Solver.Details -> Dict ( String, String ) T.CEP_Name a -> Task Details
verifyDependencies ((Env key scope root cache _ _ _) as env) time outline solution directDeps =
    Task.eio identity
        (Reporting.report key (Reporting.DStart (Dict.size solution))
            |> IO.bind (\_ -> Utils.newEmptyMVar_DictNameMVarDep)
            |> IO.bind
                (\mvar ->
                    Stuff.withRegistryLock cache
                        (Utils.mapTraverseWithKey identity Pkg.compareName (\k v -> fork_CED_Dep (verifyDep env mvar solution k v)) solution)
                        |> IO.bind
                            (\mvars ->
                                Utils.putMVar_DictNameMVarDep mvar mvars
                                    |> IO.bind
                                        (\_ ->
                                            Utils.mapTraverse identity Pkg.compareName Utils.readMVar_CED_Dep mvars
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


addObjects : T.CED_Artifacts -> T.CASTO_GlobalGraph -> T.CASTO_GlobalGraph
addObjects (T.CED_Artifacts _ objs) graph =
    Opt.addGlobalGraph objs graph


addInterfaces : Dict ( String, String ) T.CEP_Name a -> T.CEP_Name -> T.CED_Artifacts -> Interfaces -> Interfaces
addInterfaces directDeps pkg (T.CED_Artifacts ifaces _) dependencyInterfaces =
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


gatherForeigns : T.CEP_Name -> T.CED_Artifacts -> Dict String T.CEMN_Raw (OneOrMore.OneOrMore T.CEP_Name) -> Dict String T.CEMN_Raw (OneOrMore.OneOrMore T.CEP_Name)
gatherForeigns pkg (T.CED_Artifacts ifaces _) foreigns =
    let
        isPublic : T.CEI_DependencyInterface -> Maybe (OneOrMore.OneOrMore T.CEP_Name)
        isPublic di =
            case di of
                T.CEI_Public _ ->
                    Just (OneOrMore.one pkg)

                T.CEI_Private _ _ _ ->
                    Nothing
    in
    Utils.mapUnionWith identity compare OneOrMore.more foreigns (Utils.mapMapMaybe identity compare isPublic ifaces)



-- VERIFY DEPENDENCY


verifyDep : Env -> T.MVar_DictNameMVarDep -> Dict ( String, String ) T.CEP_Name Solver.Details -> T.CEP_Name -> Solver.Details -> IO T.CED_Dep
verifyDep (Env key _ _ cache manager _ _) depsMVar solution pkg ((Solver.Details vsn directDeps) as details) =
    let
        fingerprint : Dict ( String, String ) T.CEP_Name T.CEV_Version
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
                                                        |> IO.fmap (\_ -> Err (Just (T.BRE_BD_BadDownload pkg vsn problem)))

                                                Ok () ->
                                                    Reporting.report key (Reporting.DReceived pkg vsn)
                                                        |> IO.bind (\_ -> build key cache depsMVar pkg details fingerprint EverySet.empty)
                                        )
                            )
            )



-- ARTIFACT CACHE


type ArtifactCache
    = ArtifactCache (EverySet (List ( ( String, String ), ( Int, Int, Int ) )) Fingerprint) T.CED_Artifacts


type alias Fingerprint =
    Dict ( String, String ) T.CEP_Name T.CEV_Version


toComparableFingerprint : Fingerprint -> List ( ( String, String ), ( Int, Int, Int ) )
toComparableFingerprint fingerprint =
    Dict.toList compare fingerprint
        |> List.map (Tuple.mapSecond V.toComparable)



-- BUILD


build : Reporting.DKey -> T.BS_PackageCache -> T.MVar_DictNameMVarDep -> T.CEP_Name -> Solver.Details -> Fingerprint -> EverySet (List ( ( String, String ), ( Int, Int, Int ) )) Fingerprint -> IO T.CED_Dep
build key cache depsMVar pkg (Solver.Details vsn _) f fs =
    Outline.read (Stuff.package cache pkg vsn)
        |> IO.bind
            (\eitherOutline ->
                case eitherOutline of
                    Err _ ->
                        Reporting.report key Reporting.DBroken
                            |> IO.fmap (\_ -> Err (Just (T.BRE_BD_BadBuild pkg vsn f)))

                    Ok (Outline.App _) ->
                        Reporting.report key Reporting.DBroken
                            |> IO.fmap (\_ -> Err (Just (T.BRE_BD_BadBuild pkg vsn f)))

                    Ok (Outline.Pkg (Outline.PkgOutline _ _ _ _ exposed deps _ _)) ->
                        Utils.readMVar_DictNameMVarDep depsMVar
                            |> IO.bind
                                (\allDeps ->
                                    Utils.mapTraverse identity Pkg.compareName Utils.readMVar_CED_Dep (Dict.intersection compare allDeps deps)
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
                                                                    Utils.newEmptyMVar_BED_StatusDict
                                                                        |> IO.bind
                                                                            (\mvar ->
                                                                                Utils.mapTraverseWithKey identity compare (always << fork_Maybe_BED_Status << crawlModule foreignDeps mvar pkg src docsStatus) exposedDict
                                                                                    |> IO.bind
                                                                                        (\mvars ->
                                                                                            Utils.putMVar_BED_StatusDict mvar mvars
                                                                                                |> IO.bind (\_ -> Utils.dictMapM_ compare Utils.readMVar_Maybe_BED_Status mvars)
                                                                                                |> IO.bind (\_ -> IO.bind (Utils.mapTraverse identity compare Utils.readMVar_Maybe_BED_Status) (Utils.readMVar_BED_StatusDict mvar))
                                                                                                |> IO.bind
                                                                                                    (\maybeStatuses ->
                                                                                                        case Utils.sequenceDictMaybe identity compare maybeStatuses of
                                                                                                            Nothing ->
                                                                                                                Reporting.report key Reporting.DBroken
                                                                                                                    |> IO.fmap (\_ -> Err (Just (T.BRE_BD_BadBuild pkg vsn f)))

                                                                                                            Just statuses ->
                                                                                                                Utils.newEmptyMVar_DictRawMVarMaybeDResult
                                                                                                                    |> IO.bind
                                                                                                                        (\rmvar ->
                                                                                                                            Utils.mapTraverse identity compare (fork_Maybe_BED_DResult << compile pkg rmvar) statuses
                                                                                                                                |> IO.bind
                                                                                                                                    (\rmvars ->
                                                                                                                                        Utils.putMVar_DictRawMVarMaybeDResult rmvar rmvars
                                                                                                                                            |> IO.bind (\_ -> Utils.mapTraverse identity compare Utils.readMVar_Maybe_BED_DResult rmvars)
                                                                                                                                            |> IO.bind
                                                                                                                                                (\maybeResults ->
                                                                                                                                                    case Utils.sequenceDictMaybe identity compare maybeResults of
                                                                                                                                                        Nothing ->
                                                                                                                                                            Reporting.report key Reporting.DBroken
                                                                                                                                                                |> IO.fmap (\_ -> Err (Just (T.BRE_BD_BadBuild pkg vsn f)))

                                                                                                                                                        Just results ->
                                                                                                                                                            let
                                                                                                                                                                path : String
                                                                                                                                                                path =
                                                                                                                                                                    Stuff.package cache pkg vsn ++ "/artifacts.json"

                                                                                                                                                                ifaces : Dict String T.CEMN_Raw T.CEI_DependencyInterface
                                                                                                                                                                ifaces =
                                                                                                                                                                    gatherInterfaces exposedDict results

                                                                                                                                                                objects : T.CASTO_GlobalGraph
                                                                                                                                                                objects =
                                                                                                                                                                    gatherObjects results

                                                                                                                                                                artifacts : T.CED_Artifacts
                                                                                                                                                                artifacts =
                                                                                                                                                                    T.CED_Artifacts ifaces objects

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


gatherObjects : Dict String T.CEMN_Raw T.BED_DResult -> T.CASTO_GlobalGraph
gatherObjects results =
    Dict.foldr compare addLocalGraph Opt.empty results


addLocalGraph : T.CEMN_Raw -> T.BED_DResult -> T.CASTO_GlobalGraph -> T.CASTO_GlobalGraph
addLocalGraph name status graph =
    case status of
        T.BED_RLocal _ objs _ ->
            Opt.addLocalGraph objs graph

        T.BED_RForeign _ ->
            graph

        T.BED_RKernelLocal cs ->
            Opt.addKernel (Name.getKernel name) cs graph

        T.BED_RKernelForeign ->
            graph


gatherInterfaces : Dict String T.CEMN_Raw () -> Dict String T.CEMN_Raw T.BED_DResult -> Dict String T.CEMN_Raw T.CEI_DependencyInterface
gatherInterfaces exposed artifacts =
    let
        onLeft : a -> b -> c -> d
        onLeft _ _ _ =
            crash "compiler bug manifesting in Elm.Details.gatherInterfaces"

        onBoth : comparable -> () -> T.BED_DResult -> Dict comparable comparable T.CEI_DependencyInterface -> Dict comparable comparable T.CEI_DependencyInterface
        onBoth k () iface =
            toLocalInterface I.public iface
                |> Maybe.map (Dict.insert identity k)
                |> Maybe.withDefault identity

        onRight : comparable -> T.BED_DResult -> Dict comparable comparable T.CEI_DependencyInterface -> Dict comparable comparable T.CEI_DependencyInterface
        onRight k iface =
            toLocalInterface I.private iface
                |> Maybe.map (Dict.insert identity k)
                |> Maybe.withDefault identity
    in
    Dict.merge compare onLeft onBoth onRight exposed artifacts Dict.empty


toLocalInterface : (T.CEI_Interface -> a) -> T.BED_DResult -> Maybe a
toLocalInterface func result =
    case result of
        T.BED_RLocal iface _ _ ->
            Just (func iface)

        T.BED_RForeign _ ->
            Nothing

        T.BED_RKernelLocal _ ->
            Nothing

        T.BED_RKernelForeign ->
            Nothing



-- GATHER FOREIGN INTERFACES


type ForeignInterface
    = ForeignAmbiguous
    | ForeignSpecific T.CEI_Interface


gatherForeignInterfaces : Dict ( String, String ) T.CEP_Name T.CED_Artifacts -> Dict String T.CEMN_Raw ForeignInterface
gatherForeignInterfaces directArtifacts =
    let
        finalize : T.CEI_Interface -> List T.CEI_Interface -> ForeignInterface
        finalize i is =
            case is of
                [] ->
                    ForeignSpecific i

                _ :: _ ->
                    ForeignAmbiguous

        gather : T.CEP_Name -> T.CED_Artifacts -> Dict String T.CEMN_Raw (OneOrMore.OneOrMore T.CEI_Interface) -> Dict String T.CEMN_Raw (OneOrMore.OneOrMore T.CEI_Interface)
        gather _ (T.CED_Artifacts ifaces _) buckets =
            Utils.mapUnionWith identity compare OneOrMore.more buckets (Utils.mapMapMaybe identity compare isPublic ifaces)

        isPublic : T.CEI_DependencyInterface -> Maybe (OneOrMore.OneOrMore T.CEI_Interface)
        isPublic di =
            case di of
                T.CEI_Public iface ->
                    Just (OneOrMore.one iface)

                T.CEI_Private _ _ _ ->
                    Nothing
    in
    Dict.map (\_ -> OneOrMore.destruct finalize) <|
        Dict.foldr compare gather Dict.empty directArtifacts



-- CRAWL


crawlModule : Dict String T.CEMN_Raw ForeignInterface -> T.MVar_BED_StatusDict -> T.CEP_Name -> T.FilePath -> T.BED_DocsStatus -> T.CEMN_Raw -> IO (Maybe T.BED_Status)
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


crawlFile : Dict String T.CEMN_Raw ForeignInterface -> T.MVar_BED_StatusDict -> T.CEP_Name -> T.FilePath -> T.BED_DocsStatus -> T.CEMN_Raw -> T.FilePath -> IO (Maybe T.BED_Status)
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


crawlImports : Dict String T.CEMN_Raw ForeignInterface -> T.MVar_BED_StatusDict -> T.CEP_Name -> T.FilePath -> List T.CASTS_Import -> IO (Dict String T.CEMN_Raw ())
crawlImports foreignDeps mvar pkg src imports =
    Utils.takeMVar_BED_StatusDict mvar
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
                            Utils.putMVar_BED_StatusDict mvar (Dict.union mvars statusDict)
                                |> IO.bind (\_ -> Utils.dictMapM_ compare Utils.readMVar_Maybe_BED_Status mvars)
                                |> IO.fmap (\_ -> deps)
                        )
            )


crawlKernel : Dict String T.CEMN_Raw ForeignInterface -> T.MVar_BED_StatusDict -> T.CEP_Name -> T.FilePath -> T.CEMN_Raw -> IO (Maybe T.BED_Status)
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


compile : T.CEP_Name -> T.MVar_DictRawMVarMaybeDResult -> T.BED_Status -> IO (Maybe T.BED_DResult)
compile pkg mvar status =
    case status of
        T.BED_SLocal docsStatus deps modul ->
            Utils.readMVar_DictRawMVarMaybeDResult mvar
                |> IO.bind
                    (\resultsDict ->
                        Utils.mapTraverse identity compare Utils.readMVar_Maybe_BED_DResult (Dict.intersection compare resultsDict deps)
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

                                                                    docs : Maybe T.CED_Module
                                                                    docs =
                                                                        makeDocs docsStatus canonical
                                                                in
                                                                Just (T.BED_RLocal ifaces objects docs)
                                                    )

                                        Nothing ->
                                            IO.pure Nothing
                                )
                    )

        T.BED_SForeign iface ->
            IO.pure (Just (T.BED_RForeign iface))

        T.BED_SKernelLocal chunks ->
            IO.pure (Just (T.BED_RKernelLocal chunks))

        T.BED_SKernelForeign ->
            IO.pure (Just T.BED_RKernelForeign)


getInterface : T.BED_DResult -> Maybe T.CEI_Interface
getInterface result =
    case result of
        T.BED_RLocal iface _ _ ->
            Just iface

        T.BED_RForeign iface ->
            Just iface

        T.BED_RKernelLocal _ ->
            Nothing

        T.BED_RKernelForeign ->
            Nothing



-- MAKE DOCS


getDocsStatus : T.BS_PackageCache -> T.CEP_Name -> T.CEV_Version -> IO T.BED_DocsStatus
getDocsStatus cache pkg vsn =
    File.exists (Stuff.package cache pkg vsn ++ "/docs.json")
        |> IO.fmap
            (\exists ->
                if exists then
                    T.BED_DocsNotNeeded

                else
                    T.BED_DocsNeeded
            )


makeDocs : T.BED_DocsStatus -> Can.Module -> Maybe T.CED_Module
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


writeDocs : T.BS_PackageCache -> T.CEP_Name -> T.CEV_Version -> T.BED_DocsStatus -> Dict String T.CEMN_Raw T.BED_DResult -> IO ()
writeDocs cache pkg vsn status results =
    case status of
        T.BED_DocsNeeded ->
            E.writeUgly (Stuff.package cache pkg vsn ++ "/docs.json")
                (Docs.encode (Utils.mapMapMaybe identity compare toDocs results))

        T.BED_DocsNotNeeded ->
            IO.pure ()


toDocs : T.BED_DResult -> Maybe T.CED_Module
toDocs result =
    case result of
        T.BED_RLocal _ _ docs ->
            docs

        T.BED_RForeign _ ->
            Nothing

        T.BED_RKernelLocal _ ->
            Nothing

        T.BED_RKernelForeign ->
            Nothing



-- DOWNLOAD PACKAGE


downloadPackage : T.BS_PackageCache -> T.BH_Manager -> T.CEP_Name -> T.CEV_Version -> IO (Result T.BRE_PackageProblem ())
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
                        IO.pure (Err (T.BRE_PP_BadEndpointRequest err))

                    Ok byteString ->
                        case D.fromByteString endpointDecoder byteString of
                            Err _ ->
                                IO.pure (Err (T.BRE_PP_BadEndpointContent url))

                            Ok ( endpoint, expectedHash ) ->
                                Http.getArchive manager endpoint T.BRE_PP_BadArchiveRequest (T.BRE_PP_BadArchiveContent endpoint) <|
                                    \( sha, archive ) ->
                                        if expectedHash == Http.shaToChars sha then
                                            IO.fmap Ok (File.writePackage (Stuff.package cache pkg vsn) archive)

                                        else
                                            IO.pure (Err (T.BRE_PP_BadArchiveHash endpoint expectedHash (Http.shaToChars sha)))
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


artifactsEncoder : T.CED_Artifacts -> Encode.Value
artifactsEncoder (T.CED_Artifacts ifaces objects) =
    Encode.object
        [ ( "type", Encode.string "Artifacts" )
        , ( "ifaces", E.assocListDict compare ModuleName.rawEncoder I.dependencyInterfaceEncoder ifaces )
        , ( "objects", Opt.globalGraphEncoder objects )
        ]


artifactsDecoder : Decode.Decoder T.CED_Artifacts
artifactsDecoder =
    Decode.map2 T.CED_Artifacts
        (Decode.field "ifaces" (D.assocListDict identity ModuleName.rawDecoder I.dependencyInterfaceDecoder))
        (Decode.field "objects" Opt.globalGraphDecoder)


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


localEncoder : T.BED_Local -> Encode.Value
localEncoder (T.BED_Local path time deps hasMain lastChange lastCompile) =
    Encode.object
        [ ( "type", Encode.string "Local" )
        , ( "path", Encode.string path )
        , ( "time", File.timeEncoder time )
        , ( "deps", Encode.list ModuleName.rawEncoder deps )
        , ( "hasMain", Encode.bool hasMain )
        , ( "lastChange", Encode.int lastChange )
        , ( "lastCompile", Encode.int lastCompile )
        ]


localDecoder : Decode.Decoder T.BED_Local
localDecoder =
    Decode.map6 T.BED_Local
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
