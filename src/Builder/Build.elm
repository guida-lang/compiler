module Builder.Build exposing
    ( DocsGoal(..)
    , ReplArtifacts(..)
    , fromExposed_Documentation
    , fromExposed_Unit
    , fromPaths
    , fromRepl
    , getRootNames
    , ignoreDocs
    , keepDocs
    , writeDocs
    )

import Basics.Extra exposing (flip)
import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Source as Src
import Compiler.Compile as Compile
import Compiler.Data.Map.Utils as Map
import Compiler.Data.Name as Name
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Elm.Docs as Docs
import Compiler.Elm.Interface as I
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Json.Encode as E
import Compiler.Parse.Module as Parse
import Compiler.Reporting.Render.Type.Localizer as L
import Data.Graph as Graph
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet
import System.IO as IO
import Types as T exposing (IO)
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- ENVIRONMENT


type Env
    = Env T.BR_BKey String Parse.ProjectType (List AbsoluteSrcDir) T.BED_BuildID (Dict String T.CEMN_Raw T.BED_Local) (Dict String T.CEMN_Raw Details.Foreign)


makeEnv : T.BR_BKey -> T.FilePath -> Details.Details -> IO Env
makeEnv key root (Details.Details _ validOutline buildID locals foreigns _) =
    case validOutline of
        Details.ValidApp givenSrcDirs ->
            Utils.listTraverse (toAbsoluteSrcDir root) (NE.toList givenSrcDirs)
                |> IO.fmap (\srcDirs -> Env key root Parse.Application srcDirs buildID locals foreigns)

        Details.ValidPkg pkg _ _ ->
            toAbsoluteSrcDir root (Outline.RelativeSrcDir "src")
                |> IO.fmap (\srcDir -> Env key root (Parse.Package pkg) [ srcDir ] buildID locals foreigns)



-- SOURCE DIRECTORY


type AbsoluteSrcDir
    = AbsoluteSrcDir T.FilePath


toAbsoluteSrcDir : T.FilePath -> Outline.SrcDir -> IO AbsoluteSrcDir
toAbsoluteSrcDir root srcDir =
    IO.fmap AbsoluteSrcDir
        (Utils.dirCanonicalizePath
            (case srcDir of
                Outline.AbsoluteSrcDir dir ->
                    dir

                Outline.RelativeSrcDir dir ->
                    Utils.fpForwardSlash root dir
            )
        )


addRelative : AbsoluteSrcDir -> T.FilePath -> T.FilePath
addRelative (AbsoluteSrcDir srcDir) path =
    Utils.fpForwardSlash srcDir path



-- FORK


{-| PERF try using IORef semephore on file crawl phase?
described in Chapter 13 of Parallel and Concurrent Programming in Haskell by Simon Marlow
<https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch13.html#sec_conc-par-overhead>
-}
fork_Result_BuildProjectProblem_RootInfo : IO (Result T.BRE_BuildProjectProblem T.BB_RootInfo) -> IO T.MVar_Result_BuildProjectProblem_RootInfo
fork_Result_BuildProjectProblem_RootInfo work =
    Utils.newEmptyMVar_Result_BuildProjectProblem_RootInfo
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar_Result_BuildProjectProblem_RootInfo mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )


fork_MaybeDep : IO (Maybe T.BB_Dep) -> IO T.MVar_MaybeDep
fork_MaybeDep work =
    Utils.newEmptyMVar_MaybeDep
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar_MaybeDep mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )


fork_BB_RootResult : IO T.BB_RootResult -> IO T.MVar_BB_RootResult
fork_BB_RootResult work =
    Utils.newEmptyMVar_BB_RootResult
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar_BB_RootResult mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )


fork_BB_RootStatus : IO T.BB_RootStatus -> IO T.MVar_BB_RootStatus
fork_BB_RootStatus work =
    Utils.newEmptyMVar_BB_RootStatus
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar_BB_RootStatus mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )


fork_BB_BResult : IO T.BB_BResult -> IO T.MVar_BB_BResult
fork_BB_BResult work =
    Utils.newEmptyMVar_BB_BResult
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar_BB_BResult mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )


fork_BB_Status : IO T.BB_Status -> IO T.MVar_BB_Status
fork_BB_Status work =
    Utils.newEmptyMVar_BB_Status
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar_BB_Status mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )


forkWithKey_BB_BResult : (k -> comparable) -> (k -> k -> Order) -> (k -> a -> IO T.BB_BResult) -> Dict comparable k a -> IO (Dict comparable k T.MVar_BB_BResult)
forkWithKey_BB_BResult toComparable keyComparison func dict =
    Utils.mapTraverseWithKey toComparable keyComparison (\k v -> fork_BB_BResult (func k v)) dict



-- FROM EXPOSED


fromExposed_Documentation : Reporting.Style -> T.FilePath -> Details.Details -> DocsGoal T.CED_Documentation -> NE.Nonempty T.CEMN_Raw -> IO (Result T.BRE_BuildProblem T.CED_Documentation)
fromExposed_Documentation style root details docsGoal ((NE.Nonempty e es) as exposed) =
    Reporting.trackBuild_Documentation style <|
        \key ->
            makeEnv key root details
                |> IO.bind
                    (\env ->
                        Details.loadInterfaces root details
                            |> IO.bind
                                (\dmvar ->
                                    -- crawl
                                    Utils.newEmptyMVar_BB_StatusDict
                                        |> IO.bind
                                            (\mvar ->
                                                let
                                                    docsNeed : T.BB_DocsNeed
                                                    docsNeed =
                                                        toDocsNeed docsGoal
                                                in
                                                Map.fromKeysA identity (fork_BB_Status << crawlModule env mvar docsNeed) (e :: es)
                                                    |> IO.bind
                                                        (\roots ->
                                                            Utils.putMVar_BB_StatusDict mvar roots
                                                                |> IO.bind
                                                                    (\_ ->
                                                                        Utils.dictMapM_ compare Utils.readMVar_BB_Status roots
                                                                            |> IO.bind
                                                                                (\_ ->
                                                                                    IO.bind (Utils.mapTraverse identity compare Utils.readMVar_BB_Status) (Utils.readMVar_BB_StatusDict mvar)
                                                                                        |> IO.bind
                                                                                            (\statuses ->
                                                                                                -- compile
                                                                                                checkMidpoint dmvar statuses
                                                                                                    |> IO.bind
                                                                                                        (\midpoint ->
                                                                                                            case midpoint of
                                                                                                                Err problem ->
                                                                                                                    IO.pure (Err (T.BRE_BuildProjectProblem problem))

                                                                                                                Ok foreigns ->
                                                                                                                    Utils.newEmptyMVar_BB_ResultDict
                                                                                                                        |> IO.bind
                                                                                                                            (\rmvar ->
                                                                                                                                forkWithKey_BB_BResult identity compare (checkModule env foreigns rmvar) statuses
                                                                                                                                    |> IO.bind
                                                                                                                                        (\resultMVars ->
                                                                                                                                            Utils.putMVar_BB_ResultDict rmvar resultMVars
                                                                                                                                                |> IO.bind
                                                                                                                                                    (\_ ->
                                                                                                                                                        Utils.mapTraverse identity compare Utils.readMVar_BB_BResult resultMVars
                                                                                                                                                            |> IO.bind
                                                                                                                                                                (\results ->
                                                                                                                                                                    writeDetails root details results
                                                                                                                                                                        |> IO.bind
                                                                                                                                                                            (\_ ->
                                                                                                                                                                                finalizeExposed root docsGoal exposed results
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
                                            )
                                )
                    )


fromExposed_Unit : Reporting.Style -> T.FilePath -> Details.Details -> DocsGoal () -> NE.Nonempty T.CEMN_Raw -> IO (Result T.BRE_BuildProblem ())
fromExposed_Unit style root details docsGoal ((NE.Nonempty e es) as exposed) =
    Reporting.trackBuild_Unit style <|
        \key ->
            makeEnv key root details
                |> IO.bind
                    (\env ->
                        Details.loadInterfaces root details
                            |> IO.bind
                                (\dmvar ->
                                    -- crawl
                                    Utils.newEmptyMVar_BB_StatusDict
                                        |> IO.bind
                                            (\mvar ->
                                                let
                                                    docsNeed : T.BB_DocsNeed
                                                    docsNeed =
                                                        toDocsNeed docsGoal
                                                in
                                                Map.fromKeysA identity (fork_BB_Status << crawlModule env mvar docsNeed) (e :: es)
                                                    |> IO.bind
                                                        (\roots ->
                                                            Utils.putMVar_BB_StatusDict mvar roots
                                                                |> IO.bind
                                                                    (\_ ->
                                                                        Utils.dictMapM_ compare Utils.readMVar_BB_Status roots
                                                                            |> IO.bind
                                                                                (\_ ->
                                                                                    IO.bind (Utils.mapTraverse identity compare Utils.readMVar_BB_Status) (Utils.readMVar_BB_StatusDict mvar)
                                                                                        |> IO.bind
                                                                                            (\statuses ->
                                                                                                -- compile
                                                                                                checkMidpoint dmvar statuses
                                                                                                    |> IO.bind
                                                                                                        (\midpoint ->
                                                                                                            case midpoint of
                                                                                                                Err problem ->
                                                                                                                    IO.pure (Err (T.BRE_BuildProjectProblem problem))

                                                                                                                Ok foreigns ->
                                                                                                                    Utils.newEmptyMVar_BB_ResultDict
                                                                                                                        |> IO.bind
                                                                                                                            (\rmvar ->
                                                                                                                                forkWithKey_BB_BResult identity compare (checkModule env foreigns rmvar) statuses
                                                                                                                                    |> IO.bind
                                                                                                                                        (\resultMVars ->
                                                                                                                                            Utils.putMVar_BB_ResultDict rmvar resultMVars
                                                                                                                                                |> IO.bind
                                                                                                                                                    (\_ ->
                                                                                                                                                        Utils.mapTraverse identity compare Utils.readMVar_BB_BResult resultMVars
                                                                                                                                                            |> IO.bind
                                                                                                                                                                (\results ->
                                                                                                                                                                    writeDetails root details results
                                                                                                                                                                        |> IO.bind
                                                                                                                                                                            (\_ ->
                                                                                                                                                                                finalizeExposed root docsGoal exposed results
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
                                            )
                                )
                    )



-- FROM PATHS


fromPaths : Reporting.Style -> T.FilePath -> Details.Details -> NE.Nonempty T.FilePath -> IO (Result T.BRE_BuildProblem T.BB_Artifacts)
fromPaths style root details paths =
    Reporting.trackBuild_BB_Artifacts style <|
        \key ->
            makeEnv key root details
                |> IO.bind
                    (\env ->
                        findRoots env paths
                            |> IO.bind
                                (\elroots ->
                                    case elroots of
                                        Err problem ->
                                            IO.pure (Err (T.BRE_BuildProjectProblem problem))

                                        Ok lroots ->
                                            -- crawl
                                            Details.loadInterfaces root details
                                                |> IO.bind
                                                    (\dmvar ->
                                                        Utils.newMVar_BB_StatusDict Dict.empty
                                                            |> IO.bind
                                                                (\smvar ->
                                                                    Utils.nonEmptyListTraverse (fork_BB_RootStatus << crawlRoot env smvar) lroots
                                                                        |> IO.bind
                                                                            (\srootMVars ->
                                                                                Utils.nonEmptyListTraverse Utils.readMVar_BB_RootStatus srootMVars
                                                                                    |> IO.bind
                                                                                        (\sroots ->
                                                                                            IO.bind (Utils.mapTraverse identity compare Utils.readMVar_BB_Status) (Utils.readMVar_BB_StatusDict smvar)
                                                                                                |> IO.bind
                                                                                                    (\statuses ->
                                                                                                        checkMidpointAndRoots dmvar statuses sroots
                                                                                                            |> IO.bind
                                                                                                                (\midpoint ->
                                                                                                                    case midpoint of
                                                                                                                        Err problem ->
                                                                                                                            IO.pure (Err (T.BRE_BuildProjectProblem problem))

                                                                                                                        Ok foreigns ->
                                                                                                                            -- compile
                                                                                                                            Utils.newEmptyMVar_BB_ResultDict
                                                                                                                                |> IO.bind
                                                                                                                                    (\rmvar ->
                                                                                                                                        forkWithKey_BB_BResult identity compare (checkModule env foreigns rmvar) statuses
                                                                                                                                            |> IO.bind
                                                                                                                                                (\resultsMVars ->
                                                                                                                                                    Utils.putMVar_BB_ResultDict rmvar resultsMVars
                                                                                                                                                        |> IO.bind
                                                                                                                                                            (\_ ->
                                                                                                                                                                Utils.nonEmptyListTraverse (fork_BB_RootResult << checkRoot env resultsMVars) sroots
                                                                                                                                                                    |> IO.bind
                                                                                                                                                                        (\rrootMVars ->
                                                                                                                                                                            Utils.mapTraverse identity compare Utils.readMVar_BB_BResult resultsMVars
                                                                                                                                                                                |> IO.bind
                                                                                                                                                                                    (\results ->
                                                                                                                                                                                        writeDetails root details results
                                                                                                                                                                                            |> IO.bind
                                                                                                                                                                                                (\_ ->
                                                                                                                                                                                                    IO.fmap (toArtifacts env foreigns results) (Utils.nonEmptyListTraverse Utils.readMVar_BB_RootResult rrootMVars)
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
                                                                )
                                                    )
                                )
                    )



-- GET ROOT NAMES


getRootNames : T.BB_Artifacts -> NE.Nonempty T.CEMN_Raw
getRootNames (T.BB_Artifacts _ _ roots _) =
    NE.map getRootName roots


getRootName : T.BB_Root -> T.CEMN_Raw
getRootName root =
    case root of
        T.BB_Inside name ->
            name

        T.BB_Outside name _ _ ->
            name



-- CRAWL


crawlDeps : Env -> T.MVar_BB_StatusDict -> List T.CEMN_Raw -> a -> IO a
crawlDeps env mvar deps blockedValue =
    let
        crawlNew : T.CEMN_Raw -> () -> IO T.MVar_BB_Status
        crawlNew name () =
            fork_BB_Status (crawlModule env mvar (T.BB_DocsNeed False) name)
    in
    Utils.takeMVar_BB_StatusDict mvar
        |> IO.bind
            (\statusDict ->
                let
                    depsDict : Dict String T.CEMN_Raw ()
                    depsDict =
                        Map.fromKeys (\_ -> ()) deps

                    newsDict : Dict String T.CEMN_Raw ()
                    newsDict =
                        Dict.diff depsDict statusDict
                in
                Utils.mapTraverseWithKey identity compare crawlNew newsDict
                    |> IO.bind
                        (\statuses ->
                            Utils.putMVar_BB_StatusDict mvar (Dict.union statuses statusDict)
                                |> IO.bind
                                    (\_ ->
                                        Utils.dictMapM_ compare Utils.readMVar_BB_Status statuses
                                            |> IO.fmap (\_ -> blockedValue)
                                    )
                        )
            )


crawlModule : Env -> T.MVar_BB_StatusDict -> T.BB_DocsNeed -> T.CEMN_Raw -> IO T.BB_Status
crawlModule ((Env _ root projectType srcDirs buildID locals foreigns) as env) mvar ((T.BB_DocsNeed needsDocs) as docsNeed) name =
    let
        fileName : String
        fileName =
            ModuleName.toFilePath name ++ ".elm"
    in
    Utils.filterM File.exists (List.map (flip addRelative fileName) srcDirs)
        |> IO.bind
            (\paths ->
                case paths of
                    [ path ] ->
                        case Dict.get identity name foreigns of
                            Just (Details.Foreign dep deps) ->
                                IO.pure <| T.BB_SBadImport <| T.CREI_Ambiguous path [] dep deps

                            Nothing ->
                                File.getTime path
                                    |> IO.bind
                                        (\newTime ->
                                            case Dict.get identity name locals of
                                                Nothing ->
                                                    crawlFile env mvar docsNeed name path newTime buildID

                                                Just ((T.BED_Local oldPath oldTime deps _ lastChange _) as local) ->
                                                    if path /= oldPath || oldTime /= newTime || needsDocs then
                                                        crawlFile env mvar docsNeed name path newTime lastChange

                                                    else
                                                        crawlDeps env mvar deps (T.BB_SCached local)
                                        )

                    p1 :: p2 :: ps ->
                        IO.pure <| T.BB_SBadImport <| T.CREI_AmbiguousLocal (Utils.fpMakeRelative root p1) (Utils.fpMakeRelative root p2) (List.map (Utils.fpMakeRelative root) ps)

                    [] ->
                        case Dict.get identity name foreigns of
                            Just (Details.Foreign dep deps) ->
                                case deps of
                                    [] ->
                                        IO.pure <| T.BB_SForeign dep

                                    d :: ds ->
                                        IO.pure <| T.BB_SBadImport <| T.CREI_AmbiguousForeign dep d ds

                            Nothing ->
                                if Name.isKernel name && Parse.isKernel projectType then
                                    File.exists ("src/" ++ ModuleName.toFilePath name ++ ".js")
                                        |> IO.fmap
                                            (\exists ->
                                                if exists then
                                                    T.BB_SKernel

                                                else
                                                    T.BB_SBadImport T.CREI_NotFound
                                            )

                                else
                                    IO.pure <| T.BB_SBadImport T.CREI_NotFound
            )


crawlFile : Env -> T.MVar_BB_StatusDict -> T.BB_DocsNeed -> T.CEMN_Raw -> T.FilePath -> T.BF_Time -> T.BED_BuildID -> IO T.BB_Status
crawlFile ((Env _ root projectType _ buildID _ _) as env) mvar docsNeed expectedName path time lastChange =
    File.readUtf8 (Utils.fpForwardSlash root path)
        |> IO.bind
            (\source ->
                case Parse.fromByteString projectType source of
                    Err err ->
                        IO.pure <| T.BB_SBadSyntax path time source err

                    Ok ((T.CASTS_Module maybeActualName _ _ imports values _ _ _ _) as modul) ->
                        case maybeActualName of
                            Nothing ->
                                IO.pure <| T.BB_SBadSyntax path time source (T.CRES_ModuleNameUnspecified expectedName)

                            Just ((T.CRA_At _ actualName) as name) ->
                                if expectedName == actualName then
                                    let
                                        deps : List T.CDN_Name
                                        deps =
                                            List.map Src.getImportName imports

                                        local : T.BED_Local
                                        local =
                                            T.BED_Local path time deps (List.any isMain values) lastChange buildID
                                    in
                                    crawlDeps env mvar deps (T.BB_SChanged local source modul docsNeed)

                                else
                                    IO.pure <| T.BB_SBadSyntax path time source (T.CRES_ModuleNameMismatch expectedName name)
            )


isMain : T.CRA_Located T.CASTS_Value -> Bool
isMain (T.CRA_At _ (T.CASTS_Value (T.CRA_At _ name) _ _ _)) =
    name == Name.main_



-- CHECK MODULE


checkModule : Env -> T.BB_Dependencies -> T.MVar_BB_ResultDict -> T.CEMN_Raw -> T.BB_Status -> IO T.BB_BResult
checkModule ((Env _ root projectType _ _ _ _) as env) foreigns resultsMVar name status =
    case status of
        T.BB_SCached ((T.BED_Local path time deps hasMain lastChange lastCompile) as local) ->
            Utils.readMVar_BB_ResultDict resultsMVar
                |> IO.bind
                    (\results ->
                        checkDeps root results deps lastCompile
                            |> IO.bind
                                (\depsStatus ->
                                    case depsStatus of
                                        DepsChange ifaces ->
                                            File.readUtf8 path
                                                |> IO.bind
                                                    (\source ->
                                                        case Parse.fromByteString projectType source of
                                                            Ok modul ->
                                                                compile env (T.BB_DocsNeed False) local source ifaces modul

                                                            Err err ->
                                                                IO.pure <|
                                                                    T.BB_RProblem <|
                                                                        T.CRE_Module name path time source (T.CRE_BadSyntax err)
                                                    )

                                        DepsSame _ _ ->
                                            Utils.newMVar_BB_CachedInterface T.BB_Unneeded
                                                |> IO.fmap
                                                    (\mvar ->
                                                        T.BB_RCached hasMain lastChange mvar
                                                    )

                                        DepsBlock ->
                                            IO.pure T.BB_RBlocked

                                        DepsNotFound problems ->
                                            File.readUtf8 path
                                                |> IO.bind
                                                    (\source ->
                                                        IO.pure <|
                                                            T.BB_RProblem <|
                                                                T.CRE_Module name path time source <|
                                                                    case Parse.fromByteString projectType source of
                                                                        Ok (T.CASTS_Module _ _ _ imports _ _ _ _ _) ->
                                                                            T.CRE_BadImports (toImportErrors env results imports problems)

                                                                        Err err ->
                                                                            T.CRE_BadSyntax err
                                                    )
                                )
                    )

        T.BB_SChanged ((T.BED_Local path time deps _ _ lastCompile) as local) source ((T.CASTS_Module _ _ _ imports _ _ _ _ _) as modul) docsNeed ->
            Utils.readMVar_BB_ResultDict resultsMVar
                |> IO.bind
                    (\results ->
                        checkDeps root results deps lastCompile
                            |> IO.bind
                                (\depsStatus ->
                                    case depsStatus of
                                        DepsChange ifaces ->
                                            compile env docsNeed local source ifaces modul

                                        DepsSame same cached ->
                                            loadInterfaces root same cached
                                                |> IO.bind
                                                    (\maybeLoaded ->
                                                        case maybeLoaded of
                                                            Nothing ->
                                                                IO.pure T.BB_RBlocked

                                                            Just ifaces ->
                                                                compile env docsNeed local source ifaces modul
                                                    )

                                        DepsBlock ->
                                            IO.pure T.BB_RBlocked

                                        DepsNotFound problems ->
                                            IO.pure <|
                                                T.BB_RProblem <|
                                                    T.CRE_Module name path time source <|
                                                        T.CRE_BadImports (toImportErrors env results imports problems)
                                )
                    )

        T.BB_SBadImport importProblem ->
            IO.pure (T.BB_RNotFound importProblem)

        T.BB_SBadSyntax path time source err ->
            IO.pure <|
                T.BB_RProblem <|
                    T.CRE_Module name path time source <|
                        T.CRE_BadSyntax err

        T.BB_SForeign home ->
            case Utils.find ModuleName.toComparableCanonical (T.CEMN_Canonical home name) foreigns of
                T.CEI_Public iface ->
                    IO.pure (T.BB_RForeign iface)

                T.CEI_Private _ _ _ ->
                    crash <| "mistakenly seeing private interface for " ++ Pkg.toChars home ++ " " ++ name

        T.BB_SKernel ->
            IO.pure T.BB_RKernel



-- CHECK DEPS


type DepsStatus
    = DepsChange (Dict String T.CEMN_Raw T.CEI_Interface)
    | DepsSame (List T.BB_Dep) (List CDep)
    | DepsBlock
    | DepsNotFound (NE.Nonempty ( T.CEMN_Raw, T.CREI_Problem ))


checkDeps : T.FilePath -> T.BB_ResultDict -> List T.CEMN_Raw -> T.BED_BuildID -> IO DepsStatus
checkDeps root results deps lastCompile =
    checkDepsHelp root results deps [] [] [] [] False 0 lastCompile


type alias CDep =
    ( T.CEMN_Raw, T.MVar_BB_CachedInterface )


checkDepsHelp : T.FilePath -> T.BB_ResultDict -> List T.CEMN_Raw -> List T.BB_Dep -> List T.BB_Dep -> List CDep -> List ( T.CEMN_Raw, T.CREI_Problem ) -> Bool -> T.BED_BuildID -> T.BED_BuildID -> IO DepsStatus
checkDepsHelp root results deps new same cached importProblems isBlocked lastDepChange lastCompile =
    case deps of
        dep :: otherDeps ->
            Utils.readMVar_BB_BResult (Utils.find identity dep results)
                |> IO.bind
                    (\result ->
                        case result of
                            T.BB_RNew (T.BED_Local _ _ _ _ lastChange _) iface _ _ ->
                                checkDepsHelp root results otherDeps (( dep, iface ) :: new) same cached importProblems isBlocked (max lastChange lastDepChange) lastCompile

                            T.BB_RSame (T.BED_Local _ _ _ _ lastChange _) iface _ _ ->
                                checkDepsHelp root results otherDeps new (( dep, iface ) :: same) cached importProblems isBlocked (max lastChange lastDepChange) lastCompile

                            T.BB_RCached _ lastChange mvar ->
                                checkDepsHelp root results otherDeps new same (( dep, mvar ) :: cached) importProblems isBlocked (max lastChange lastDepChange) lastCompile

                            T.BB_RNotFound prob ->
                                checkDepsHelp root results otherDeps new same cached (( dep, prob ) :: importProblems) True lastDepChange lastCompile

                            T.BB_RProblem _ ->
                                checkDepsHelp root results otherDeps new same cached importProblems True lastDepChange lastCompile

                            T.BB_RBlocked ->
                                checkDepsHelp root results otherDeps new same cached importProblems True lastDepChange lastCompile

                            T.BB_RForeign iface ->
                                checkDepsHelp root results otherDeps new (( dep, iface ) :: same) cached importProblems isBlocked lastDepChange lastCompile

                            T.BB_RKernel ->
                                checkDepsHelp root results otherDeps new same cached importProblems isBlocked lastDepChange lastCompile
                    )

        [] ->
            case List.reverse importProblems of
                p :: ps ->
                    IO.pure <| DepsNotFound (NE.Nonempty p ps)

                [] ->
                    if isBlocked then
                        IO.pure <| DepsBlock

                    else if List.isEmpty new && lastDepChange <= lastCompile then
                        IO.pure <| DepsSame same cached

                    else
                        loadInterfaces root same cached
                            |> IO.bind
                                (\maybeLoaded ->
                                    case maybeLoaded of
                                        Nothing ->
                                            IO.pure DepsBlock

                                        Just ifaces ->
                                            IO.pure <| DepsChange <| Dict.union (Dict.fromList identity new) ifaces
                                )



-- TO IMPORT ERROR


toImportErrors : Env -> T.BB_ResultDict -> List T.CASTS_Import -> NE.Nonempty ( T.CEMN_Raw, T.CREI_Problem ) -> NE.Nonempty T.CREI_Error
toImportErrors (Env _ _ _ _ _ locals foreigns) results imports problems =
    let
        knownModules : EverySet.EverySet String T.CEMN_Raw
        knownModules =
            EverySet.fromList identity
                (List.concat
                    [ Dict.keys compare foreigns
                    , Dict.keys compare locals
                    , Dict.keys compare results
                    ]
                )

        unimportedModules : EverySet.EverySet String T.CEMN_Raw
        unimportedModules =
            EverySet.diff knownModules (EverySet.fromList identity (List.map Src.getImportName imports))

        regionDict : Dict String T.CDN_Name T.CRA_Region
        regionDict =
            Dict.fromList identity (List.map (\(T.CASTS_Import (T.CRA_At region name) _ _) -> ( name, region )) imports)

        toError : ( T.CDN_Name, T.CREI_Problem ) -> T.CREI_Error
        toError ( name, problem ) =
            T.CREI_Error (Utils.find identity name regionDict) name unimportedModules problem
    in
    NE.map toError problems



-- LOAD CACHED INTERFACES


loadInterfaces : T.FilePath -> List T.BB_Dep -> List CDep -> IO (Maybe (Dict String T.CEMN_Raw T.CEI_Interface))
loadInterfaces root same cached =
    Utils.listTraverse (fork_MaybeDep << loadInterface root) cached
        |> IO.bind
            (\loading ->
                Utils.listTraverse Utils.readMVar_MaybeDep loading
                    |> IO.bind
                        (\maybeLoaded ->
                            case Utils.sequenceListMaybe maybeLoaded of
                                Nothing ->
                                    IO.pure Nothing

                                Just loaded ->
                                    IO.pure <| Just <| Dict.union (Dict.fromList identity loaded) (Dict.fromList identity same)
                        )
            )


loadInterface : T.FilePath -> CDep -> IO (Maybe T.BB_Dep)
loadInterface root ( name, ciMvar ) =
    Utils.takeMVar_BB_CachedInterface ciMvar
        |> IO.bind
            (\cachedInterface ->
                case cachedInterface of
                    T.BB_Corrupted ->
                        Utils.putMVar_BB_CachedInterface ciMvar cachedInterface
                            |> IO.fmap (\_ -> Nothing)

                    T.BB_Loaded iface ->
                        Utils.putMVar_BB_CachedInterface ciMvar cachedInterface
                            |> IO.fmap (\_ -> Just ( name, iface ))

                    T.BB_Unneeded ->
                        File.readBinary I.interfaceDecoder (Stuff.elmi root name)
                            |> IO.bind
                                (\maybeIface ->
                                    case maybeIface of
                                        Nothing ->
                                            Utils.putMVar_BB_CachedInterface ciMvar T.BB_Corrupted
                                                |> IO.fmap (\_ -> Nothing)

                                        Just iface ->
                                            Utils.putMVar_BB_CachedInterface ciMvar (T.BB_Loaded iface)
                                                |> IO.fmap (\_ -> Just ( name, iface ))
                                )
            )



-- CHECK PROJECT


checkMidpoint : T.MVar_Maybe_BB_Dependencies -> Dict String T.CEMN_Raw T.BB_Status -> IO (Result T.BRE_BuildProjectProblem T.BB_Dependencies)
checkMidpoint dmvar statuses =
    case checkForCycles statuses of
        Nothing ->
            Utils.readMVar_Maybe_BB_Dependencies dmvar
                |> IO.fmap
                    (\maybeForeigns ->
                        case maybeForeigns of
                            Nothing ->
                                Err T.BRE_BP_CannotLoadDependencies

                            Just fs ->
                                Ok fs
                    )

        Just (NE.Nonempty name names) ->
            Utils.readMVar_Maybe_BB_Dependencies dmvar
                |> IO.fmap (\_ -> Err (T.BRE_BP_Cycle name names))


checkMidpointAndRoots : T.MVar_Maybe_BB_Dependencies -> Dict String T.CEMN_Raw T.BB_Status -> NE.Nonempty T.BB_RootStatus -> IO (Result T.BRE_BuildProjectProblem T.BB_Dependencies)
checkMidpointAndRoots dmvar statuses sroots =
    case checkForCycles statuses of
        Nothing ->
            case checkUniqueRoots statuses sroots of
                Nothing ->
                    Utils.readMVar_Maybe_BB_Dependencies dmvar
                        |> IO.bind
                            (\maybeForeigns ->
                                case maybeForeigns of
                                    Nothing ->
                                        IO.pure (Err T.BRE_BP_CannotLoadDependencies)

                                    Just fs ->
                                        IO.pure (Ok fs)
                            )

                Just problem ->
                    Utils.readMVar_Maybe_BB_Dependencies dmvar
                        |> IO.fmap (\_ -> Err problem)

        Just (NE.Nonempty name names) ->
            Utils.readMVar_Maybe_BB_Dependencies dmvar
                |> IO.fmap (\_ -> Err (T.BRE_BP_Cycle name names))



-- CHECK FOR CYCLES


checkForCycles : Dict String T.CEMN_Raw T.BB_Status -> Maybe (NE.Nonempty T.CEMN_Raw)
checkForCycles modules =
    let
        graph : List Node
        graph =
            Dict.foldr compare addToGraph [] modules

        sccs : List (Graph.SCC T.CEMN_Raw)
        sccs =
            Graph.stronglyConnComp graph
    in
    checkForCyclesHelp sccs


checkForCyclesHelp : List (Graph.SCC T.CEMN_Raw) -> Maybe (NE.Nonempty T.CEMN_Raw)
checkForCyclesHelp sccs =
    case sccs of
        [] ->
            Nothing

        scc :: otherSccs ->
            case scc of
                Graph.AcyclicSCC _ ->
                    checkForCyclesHelp otherSccs

                Graph.CyclicSCC [] ->
                    checkForCyclesHelp otherSccs

                Graph.CyclicSCC (m :: ms) ->
                    Just (NE.Nonempty m ms)


type alias Node =
    ( T.CEMN_Raw, T.CEMN_Raw, List T.CEMN_Raw )


addToGraph : T.CEMN_Raw -> T.BB_Status -> List Node -> List Node
addToGraph name status graph =
    let
        dependencies : List T.CEMN_Raw
        dependencies =
            case status of
                T.BB_SCached (T.BED_Local _ _ deps _ _ _) ->
                    deps

                T.BB_SChanged (T.BED_Local _ _ deps _ _ _) _ _ _ ->
                    deps

                T.BB_SBadImport _ ->
                    []

                T.BB_SBadSyntax _ _ _ _ ->
                    []

                T.BB_SForeign _ ->
                    []

                T.BB_SKernel ->
                    []
    in
    ( name, name, dependencies ) :: graph



-- CHECK UNIQUE ROOTS


checkUniqueRoots : Dict String T.CEMN_Raw T.BB_Status -> NE.Nonempty T.BB_RootStatus -> Maybe T.BRE_BuildProjectProblem
checkUniqueRoots insides sroots =
    let
        outsidesDict : Dict String T.CEMN_Raw (OneOrMore.OneOrMore T.FilePath)
        outsidesDict =
            Utils.mapFromListWith identity OneOrMore.more (List.filterMap rootStatusToNamePathPair (NE.toList sroots))
    in
    case Utils.mapTraverseWithKeyResult identity compare checkOutside outsidesDict of
        Err problem ->
            Just problem

        Ok outsides ->
            case Utils.sequenceDictResult_ identity compare (Utils.mapIntersectionWithKey identity compare checkInside outsides insides) of
                Ok () ->
                    Nothing

                Err problem ->
                    Just problem


rootStatusToNamePathPair : T.BB_RootStatus -> Maybe ( T.CEMN_Raw, OneOrMore.OneOrMore T.FilePath )
rootStatusToNamePathPair sroot =
    case sroot of
        T.BB_SInside _ ->
            Nothing

        T.BB_SOutsideOk (T.BED_Local path _ _ _ _ _) _ modul ->
            Just ( Src.getName modul, OneOrMore.one path )

        T.BB_SOutsideErr _ ->
            Nothing


checkOutside : T.CEMN_Raw -> OneOrMore.OneOrMore T.FilePath -> Result T.BRE_BuildProjectProblem T.FilePath
checkOutside name paths =
    case OneOrMore.destruct NE.Nonempty paths of
        NE.Nonempty p [] ->
            Ok p

        NE.Nonempty p1 (p2 :: _) ->
            Err (T.BRE_BP_RootNameDuplicate name p1 p2)


checkInside : T.CEMN_Raw -> T.FilePath -> T.BB_Status -> Result T.BRE_BuildProjectProblem ()
checkInside name p1 status =
    case status of
        T.BB_SCached (T.BED_Local p2 _ _ _ _ _) ->
            Err (T.BRE_BP_RootNameDuplicate name p1 p2)

        T.BB_SChanged (T.BED_Local p2 _ _ _ _ _) _ _ _ ->
            Err (T.BRE_BP_RootNameDuplicate name p1 p2)

        T.BB_SBadImport _ ->
            Ok ()

        T.BB_SBadSyntax _ _ _ _ ->
            Ok ()

        T.BB_SForeign _ ->
            Ok ()

        T.BB_SKernel ->
            Ok ()



-- COMPILE MODULE


compile : Env -> T.BB_DocsNeed -> T.BED_Local -> String -> Dict String T.CEMN_Raw T.CEI_Interface -> T.CASTS_Module -> IO T.BB_BResult
compile (Env key root projectType _ buildID _ _) docsNeed (T.BED_Local path time deps main lastChange _) source ifaces modul =
    let
        pkg : T.CEP_Name
        pkg =
            projectTypeToPkg projectType
    in
    Compile.compile pkg ifaces modul
        |> IO.bind
            (\result ->
                case result of
                    Ok (Compile.Artifacts canonical annotations objects) ->
                        case makeDocs docsNeed canonical of
                            Err err ->
                                IO.pure <|
                                    T.BB_RProblem <|
                                        T.CRE_Module (Src.getName modul) path time source (T.CRE_BadDocs err)

                            Ok docs ->
                                let
                                    name : T.CDN_Name
                                    name =
                                        Src.getName modul

                                    iface : T.CEI_Interface
                                    iface =
                                        I.fromModule pkg canonical annotations

                                    elmi : String
                                    elmi =
                                        Stuff.elmi root name
                                in
                                File.writeBinary Opt.localGraphEncoder (Stuff.elmo root name) objects
                                    |> IO.bind
                                        (\_ ->
                                            File.readBinary I.interfaceDecoder elmi
                                                |> IO.bind
                                                    (\maybeOldi ->
                                                        case maybeOldi of
                                                            Just oldi ->
                                                                if oldi == iface then
                                                                    -- iface should be fully forced by equality check
                                                                    Reporting.report key T.BR_BDone
                                                                        |> IO.fmap
                                                                            (\_ ->
                                                                                let
                                                                                    local : T.BED_Local
                                                                                    local =
                                                                                        T.BED_Local path time deps main lastChange buildID
                                                                                in
                                                                                T.BB_RSame local iface objects docs
                                                                            )

                                                                else
                                                                    File.writeBinary I.interfaceEncoder elmi iface
                                                                        |> IO.bind
                                                                            (\_ ->
                                                                                Reporting.report key T.BR_BDone
                                                                                    |> IO.fmap
                                                                                        (\_ ->
                                                                                            let
                                                                                                local : T.BED_Local
                                                                                                local =
                                                                                                    T.BED_Local path time deps main buildID buildID
                                                                                            in
                                                                                            T.BB_RNew local iface objects docs
                                                                                        )
                                                                            )

                                                            _ ->
                                                                -- iface may be lazy still
                                                                File.writeBinary I.interfaceEncoder elmi iface
                                                                    |> IO.bind
                                                                        (\_ ->
                                                                            Reporting.report key T.BR_BDone
                                                                                |> IO.fmap
                                                                                    (\_ ->
                                                                                        let
                                                                                            local : T.BED_Local
                                                                                            local =
                                                                                                T.BED_Local path time deps main buildID buildID
                                                                                        in
                                                                                        T.BB_RNew local iface objects docs
                                                                                    )
                                                                        )
                                                    )
                                        )

                    Err err ->
                        IO.pure <|
                            T.BB_RProblem <|
                                T.CRE_Module (Src.getName modul) path time source err
            )


projectTypeToPkg : Parse.ProjectType -> T.CEP_Name
projectTypeToPkg projectType =
    case projectType of
        Parse.Package pkg ->
            pkg

        Parse.Application ->
            Pkg.dummyName



-- WRITE DETAILS


writeDetails : T.FilePath -> Details.Details -> Dict String T.CEMN_Raw T.BB_BResult -> IO ()
writeDetails root (Details.Details time outline buildID locals foreigns extras) results =
    File.writeBinary Details.detailsEncoder (Stuff.details root) <|
        Details.Details time outline buildID (Dict.foldr compare addNewLocal locals results) foreigns extras


addNewLocal : T.CEMN_Raw -> T.BB_BResult -> Dict String T.CEMN_Raw T.BED_Local -> Dict String T.CEMN_Raw T.BED_Local
addNewLocal name result locals =
    case result of
        T.BB_RNew local _ _ _ ->
            Dict.insert identity name local locals

        T.BB_RSame local _ _ _ ->
            Dict.insert identity name local locals

        T.BB_RCached _ _ _ ->
            locals

        T.BB_RNotFound _ ->
            locals

        T.BB_RProblem _ ->
            locals

        T.BB_RBlocked ->
            locals

        T.BB_RForeign _ ->
            locals

        T.BB_RKernel ->
            locals



-- FINALIZE EXPOSED


finalizeExposed : T.FilePath -> DocsGoal docs -> NE.Nonempty T.CEMN_Raw -> Dict String T.CEMN_Raw T.BB_BResult -> IO (Result T.BRE_BuildProblem docs)
finalizeExposed root docsGoal exposed results =
    case List.foldr (addImportProblems results) [] (NE.toList exposed) of
        p :: ps ->
            IO.pure <| Err <| T.BRE_BuildProjectProblem (T.BRE_BP_MissingExposed (NE.Nonempty p ps))

        [] ->
            case Dict.foldr compare (\_ -> addErrors) [] results of
                [] ->
                    IO.fmap Ok (finalizeDocs docsGoal results)

                e :: es ->
                    IO.pure <| Err <| T.BRE_BuildBadModules root e es


addErrors : T.BB_BResult -> List T.CRE_Module -> List T.CRE_Module
addErrors result errors =
    case result of
        T.BB_RNew _ _ _ _ ->
            errors

        T.BB_RSame _ _ _ _ ->
            errors

        T.BB_RCached _ _ _ ->
            errors

        T.BB_RNotFound _ ->
            errors

        T.BB_RProblem e ->
            e :: errors

        T.BB_RBlocked ->
            errors

        T.BB_RForeign _ ->
            errors

        T.BB_RKernel ->
            errors


addImportProblems : Dict String T.CEMN_Raw T.BB_BResult -> T.CEMN_Raw -> List ( T.CEMN_Raw, T.CREI_Problem ) -> List ( T.CEMN_Raw, T.CREI_Problem )
addImportProblems results name problems =
    case Utils.find identity name results of
        T.BB_RNew _ _ _ _ ->
            problems

        T.BB_RSame _ _ _ _ ->
            problems

        T.BB_RCached _ _ _ ->
            problems

        T.BB_RNotFound p ->
            ( name, p ) :: problems

        T.BB_RProblem _ ->
            problems

        T.BB_RBlocked ->
            problems

        T.BB_RForeign _ ->
            problems

        T.BB_RKernel ->
            problems



-- DOCS


type DocsGoal docs
    = KeepDocs (Dict String T.CEMN_Raw T.BB_BResult -> docs)
    | WriteDocs (Dict String T.CEMN_Raw T.BB_BResult -> IO docs)
    | IgnoreDocs docs


keepDocs : DocsGoal (Dict String T.CEMN_Raw T.CED_Module)
keepDocs =
    KeepDocs (Utils.mapMapMaybe identity compare toDocs)


writeDocs : T.FilePath -> DocsGoal ()
writeDocs path =
    WriteDocs (E.writeUgly path << Docs.encode << Utils.mapMapMaybe identity compare toDocs)


ignoreDocs : DocsGoal ()
ignoreDocs =
    IgnoreDocs ()


toDocsNeed : DocsGoal a -> T.BB_DocsNeed
toDocsNeed goal =
    case goal of
        IgnoreDocs _ ->
            T.BB_DocsNeed False

        WriteDocs _ ->
            T.BB_DocsNeed True

        KeepDocs _ ->
            T.BB_DocsNeed True


makeDocs : T.BB_DocsNeed -> Can.Module -> Result T.CRED_Error (Maybe T.CED_Module)
makeDocs (T.BB_DocsNeed isNeeded) modul =
    if isNeeded then
        case Docs.fromModule modul of
            Ok docs ->
                Ok (Just docs)

            Err err ->
                Err err

    else
        Ok Nothing


finalizeDocs : DocsGoal docs -> Dict String T.CEMN_Raw T.BB_BResult -> IO docs
finalizeDocs goal results =
    case goal of
        KeepDocs f ->
            IO.pure <| f results

        WriteDocs f ->
            f results

        IgnoreDocs val ->
            IO.pure val


toDocs : T.BB_BResult -> Maybe T.CED_Module
toDocs result =
    case result of
        T.BB_RNew _ _ _ d ->
            d

        T.BB_RSame _ _ _ d ->
            d

        T.BB_RCached _ _ _ ->
            Nothing

        T.BB_RNotFound _ ->
            Nothing

        T.BB_RProblem _ ->
            Nothing

        T.BB_RBlocked ->
            Nothing

        T.BB_RForeign _ ->
            Nothing

        T.BB_RKernel ->
            Nothing



-------------------------------------------------------------------------------
------ NOW FOR SOME REPL STUFF -------------------------------------------------
--------------------------------------------------------------------------------
-- FROM REPL


type ReplArtifacts
    = ReplArtifacts T.CEMN_Canonical (List T.BB_Module) T.CRRTL_Localizer (Dict String T.CDN_Name T.CASTC_Annotation)


fromRepl : T.FilePath -> Details.Details -> String -> IO (Result Exit.Repl ReplArtifacts)
fromRepl root details source =
    makeEnv Reporting.ignorer root details
        |> IO.bind
            (\((Env _ _ projectType _ _ _ _) as env) ->
                case Parse.fromByteString projectType source of
                    Err syntaxError ->
                        IO.pure <| Err <| Exit.ReplBadInput source <| T.CRE_BadSyntax syntaxError

                    Ok ((T.CASTS_Module _ _ _ imports _ _ _ _ _) as modul) ->
                        Details.loadInterfaces root details
                            |> IO.bind
                                (\dmvar ->
                                    let
                                        deps : List T.CDN_Name
                                        deps =
                                            List.map Src.getImportName imports
                                    in
                                    Utils.newMVar_BB_StatusDict Dict.empty
                                        |> IO.bind
                                            (\mvar ->
                                                crawlDeps env mvar deps ()
                                                    |> IO.bind
                                                        (\_ ->
                                                            IO.bind (Utils.mapTraverse identity compare Utils.readMVar_BB_Status) (Utils.readMVar_BB_StatusDict mvar)
                                                                |> IO.bind
                                                                    (\statuses ->
                                                                        checkMidpoint dmvar statuses
                                                                            |> IO.bind
                                                                                (\midpoint ->
                                                                                    case midpoint of
                                                                                        Err problem ->
                                                                                            IO.pure <| Err <| Exit.ReplProjectProblem problem

                                                                                        Ok foreigns ->
                                                                                            Utils.newEmptyMVar_BB_ResultDict
                                                                                                |> IO.bind
                                                                                                    (\rmvar ->
                                                                                                        forkWithKey_BB_BResult identity compare (checkModule env foreigns rmvar) statuses
                                                                                                            |> IO.bind
                                                                                                                (\resultMVars ->
                                                                                                                    Utils.putMVar_BB_ResultDict rmvar resultMVars
                                                                                                                        |> IO.bind
                                                                                                                            (\_ ->
                                                                                                                                Utils.mapTraverse identity compare Utils.readMVar_BB_BResult resultMVars
                                                                                                                                    |> IO.bind
                                                                                                                                        (\results ->
                                                                                                                                            writeDetails root details results
                                                                                                                                                |> IO.bind
                                                                                                                                                    (\_ ->
                                                                                                                                                        checkDeps root resultMVars deps 0
                                                                                                                                                            |> IO.bind
                                                                                                                                                                (\depsStatus ->
                                                                                                                                                                    finalizeReplArtifacts env source modul depsStatus resultMVars results
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
                                )
            )


finalizeReplArtifacts : Env -> String -> T.CASTS_Module -> DepsStatus -> T.BB_ResultDict -> Dict String T.CEMN_Raw T.BB_BResult -> IO (Result Exit.Repl ReplArtifacts)
finalizeReplArtifacts ((Env _ root projectType _ _ _ _) as env) source ((T.CASTS_Module _ _ _ imports _ _ _ _ _) as modul) depsStatus resultMVars results =
    let
        pkg : T.CEP_Name
        pkg =
            projectTypeToPkg projectType

        compileInput : Dict String T.CEMN_Raw T.CEI_Interface -> IO (Result Exit.Repl ReplArtifacts)
        compileInput ifaces =
            Compile.compile pkg ifaces modul
                |> IO.fmap
                    (\result ->
                        case result of
                            Ok (Compile.Artifacts ((Can.Module name _ _ _ _ _ _ _) as canonical) annotations objects) ->
                                let
                                    h : T.CEMN_Canonical
                                    h =
                                        name

                                    m : T.BB_Module
                                    m =
                                        T.BB_Fresh (Src.getName modul) (I.fromModule pkg canonical annotations) objects

                                    ms : List T.BB_Module
                                    ms =
                                        Dict.foldr compare addInside [] results
                                in
                                Ok <| ReplArtifacts h (m :: ms) (L.fromModule modul) annotations

                            Err errors ->
                                Err <| Exit.ReplBadInput source errors
                    )
    in
    case depsStatus of
        DepsChange ifaces ->
            compileInput ifaces

        DepsSame same cached ->
            loadInterfaces root same cached
                |> IO.bind
                    (\maybeLoaded ->
                        case maybeLoaded of
                            Just ifaces ->
                                compileInput ifaces

                            Nothing ->
                                IO.pure <| Err <| Exit.ReplBadCache
                    )

        DepsBlock ->
            case Dict.foldr compare (\_ -> addErrors) [] results of
                [] ->
                    IO.pure <| Err <| Exit.ReplBlocked

                e :: es ->
                    IO.pure <| Err <| Exit.ReplBadLocalDeps root e es

        DepsNotFound problems ->
            IO.pure <|
                Err <|
                    Exit.ReplBadInput source <|
                        T.CRE_BadImports <|
                            toImportErrors env resultMVars imports problems



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
------ AFTER THIS, EVERYTHING IS ABOUT HANDLING MODULES GIVEN BY FILEPATH ------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- FIND ROOT


findRoots : Env -> NE.Nonempty T.FilePath -> IO (Result T.BRE_BuildProjectProblem (NE.Nonempty T.BB_RootLocation))
findRoots env paths =
    Utils.nonEmptyListTraverse (fork_Result_BuildProjectProblem_RootInfo << getRootInfo env) paths
        |> IO.bind
            (\mvars ->
                Utils.nonEmptyListTraverse Utils.readMVar_Result_BuildProjectProblem_RootInfo mvars
                    |> IO.bind
                        (\einfos ->
                            IO.pure (Result.andThen checkRoots (Utils.sequenceNonemptyListResult einfos))
                        )
            )


checkRoots : NE.Nonempty T.BB_RootInfo -> Result T.BRE_BuildProjectProblem (NE.Nonempty T.BB_RootLocation)
checkRoots infos =
    let
        toOneOrMore : T.BB_RootInfo -> ( T.FilePath, OneOrMore.OneOrMore T.BB_RootInfo )
        toOneOrMore ((T.BB_RootInfo absolute _ _) as loc) =
            ( absolute, OneOrMore.one loc )

        fromOneOrMore : T.BB_RootInfo -> List T.BB_RootInfo -> Result T.BRE_BuildProjectProblem ()
        fromOneOrMore (T.BB_RootInfo _ relative _) locs =
            case locs of
                [] ->
                    Ok ()

                (T.BB_RootInfo _ relative2 _) :: _ ->
                    Err (T.BRE_BP_MainPathDuplicate relative relative2)
    in
    Result.map (\_ -> NE.map (\(T.BB_RootInfo _ _ location) -> location) infos) <|
        Utils.mapTraverseResult identity compare (OneOrMore.destruct fromOneOrMore) <|
            Utils.mapFromListWith identity OneOrMore.more <|
                List.map toOneOrMore (NE.toList infos)



-- ROOT INFO


getRootInfo : Env -> T.FilePath -> IO (Result T.BRE_BuildProjectProblem T.BB_RootInfo)
getRootInfo env path =
    File.exists path
        |> IO.bind
            (\exists ->
                if exists then
                    IO.bind (getRootInfoHelp env path) (Utils.dirCanonicalizePath path)

                else
                    IO.pure (Err (T.BRE_BP_PathUnknown path))
            )


getRootInfoHelp : Env -> T.FilePath -> T.FilePath -> IO (Result T.BRE_BuildProjectProblem T.BB_RootInfo)
getRootInfoHelp (Env _ _ _ srcDirs _ _ _) path absolutePath =
    let
        ( dirs, file ) =
            Utils.fpSplitFileName absolutePath

        ( final, ext ) =
            Utils.fpSplitExtension file
    in
    if ext /= ".elm" then
        IO.pure <| Err <| T.BRE_BP_WithBadExtension path

    else
        let
            absoluteSegments : List String
            absoluteSegments =
                Utils.fpSplitDirectories dirs ++ [ final ]
        in
        case List.filterMap (isInsideSrcDirByPath absoluteSegments) srcDirs of
            [] ->
                IO.pure <| Ok <| T.BB_RootInfo absolutePath path (T.BB_LOutside path)

            [ ( _, Ok names ) ] ->
                let
                    name : String
                    name =
                        String.join "." names
                in
                Utils.filterM (isInsideSrcDirByName names) srcDirs
                    |> IO.bind
                        (\matchingDirs ->
                            case matchingDirs of
                                d1 :: d2 :: _ ->
                                    let
                                        p1 : T.FilePath
                                        p1 =
                                            addRelative d1 (Utils.fpJoinPath names ++ ".elm")

                                        p2 : T.FilePath
                                        p2 =
                                            addRelative d2 (Utils.fpJoinPath names ++ ".elm")
                                    in
                                    IO.pure <| Err <| T.BRE_BP_RootNameDuplicate name p1 p2

                                _ ->
                                    IO.pure <| Ok <| T.BB_RootInfo absolutePath path (T.BB_LInside name)
                        )

            [ ( s, Err names ) ] ->
                IO.pure <| Err <| T.BRE_BP_RootNameInvalid path s names

            ( s1, _ ) :: ( s2, _ ) :: _ ->
                IO.pure <| Err <| T.BRE_BP_WithAmbiguousSrcDir path s1 s2


isInsideSrcDirByName : List String -> AbsoluteSrcDir -> IO Bool
isInsideSrcDirByName names srcDir =
    File.exists (addRelative srcDir (Utils.fpJoinPath names ++ ".elm"))


isInsideSrcDirByPath : List String -> AbsoluteSrcDir -> Maybe ( T.FilePath, Result (List String) (List String) )
isInsideSrcDirByPath segments (AbsoluteSrcDir srcDir) =
    dropPrefix (Utils.fpSplitDirectories srcDir) segments
        |> Maybe.map
            (\names ->
                if List.all isGoodName names then
                    ( srcDir, Ok names )

                else
                    ( srcDir, Err names )
            )


isGoodName : String -> Bool
isGoodName name =
    case String.toList name of
        [] ->
            False

        char :: chars ->
            Char.isUpper char && List.all (\c -> Char.isAlphaNum c || c == '_') chars



-- INVARIANT: Dir.canonicalizePath has been run on both inputs


dropPrefix : List T.FilePath -> List T.FilePath -> Maybe (List T.FilePath)
dropPrefix roots paths =
    case roots of
        [] ->
            Just paths

        r :: rs ->
            case paths of
                [] ->
                    Nothing

                p :: ps ->
                    if r == p then
                        dropPrefix rs ps

                    else
                        Nothing



-- CRAWL ROOTS


crawlRoot : Env -> T.MVar_BB_StatusDict -> T.BB_RootLocation -> IO T.BB_RootStatus
crawlRoot ((Env _ _ projectType _ buildID _ _) as env) mvar root =
    case root of
        T.BB_LInside name ->
            Utils.newEmptyMVar_BB_Status
                |> IO.bind
                    (\statusMVar ->
                        Utils.takeMVar_BB_StatusDict mvar
                            |> IO.bind
                                (\statusDict ->
                                    Utils.putMVar_BB_StatusDict mvar (Dict.insert identity name statusMVar statusDict)
                                        |> IO.bind
                                            (\_ ->
                                                IO.bind (Utils.putMVar_BB_Status statusMVar) (crawlModule env mvar (T.BB_DocsNeed False) name)
                                                    |> IO.fmap (\_ -> T.BB_SInside name)
                                            )
                                )
                    )

        T.BB_LOutside path ->
            File.getTime path
                |> IO.bind
                    (\time ->
                        File.readUtf8 path
                            |> IO.bind
                                (\source ->
                                    case Parse.fromByteString projectType source of
                                        Ok ((T.CASTS_Module _ _ _ imports values _ _ _ _) as modul) ->
                                            let
                                                deps : List T.CDN_Name
                                                deps =
                                                    List.map Src.getImportName imports

                                                local : T.BED_Local
                                                local =
                                                    T.BED_Local path time deps (List.any isMain values) buildID buildID
                                            in
                                            crawlDeps env mvar deps (T.BB_SOutsideOk local source modul)

                                        Err syntaxError ->
                                            IO.pure <|
                                                T.BB_SOutsideErr <|
                                                    T.CRE_Module "???" path time source (T.CRE_BadSyntax syntaxError)
                                )
                    )



-- CHECK ROOTS


checkRoot : Env -> T.BB_ResultDict -> T.BB_RootStatus -> IO T.BB_RootResult
checkRoot ((Env _ root _ _ _ _ _) as env) results rootStatus =
    case rootStatus of
        T.BB_SInside name ->
            IO.pure (T.BB_RInside name)

        T.BB_SOutsideErr err ->
            IO.pure (T.BB_ROutsideErr err)

        T.BB_SOutsideOk ((T.BED_Local path time deps _ _ lastCompile) as local) source ((T.CASTS_Module _ _ _ imports _ _ _ _ _) as modul) ->
            checkDeps root results deps lastCompile
                |> IO.bind
                    (\depsStatus ->
                        case depsStatus of
                            DepsChange ifaces ->
                                compileOutside env local source ifaces modul

                            DepsSame same cached ->
                                loadInterfaces root same cached
                                    |> IO.bind
                                        (\maybeLoaded ->
                                            case maybeLoaded of
                                                Nothing ->
                                                    IO.pure T.BB_ROutsideBlocked

                                                Just ifaces ->
                                                    compileOutside env local source ifaces modul
                                        )

                            DepsBlock ->
                                IO.pure T.BB_ROutsideBlocked

                            DepsNotFound problems ->
                                IO.pure <|
                                    T.BB_ROutsideErr <|
                                        T.CRE_Module (Src.getName modul) path time source <|
                                            T.CRE_BadImports (toImportErrors env results imports problems)
                    )


compileOutside : Env -> T.BED_Local -> String -> Dict String T.CEMN_Raw T.CEI_Interface -> T.CASTS_Module -> IO T.BB_RootResult
compileOutside (Env key _ projectType _ _ _ _) (T.BED_Local path time _ _ _ _) source ifaces modul =
    let
        pkg : T.CEP_Name
        pkg =
            projectTypeToPkg projectType

        name : T.CDN_Name
        name =
            Src.getName modul
    in
    Compile.compile pkg ifaces modul
        |> IO.bind
            (\result ->
                case result of
                    Ok (Compile.Artifacts canonical annotations objects) ->
                        Reporting.report key T.BR_BDone
                            |> IO.fmap (\_ -> T.BB_ROutsideOk name (I.fromModule pkg canonical annotations) objects)

                    Err errors ->
                        IO.pure <| T.BB_ROutsideErr <| T.CRE_Module name path time source errors
            )



-- TO ARTIFACTS


toArtifacts : Env -> T.BB_Dependencies -> Dict String T.CEMN_Raw T.BB_BResult -> NE.Nonempty T.BB_RootResult -> Result T.BRE_BuildProblem T.BB_Artifacts
toArtifacts (Env _ root projectType _ _ _ _) foreigns results rootResults =
    case gatherProblemsOrMains results rootResults of
        Err (NE.Nonempty e es) ->
            Err (T.BRE_BuildBadModules root e es)

        Ok roots ->
            Ok <|
                T.BB_Artifacts (projectTypeToPkg projectType) foreigns roots <|
                    Dict.foldr compare addInside (NE.foldr addOutside [] rootResults) results


gatherProblemsOrMains : Dict String T.CEMN_Raw T.BB_BResult -> NE.Nonempty T.BB_RootResult -> Result (NE.Nonempty T.CRE_Module) (NE.Nonempty T.BB_Root)
gatherProblemsOrMains results (NE.Nonempty rootResult rootResults) =
    let
        addResult : T.BB_RootResult -> ( List T.CRE_Module, List T.BB_Root ) -> ( List T.CRE_Module, List T.BB_Root )
        addResult result ( es, roots ) =
            case result of
                T.BB_RInside n ->
                    ( es, T.BB_Inside n :: roots )

                T.BB_ROutsideOk n i o ->
                    ( es, T.BB_Outside n i o :: roots )

                T.BB_ROutsideErr e ->
                    ( e :: es, roots )

                T.BB_ROutsideBlocked ->
                    ( es, roots )

        errors : List T.CRE_Module
        errors =
            Dict.foldr compare (\_ -> addErrors) [] results
    in
    case ( rootResult, List.foldr addResult ( errors, [] ) rootResults ) of
        ( T.BB_RInside n, ( [], ms ) ) ->
            Ok (NE.Nonempty (T.BB_Inside n) ms)

        ( T.BB_RInside _, ( e :: es, _ ) ) ->
            Err (NE.Nonempty e es)

        ( T.BB_ROutsideOk n i o, ( [], ms ) ) ->
            Ok (NE.Nonempty (T.BB_Outside n i o) ms)

        ( T.BB_ROutsideOk _ _ _, ( e :: es, _ ) ) ->
            Err (NE.Nonempty e es)

        ( T.BB_ROutsideErr e, ( es, _ ) ) ->
            Err (NE.Nonempty e es)

        ( T.BB_ROutsideBlocked, ( [], _ ) ) ->
            crash "seems like guida-stuff/ is corrupted"

        ( T.BB_ROutsideBlocked, ( e :: es, _ ) ) ->
            Err (NE.Nonempty e es)


addInside : T.CEMN_Raw -> T.BB_BResult -> List T.BB_Module -> List T.BB_Module
addInside name result modules =
    case result of
        T.BB_RNew _ iface objs _ ->
            T.BB_Fresh name iface objs :: modules

        T.BB_RSame _ iface objs _ ->
            T.BB_Fresh name iface objs :: modules

        T.BB_RCached main _ mvar ->
            T.BB_Cached name main mvar :: modules

        T.BB_RNotFound _ ->
            crash (badInside name)

        T.BB_RProblem _ ->
            crash (badInside name)

        T.BB_RBlocked ->
            crash (badInside name)

        T.BB_RForeign _ ->
            modules

        T.BB_RKernel ->
            modules


badInside : T.CEMN_Raw -> String
badInside name =
    "Error from `" ++ name ++ "` should have been reported already."


addOutside : T.BB_RootResult -> List T.BB_Module -> List T.BB_Module
addOutside root modules =
    case root of
        T.BB_RInside _ ->
            modules

        T.BB_ROutsideOk name iface objs ->
            T.BB_Fresh name iface objs :: modules

        T.BB_ROutsideErr _ ->
            modules

        T.BB_ROutsideBlocked ->
            modules
