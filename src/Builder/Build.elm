module Builder.Build exposing
    ( Artifacts(..)
    , BResult
    , CachedInterface(..)
    , Dependencies
    , DocsGoal(..)
    , Module(..)
    , ReplArtifacts(..)
    , Root(..)
    , cachedInterfaceDecoder
    , fromExposed
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
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Module as Parse
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error as Error
import Compiler.Reporting.Error.Docs as EDocs
import Compiler.Reporting.Error.Import as Import
import Compiler.Reporting.Error.Syntax as Syntax
import Compiler.Reporting.Render.Type.Localizer as L
import Data.Graph as Graph
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet
import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO exposing (IO)
import System.TypeCheck.IO as TypeCheck
import Utils.Crash exposing (crash)
import Utils.Main as Utils exposing (FilePath, MVar(..))



-- ENVIRONMENT


type Env
    = Env Reporting.BKey String Parse.ProjectType (List AbsoluteSrcDir) Details.BuildID (Dict String ModuleName.Raw Details.Local) (Dict String ModuleName.Raw Details.Foreign)


makeEnv : Reporting.BKey -> FilePath -> Details.Details -> IO Env
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
    = AbsoluteSrcDir FilePath


toAbsoluteSrcDir : FilePath -> Outline.SrcDir -> IO AbsoluteSrcDir
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


addRelative : AbsoluteSrcDir -> FilePath -> FilePath
addRelative (AbsoluteSrcDir srcDir) path =
    Utils.fpForwardSlash srcDir path



-- FORK


{-| PERF try using IORef semephore on file crawl phase?
described in Chapter 13 of Parallel and Concurrent Programming in Haskell by Simon Marlow
<https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch13.html#sec_conc-par-overhead>
-}
fork : (a -> Encode.Value) -> IO a -> IO (MVar a)
fork encoder work =
    Utils.newEmptyMVar
        |> IO.bind
            (\mvar ->
                Utils.forkIO (IO.bind (Utils.putMVar encoder mvar) work)
                    |> IO.fmap (\_ -> mvar)
            )


forkWithKey : (k -> comparable) -> (k -> k -> Order) -> (b -> Encode.Value) -> (k -> a -> IO b) -> Dict comparable k a -> IO (Dict comparable k (MVar b))
forkWithKey toComparable keyComparison encoder func dict =
    Utils.mapTraverseWithKey toComparable keyComparison (\k v -> fork encoder (func k v)) dict



-- FROM EXPOSED


fromExposed : Decode.Decoder docs -> (docs -> Encode.Value) -> Reporting.Style -> FilePath -> Details.Details -> DocsGoal docs -> NE.Nonempty ModuleName.Raw -> IO (Result Exit.BuildProblem docs)
fromExposed docsDecoder docsEncoder style root details docsGoal ((NE.Nonempty e es) as exposed) =
    Reporting.trackBuild docsDecoder docsEncoder style <|
        \key ->
            makeEnv key root details
                |> IO.bind
                    (\env ->
                        Details.loadInterfaces root details
                            |> IO.bind
                                (\dmvar ->
                                    -- crawl
                                    Utils.newEmptyMVar
                                        |> IO.bind
                                            (\mvar ->
                                                let
                                                    docsNeed : DocsNeed
                                                    docsNeed =
                                                        toDocsNeed docsGoal
                                                in
                                                Map.fromKeysA identity (fork statusEncoder << crawlModule env mvar docsNeed) (e :: es)
                                                    |> IO.bind
                                                        (\roots ->
                                                            Utils.putMVar statusDictEncoder mvar roots
                                                                |> IO.bind
                                                                    (\_ ->
                                                                        Utils.dictMapM_ compare (Utils.readMVar statusDecoder) roots
                                                                            |> IO.bind
                                                                                (\_ ->
                                                                                    IO.bind (Utils.mapTraverse identity compare (Utils.readMVar statusDecoder)) (Utils.readMVar statusDictDecoder mvar)
                                                                                        |> IO.bind
                                                                                            (\statuses ->
                                                                                                -- compile
                                                                                                checkMidpoint dmvar statuses
                                                                                                    |> IO.bind
                                                                                                        (\midpoint ->
                                                                                                            case midpoint of
                                                                                                                Err problem ->
                                                                                                                    IO.pure (Err (Exit.BuildProjectProblem problem))

                                                                                                                Ok foreigns ->
                                                                                                                    Utils.newEmptyMVar
                                                                                                                        |> IO.bind
                                                                                                                            (\rmvar ->
                                                                                                                                forkWithKey identity compare bResultEncoder (checkModule env foreigns rmvar) statuses
                                                                                                                                    |> IO.bind
                                                                                                                                        (\resultMVars ->
                                                                                                                                            Utils.putMVar dictRawMVarBResultEncoder rmvar resultMVars
                                                                                                                                                |> IO.bind
                                                                                                                                                    (\_ ->
                                                                                                                                                        Utils.mapTraverse identity compare (Utils.readMVar bResultDecoder) resultMVars
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


type Artifacts
    = Artifacts Pkg.Name Dependencies (NE.Nonempty Root) (List Module)


type Module
    = Fresh ModuleName.Raw I.Interface Opt.LocalGraph
    | Cached ModuleName.Raw Bool (MVar CachedInterface)


type alias Dependencies =
    Dict (List String) TypeCheck.Canonical I.DependencyInterface


fromPaths : Reporting.Style -> FilePath -> Details.Details -> NE.Nonempty FilePath -> IO (Result Exit.BuildProblem Artifacts)
fromPaths style root details paths =
    Reporting.trackBuild artifactsDecoder artifactsEncoder style <|
        \key ->
            makeEnv key root details
                |> IO.bind
                    (\env ->
                        findRoots env paths
                            |> IO.bind
                                (\elroots ->
                                    case elroots of
                                        Err problem ->
                                            IO.pure (Err (Exit.BuildProjectProblem problem))

                                        Ok lroots ->
                                            -- crawl
                                            Details.loadInterfaces root details
                                                |> IO.bind
                                                    (\dmvar ->
                                                        Utils.newMVar statusDictEncoder Dict.empty
                                                            |> IO.bind
                                                                (\smvar ->
                                                                    Utils.nonEmptyListTraverse (fork rootStatusEncoder << crawlRoot env smvar) lroots
                                                                        |> IO.bind
                                                                            (\srootMVars ->
                                                                                Utils.nonEmptyListTraverse (Utils.readMVar rootStatusDecoder) srootMVars
                                                                                    |> IO.bind
                                                                                        (\sroots ->
                                                                                            IO.bind (Utils.mapTraverse identity compare (Utils.readMVar statusDecoder)) (Utils.readMVar statusDictDecoder smvar)
                                                                                                |> IO.bind
                                                                                                    (\statuses ->
                                                                                                        checkMidpointAndRoots dmvar statuses sroots
                                                                                                            |> IO.bind
                                                                                                                (\midpoint ->
                                                                                                                    case midpoint of
                                                                                                                        Err problem ->
                                                                                                                            IO.pure (Err (Exit.BuildProjectProblem problem))

                                                                                                                        Ok foreigns ->
                                                                                                                            -- compile
                                                                                                                            Utils.newEmptyMVar
                                                                                                                                |> IO.bind
                                                                                                                                    (\rmvar ->
                                                                                                                                        forkWithKey identity compare bResultEncoder (checkModule env foreigns rmvar) statuses
                                                                                                                                            |> IO.bind
                                                                                                                                                (\resultsMVars ->
                                                                                                                                                    Utils.putMVar resultDictEncoder rmvar resultsMVars
                                                                                                                                                        |> IO.bind
                                                                                                                                                            (\_ ->
                                                                                                                                                                Utils.nonEmptyListTraverse (fork rootResultEncoder << checkRoot env resultsMVars) sroots
                                                                                                                                                                    |> IO.bind
                                                                                                                                                                        (\rrootMVars ->
                                                                                                                                                                            Utils.mapTraverse identity compare (Utils.readMVar bResultDecoder) resultsMVars
                                                                                                                                                                                |> IO.bind
                                                                                                                                                                                    (\results ->
                                                                                                                                                                                        writeDetails root details results
                                                                                                                                                                                            |> IO.bind
                                                                                                                                                                                                (\_ ->
                                                                                                                                                                                                    IO.fmap (toArtifacts env foreigns results) (Utils.nonEmptyListTraverse (Utils.readMVar rootResultDecoder) rrootMVars)
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


getRootNames : Artifacts -> NE.Nonempty ModuleName.Raw
getRootNames (Artifacts _ _ roots _) =
    NE.map getRootName roots


getRootName : Root -> ModuleName.Raw
getRootName root =
    case root of
        Inside name ->
            name

        Outside name _ _ ->
            name



-- CRAWL


type alias StatusDict =
    Dict String ModuleName.Raw (MVar Status)


type Status
    = SCached Details.Local
    | SChanged Details.Local String Src.Module DocsNeed
    | SBadImport Import.Problem
    | SBadSyntax FilePath File.Time String Syntax.Error
    | SForeign Pkg.Name
    | SKernel


crawlDeps : Env -> MVar StatusDict -> List ModuleName.Raw -> a -> IO a
crawlDeps env mvar deps blockedValue =
    let
        crawlNew : ModuleName.Raw -> () -> IO (MVar Status)
        crawlNew name () =
            fork statusEncoder (crawlModule env mvar (DocsNeed False) name)
    in
    Utils.takeMVar statusDictDecoder mvar
        |> IO.bind
            (\statusDict ->
                let
                    depsDict : Dict String ModuleName.Raw ()
                    depsDict =
                        Map.fromKeys (\_ -> ()) deps

                    newsDict : Dict String ModuleName.Raw ()
                    newsDict =
                        Dict.diff depsDict statusDict
                in
                Utils.mapTraverseWithKey identity compare crawlNew newsDict
                    |> IO.bind
                        (\statuses ->
                            Utils.putMVar statusDictEncoder mvar (Dict.union statuses statusDict)
                                |> IO.bind
                                    (\_ ->
                                        Utils.dictMapM_ compare (Utils.readMVar statusDecoder) statuses
                                            |> IO.fmap (\_ -> blockedValue)
                                    )
                        )
            )


crawlModule : Env -> MVar StatusDict -> DocsNeed -> ModuleName.Raw -> IO Status
crawlModule ((Env _ root projectType srcDirs buildID locals foreigns) as env) mvar ((DocsNeed needsDocs) as docsNeed) name =
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
                                IO.pure <| SBadImport <| Import.Ambiguous path [] dep deps

                            Nothing ->
                                File.getTime path
                                    |> IO.bind
                                        (\newTime ->
                                            case Dict.get identity name locals of
                                                Nothing ->
                                                    crawlFile env mvar docsNeed name path newTime buildID

                                                Just ((Details.Local oldPath oldTime deps _ lastChange _) as local) ->
                                                    if path /= oldPath || oldTime /= newTime || needsDocs then
                                                        crawlFile env mvar docsNeed name path newTime lastChange

                                                    else
                                                        crawlDeps env mvar deps (SCached local)
                                        )

                    p1 :: p2 :: ps ->
                        IO.pure <| SBadImport <| Import.AmbiguousLocal (Utils.fpMakeRelative root p1) (Utils.fpMakeRelative root p2) (List.map (Utils.fpMakeRelative root) ps)

                    [] ->
                        case Dict.get identity name foreigns of
                            Just (Details.Foreign dep deps) ->
                                case deps of
                                    [] ->
                                        IO.pure <| SForeign dep

                                    d :: ds ->
                                        IO.pure <| SBadImport <| Import.AmbiguousForeign dep d ds

                            Nothing ->
                                if Name.isKernel name && Parse.isKernel projectType then
                                    File.exists ("src/" ++ ModuleName.toFilePath name ++ ".js")
                                        |> IO.fmap
                                            (\exists ->
                                                if exists then
                                                    SKernel

                                                else
                                                    SBadImport Import.NotFound
                                            )

                                else
                                    IO.pure <| SBadImport Import.NotFound
            )


crawlFile : Env -> MVar StatusDict -> DocsNeed -> ModuleName.Raw -> FilePath -> File.Time -> Details.BuildID -> IO Status
crawlFile ((Env _ root projectType _ buildID _ _) as env) mvar docsNeed expectedName path time lastChange =
    File.readUtf8 (Utils.fpForwardSlash root path)
        |> IO.bind
            (\source ->
                case Parse.fromByteString projectType source of
                    Err err ->
                        IO.pure <| SBadSyntax path time source err

                    Ok ((Src.Module maybeActualName _ _ imports values _ _ _ _) as modul) ->
                        case maybeActualName of
                            Nothing ->
                                IO.pure <| SBadSyntax path time source (Syntax.ModuleNameUnspecified expectedName)

                            Just ((A.At _ actualName) as name) ->
                                if expectedName == actualName then
                                    let
                                        deps : List Name.Name
                                        deps =
                                            List.map Src.getImportName imports

                                        local : Details.Local
                                        local =
                                            Details.Local path time deps (List.any isMain values) lastChange buildID
                                    in
                                    crawlDeps env mvar deps (SChanged local source modul docsNeed)

                                else
                                    IO.pure <| SBadSyntax path time source (Syntax.ModuleNameMismatch expectedName name)
            )


isMain : A.Located Src.Value -> Bool
isMain (A.At _ (Src.Value (A.At _ name) _ _ _)) =
    name == Name.main_



-- CHECK MODULE


type alias ResultDict =
    Dict String ModuleName.Raw (MVar BResult)


type BResult
    = RNew Details.Local I.Interface Opt.LocalGraph (Maybe Docs.Module)
    | RSame Details.Local I.Interface Opt.LocalGraph (Maybe Docs.Module)
    | RCached Bool Details.BuildID (MVar CachedInterface)
    | RNotFound Import.Problem
    | RProblem Error.Module
    | RBlocked
    | RForeign I.Interface
    | RKernel


type CachedInterface
    = Unneeded
    | Loaded I.Interface
    | Corrupted


checkModule : Env -> Dependencies -> MVar ResultDict -> ModuleName.Raw -> Status -> IO BResult
checkModule ((Env _ root projectType _ _ _ _) as env) foreigns resultsMVar name status =
    case status of
        SCached ((Details.Local path time deps hasMain lastChange lastCompile) as local) ->
            Utils.readMVar resultDictDecoder resultsMVar
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
                                                                compile env (DocsNeed False) local source ifaces modul

                                                            Err err ->
                                                                IO.pure <|
                                                                    RProblem <|
                                                                        Error.Module name path time source (Error.BadSyntax err)
                                                    )

                                        DepsSame _ _ ->
                                            Utils.newMVar cachedInterfaceEncoder Unneeded
                                                |> IO.fmap
                                                    (\mvar ->
                                                        RCached hasMain lastChange mvar
                                                    )

                                        DepsBlock ->
                                            IO.pure RBlocked

                                        DepsNotFound problems ->
                                            File.readUtf8 path
                                                |> IO.bind
                                                    (\source ->
                                                        IO.pure <|
                                                            RProblem <|
                                                                Error.Module name path time source <|
                                                                    case Parse.fromByteString projectType source of
                                                                        Ok (Src.Module _ _ _ imports _ _ _ _ _) ->
                                                                            Error.BadImports (toImportErrors env results imports problems)

                                                                        Err err ->
                                                                            Error.BadSyntax err
                                                    )
                                )
                    )

        SChanged ((Details.Local path time deps _ _ lastCompile) as local) source ((Src.Module _ _ _ imports _ _ _ _ _) as modul) docsNeed ->
            Utils.readMVar resultDictDecoder resultsMVar
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
                                                                IO.pure RBlocked

                                                            Just ifaces ->
                                                                compile env docsNeed local source ifaces modul
                                                    )

                                        DepsBlock ->
                                            IO.pure RBlocked

                                        DepsNotFound problems ->
                                            IO.pure <|
                                                RProblem <|
                                                    Error.Module name path time source <|
                                                        Error.BadImports (toImportErrors env results imports problems)
                                )
                    )

        SBadImport importProblem ->
            IO.pure (RNotFound importProblem)

        SBadSyntax path time source err ->
            IO.pure <|
                RProblem <|
                    Error.Module name path time source <|
                        Error.BadSyntax err

        SForeign home ->
            case Utils.find ModuleName.toComparableCanonical (TypeCheck.Canonical home name) foreigns of
                I.Public iface ->
                    IO.pure (RForeign iface)

                I.Private _ _ _ ->
                    crash <| "mistakenly seeing private interface for " ++ Pkg.toChars home ++ " " ++ name

        SKernel ->
            IO.pure RKernel



-- CHECK DEPS


type DepsStatus
    = DepsChange (Dict String ModuleName.Raw I.Interface)
    | DepsSame (List Dep) (List CDep)
    | DepsBlock
    | DepsNotFound (NE.Nonempty ( ModuleName.Raw, Import.Problem ))


checkDeps : FilePath -> ResultDict -> List ModuleName.Raw -> Details.BuildID -> IO DepsStatus
checkDeps root results deps lastCompile =
    checkDepsHelp root results deps [] [] [] [] False 0 lastCompile


type alias Dep =
    ( ModuleName.Raw, I.Interface )


type alias CDep =
    ( ModuleName.Raw, MVar CachedInterface )


checkDepsHelp : FilePath -> ResultDict -> List ModuleName.Raw -> List Dep -> List Dep -> List CDep -> List ( ModuleName.Raw, Import.Problem ) -> Bool -> Details.BuildID -> Details.BuildID -> IO DepsStatus
checkDepsHelp root results deps new same cached importProblems isBlocked lastDepChange lastCompile =
    case deps of
        dep :: otherDeps ->
            Utils.readMVar bResultDecoder (Utils.find identity dep results)
                |> IO.bind
                    (\result ->
                        case result of
                            RNew (Details.Local _ _ _ _ lastChange _) iface _ _ ->
                                checkDepsHelp root results otherDeps (( dep, iface ) :: new) same cached importProblems isBlocked (max lastChange lastDepChange) lastCompile

                            RSame (Details.Local _ _ _ _ lastChange _) iface _ _ ->
                                checkDepsHelp root results otherDeps new (( dep, iface ) :: same) cached importProblems isBlocked (max lastChange lastDepChange) lastCompile

                            RCached _ lastChange mvar ->
                                checkDepsHelp root results otherDeps new same (( dep, mvar ) :: cached) importProblems isBlocked (max lastChange lastDepChange) lastCompile

                            RNotFound prob ->
                                checkDepsHelp root results otherDeps new same cached (( dep, prob ) :: importProblems) True lastDepChange lastCompile

                            RProblem _ ->
                                checkDepsHelp root results otherDeps new same cached importProblems True lastDepChange lastCompile

                            RBlocked ->
                                checkDepsHelp root results otherDeps new same cached importProblems True lastDepChange lastCompile

                            RForeign iface ->
                                checkDepsHelp root results otherDeps new (( dep, iface ) :: same) cached importProblems isBlocked lastDepChange lastCompile

                            RKernel ->
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


toImportErrors : Env -> ResultDict -> List Src.Import -> NE.Nonempty ( ModuleName.Raw, Import.Problem ) -> NE.Nonempty Import.Error
toImportErrors (Env _ _ _ _ _ locals foreigns) results imports problems =
    let
        knownModules : EverySet.EverySet String ModuleName.Raw
        knownModules =
            EverySet.fromList identity
                (List.concat
                    [ Dict.keys compare foreigns
                    , Dict.keys compare locals
                    , Dict.keys compare results
                    ]
                )

        unimportedModules : EverySet.EverySet String ModuleName.Raw
        unimportedModules =
            EverySet.diff knownModules (EverySet.fromList identity (List.map Src.getImportName imports))

        regionDict : Dict String Name.Name A.Region
        regionDict =
            Dict.fromList identity (List.map (\(Src.Import (A.At region name) _ _) -> ( name, region )) imports)

        toError : ( Name.Name, Import.Problem ) -> Import.Error
        toError ( name, problem ) =
            Import.Error (Utils.find identity name regionDict) name unimportedModules problem
    in
    NE.map toError problems



-- LOAD CACHED INTERFACES


loadInterfaces : FilePath -> List Dep -> List CDep -> IO (Maybe (Dict String ModuleName.Raw I.Interface))
loadInterfaces root same cached =
    Utils.listTraverse (fork maybeDepEncoder << loadInterface root) cached
        |> IO.bind
            (\loading ->
                Utils.listTraverse (Utils.readMVar maybeDepDecoder) loading
                    |> IO.bind
                        (\maybeLoaded ->
                            case Utils.sequenceListMaybe maybeLoaded of
                                Nothing ->
                                    IO.pure Nothing

                                Just loaded ->
                                    IO.pure <| Just <| Dict.union (Dict.fromList identity loaded) (Dict.fromList identity same)
                        )
            )


loadInterface : FilePath -> CDep -> IO (Maybe Dep)
loadInterface root ( name, ciMvar ) =
    Utils.takeMVar cachedInterfaceDecoder ciMvar
        |> IO.bind
            (\cachedInterface ->
                case cachedInterface of
                    Corrupted ->
                        Utils.putMVar cachedInterfaceEncoder ciMvar cachedInterface
                            |> IO.fmap (\_ -> Nothing)

                    Loaded iface ->
                        Utils.putMVar cachedInterfaceEncoder ciMvar cachedInterface
                            |> IO.fmap (\_ -> Just ( name, iface ))

                    Unneeded ->
                        File.readBinary I.interfaceDecoder (Stuff.elmi root name)
                            |> IO.bind
                                (\maybeIface ->
                                    case maybeIface of
                                        Nothing ->
                                            Utils.putMVar cachedInterfaceEncoder ciMvar Corrupted
                                                |> IO.fmap (\_ -> Nothing)

                                        Just iface ->
                                            Utils.putMVar cachedInterfaceEncoder ciMvar (Loaded iface)
                                                |> IO.fmap (\_ -> Just ( name, iface ))
                                )
            )



-- CHECK PROJECT


checkMidpoint : MVar (Maybe Dependencies) -> Dict String ModuleName.Raw Status -> IO (Result Exit.BuildProjectProblem Dependencies)
checkMidpoint dmvar statuses =
    case checkForCycles statuses of
        Nothing ->
            Utils.readMVar maybeDependenciesDecoder dmvar
                |> IO.fmap
                    (\maybeForeigns ->
                        case maybeForeigns of
                            Nothing ->
                                Err Exit.BP_CannotLoadDependencies

                            Just fs ->
                                Ok fs
                    )

        Just (NE.Nonempty name names) ->
            Utils.readMVar maybeDependenciesDecoder dmvar
                |> IO.fmap (\_ -> Err (Exit.BP_Cycle name names))


checkMidpointAndRoots : MVar (Maybe Dependencies) -> Dict String ModuleName.Raw Status -> NE.Nonempty RootStatus -> IO (Result Exit.BuildProjectProblem Dependencies)
checkMidpointAndRoots dmvar statuses sroots =
    case checkForCycles statuses of
        Nothing ->
            case checkUniqueRoots statuses sroots of
                Nothing ->
                    Utils.readMVar maybeDependenciesDecoder dmvar
                        |> IO.bind
                            (\maybeForeigns ->
                                case maybeForeigns of
                                    Nothing ->
                                        IO.pure (Err Exit.BP_CannotLoadDependencies)

                                    Just fs ->
                                        IO.pure (Ok fs)
                            )

                Just problem ->
                    Utils.readMVar maybeDependenciesDecoder dmvar
                        |> IO.fmap (\_ -> Err problem)

        Just (NE.Nonempty name names) ->
            Utils.readMVar maybeDependenciesDecoder dmvar
                |> IO.fmap (\_ -> Err (Exit.BP_Cycle name names))



-- CHECK FOR CYCLES


checkForCycles : Dict String ModuleName.Raw Status -> Maybe (NE.Nonempty ModuleName.Raw)
checkForCycles modules =
    let
        graph : List Node
        graph =
            Dict.foldr compare addToGraph [] modules

        sccs : List (Graph.SCC ModuleName.Raw)
        sccs =
            Graph.stronglyConnComp graph
    in
    checkForCyclesHelp sccs


checkForCyclesHelp : List (Graph.SCC ModuleName.Raw) -> Maybe (NE.Nonempty ModuleName.Raw)
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
    ( ModuleName.Raw, ModuleName.Raw, List ModuleName.Raw )


addToGraph : ModuleName.Raw -> Status -> List Node -> List Node
addToGraph name status graph =
    let
        dependencies : List ModuleName.Raw
        dependencies =
            case status of
                SCached (Details.Local _ _ deps _ _ _) ->
                    deps

                SChanged (Details.Local _ _ deps _ _ _) _ _ _ ->
                    deps

                SBadImport _ ->
                    []

                SBadSyntax _ _ _ _ ->
                    []

                SForeign _ ->
                    []

                SKernel ->
                    []
    in
    ( name, name, dependencies ) :: graph



-- CHECK UNIQUE ROOTS


checkUniqueRoots : Dict String ModuleName.Raw Status -> NE.Nonempty RootStatus -> Maybe Exit.BuildProjectProblem
checkUniqueRoots insides sroots =
    let
        outsidesDict : Dict String ModuleName.Raw (OneOrMore.OneOrMore FilePath)
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


rootStatusToNamePathPair : RootStatus -> Maybe ( ModuleName.Raw, OneOrMore.OneOrMore FilePath )
rootStatusToNamePathPair sroot =
    case sroot of
        SInside _ ->
            Nothing

        SOutsideOk (Details.Local path _ _ _ _ _) _ modul ->
            Just ( Src.getName modul, OneOrMore.one path )

        SOutsideErr _ ->
            Nothing


checkOutside : ModuleName.Raw -> OneOrMore.OneOrMore FilePath -> Result Exit.BuildProjectProblem FilePath
checkOutside name paths =
    case OneOrMore.destruct NE.Nonempty paths of
        NE.Nonempty p [] ->
            Ok p

        NE.Nonempty p1 (p2 :: _) ->
            Err (Exit.BP_RootNameDuplicate name p1 p2)


checkInside : ModuleName.Raw -> FilePath -> Status -> Result Exit.BuildProjectProblem ()
checkInside name p1 status =
    case status of
        SCached (Details.Local p2 _ _ _ _ _) ->
            Err (Exit.BP_RootNameDuplicate name p1 p2)

        SChanged (Details.Local p2 _ _ _ _ _) _ _ _ ->
            Err (Exit.BP_RootNameDuplicate name p1 p2)

        SBadImport _ ->
            Ok ()

        SBadSyntax _ _ _ _ ->
            Ok ()

        SForeign _ ->
            Ok ()

        SKernel ->
            Ok ()



-- COMPILE MODULE


compile : Env -> DocsNeed -> Details.Local -> String -> Dict String ModuleName.Raw I.Interface -> Src.Module -> IO BResult
compile (Env key root projectType _ buildID _ _) docsNeed (Details.Local path time deps main lastChange _) source ifaces modul =
    let
        pkg : Pkg.Name
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
                                    RProblem <|
                                        Error.Module (Src.getName modul) path time source (Error.BadDocs err)

                            Ok docs ->
                                let
                                    name : Name.Name
                                    name =
                                        Src.getName modul

                                    iface : I.Interface
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
                                                                    Reporting.report key Reporting.BDone
                                                                        |> IO.fmap
                                                                            (\_ ->
                                                                                let
                                                                                    local : Details.Local
                                                                                    local =
                                                                                        Details.Local path time deps main lastChange buildID
                                                                                in
                                                                                RSame local iface objects docs
                                                                            )

                                                                else
                                                                    File.writeBinary I.interfaceEncoder elmi iface
                                                                        |> IO.bind
                                                                            (\_ ->
                                                                                Reporting.report key Reporting.BDone
                                                                                    |> IO.fmap
                                                                                        (\_ ->
                                                                                            let
                                                                                                local : Details.Local
                                                                                                local =
                                                                                                    Details.Local path time deps main buildID buildID
                                                                                            in
                                                                                            RNew local iface objects docs
                                                                                        )
                                                                            )

                                                            _ ->
                                                                -- iface may be lazy still
                                                                File.writeBinary I.interfaceEncoder elmi iface
                                                                    |> IO.bind
                                                                        (\_ ->
                                                                            Reporting.report key Reporting.BDone
                                                                                |> IO.fmap
                                                                                    (\_ ->
                                                                                        let
                                                                                            local : Details.Local
                                                                                            local =
                                                                                                Details.Local path time deps main buildID buildID
                                                                                        in
                                                                                        RNew local iface objects docs
                                                                                    )
                                                                        )
                                                    )
                                        )

                    Err err ->
                        IO.pure <|
                            RProblem <|
                                Error.Module (Src.getName modul) path time source err
            )


projectTypeToPkg : Parse.ProjectType -> Pkg.Name
projectTypeToPkg projectType =
    case projectType of
        Parse.Package pkg ->
            pkg

        Parse.Application ->
            Pkg.dummyName



-- WRITE DETAILS


writeDetails : FilePath -> Details.Details -> Dict String ModuleName.Raw BResult -> IO ()
writeDetails root (Details.Details time outline buildID locals foreigns extras) results =
    File.writeBinary Details.detailsEncoder (Stuff.details root) <|
        Details.Details time outline buildID (Dict.foldr compare addNewLocal locals results) foreigns extras


addNewLocal : ModuleName.Raw -> BResult -> Dict String ModuleName.Raw Details.Local -> Dict String ModuleName.Raw Details.Local
addNewLocal name result locals =
    case result of
        RNew local _ _ _ ->
            Dict.insert identity name local locals

        RSame local _ _ _ ->
            Dict.insert identity name local locals

        RCached _ _ _ ->
            locals

        RNotFound _ ->
            locals

        RProblem _ ->
            locals

        RBlocked ->
            locals

        RForeign _ ->
            locals

        RKernel ->
            locals



-- FINALIZE EXPOSED


finalizeExposed : FilePath -> DocsGoal docs -> NE.Nonempty ModuleName.Raw -> Dict String ModuleName.Raw BResult -> IO (Result Exit.BuildProblem docs)
finalizeExposed root docsGoal exposed results =
    case List.foldr (addImportProblems results) [] (NE.toList exposed) of
        p :: ps ->
            IO.pure <| Err <| Exit.BuildProjectProblem (Exit.BP_MissingExposed (NE.Nonempty p ps))

        [] ->
            case Dict.foldr compare (\_ -> addErrors) [] results of
                [] ->
                    IO.fmap Ok (finalizeDocs docsGoal results)

                e :: es ->
                    IO.pure <| Err <| Exit.BuildBadModules root e es


addErrors : BResult -> List Error.Module -> List Error.Module
addErrors result errors =
    case result of
        RNew _ _ _ _ ->
            errors

        RSame _ _ _ _ ->
            errors

        RCached _ _ _ ->
            errors

        RNotFound _ ->
            errors

        RProblem e ->
            e :: errors

        RBlocked ->
            errors

        RForeign _ ->
            errors

        RKernel ->
            errors


addImportProblems : Dict String ModuleName.Raw BResult -> ModuleName.Raw -> List ( ModuleName.Raw, Import.Problem ) -> List ( ModuleName.Raw, Import.Problem )
addImportProblems results name problems =
    case Utils.find identity name results of
        RNew _ _ _ _ ->
            problems

        RSame _ _ _ _ ->
            problems

        RCached _ _ _ ->
            problems

        RNotFound p ->
            ( name, p ) :: problems

        RProblem _ ->
            problems

        RBlocked ->
            problems

        RForeign _ ->
            problems

        RKernel ->
            problems



-- DOCS


type DocsGoal docs
    = KeepDocs (Dict String ModuleName.Raw BResult -> docs)
    | WriteDocs (Dict String ModuleName.Raw BResult -> IO docs)
    | IgnoreDocs docs


keepDocs : DocsGoal (Dict String ModuleName.Raw Docs.Module)
keepDocs =
    KeepDocs (Utils.mapMapMaybe identity compare toDocs)


writeDocs : FilePath -> DocsGoal ()
writeDocs path =
    WriteDocs (E.writeUgly path << Docs.encode << Utils.mapMapMaybe identity compare toDocs)


ignoreDocs : DocsGoal ()
ignoreDocs =
    IgnoreDocs ()


type DocsNeed
    = DocsNeed Bool


toDocsNeed : DocsGoal a -> DocsNeed
toDocsNeed goal =
    case goal of
        IgnoreDocs _ ->
            DocsNeed False

        WriteDocs _ ->
            DocsNeed True

        KeepDocs _ ->
            DocsNeed True


makeDocs : DocsNeed -> Can.Module -> Result EDocs.Error (Maybe Docs.Module)
makeDocs (DocsNeed isNeeded) modul =
    if isNeeded then
        case Docs.fromModule modul of
            Ok docs ->
                Ok (Just docs)

            Err err ->
                Err err

    else
        Ok Nothing


finalizeDocs : DocsGoal docs -> Dict String ModuleName.Raw BResult -> IO docs
finalizeDocs goal results =
    case goal of
        KeepDocs f ->
            IO.pure <| f results

        WriteDocs f ->
            f results

        IgnoreDocs val ->
            IO.pure val


toDocs : BResult -> Maybe Docs.Module
toDocs result =
    case result of
        RNew _ _ _ d ->
            d

        RSame _ _ _ d ->
            d

        RCached _ _ _ ->
            Nothing

        RNotFound _ ->
            Nothing

        RProblem _ ->
            Nothing

        RBlocked ->
            Nothing

        RForeign _ ->
            Nothing

        RKernel ->
            Nothing



-------------------------------------------------------------------------------
------ NOW FOR SOME REPL STUFF -------------------------------------------------
--------------------------------------------------------------------------------
-- FROM REPL


type ReplArtifacts
    = ReplArtifacts TypeCheck.Canonical (List Module) L.Localizer (Dict String Name.Name Can.Annotation)


fromRepl : FilePath -> Details.Details -> String -> IO (Result Exit.Repl ReplArtifacts)
fromRepl root details source =
    makeEnv Reporting.ignorer root details
        |> IO.bind
            (\((Env _ _ projectType _ _ _ _) as env) ->
                case Parse.fromByteString projectType source of
                    Err syntaxError ->
                        IO.pure <| Err <| Exit.ReplBadInput source <| Error.BadSyntax syntaxError

                    Ok ((Src.Module _ _ _ imports _ _ _ _ _) as modul) ->
                        Details.loadInterfaces root details
                            |> IO.bind
                                (\dmvar ->
                                    let
                                        deps : List Name.Name
                                        deps =
                                            List.map Src.getImportName imports
                                    in
                                    Utils.newMVar statusDictEncoder Dict.empty
                                        |> IO.bind
                                            (\mvar ->
                                                crawlDeps env mvar deps ()
                                                    |> IO.bind
                                                        (\_ ->
                                                            IO.bind (Utils.mapTraverse identity compare (Utils.readMVar statusDecoder)) (Utils.readMVar statusDictDecoder mvar)
                                                                |> IO.bind
                                                                    (\statuses ->
                                                                        checkMidpoint dmvar statuses
                                                                            |> IO.bind
                                                                                (\midpoint ->
                                                                                    case midpoint of
                                                                                        Err problem ->
                                                                                            IO.pure <| Err <| Exit.ReplProjectProblem problem

                                                                                        Ok foreigns ->
                                                                                            Utils.newEmptyMVar
                                                                                                |> IO.bind
                                                                                                    (\rmvar ->
                                                                                                        forkWithKey identity compare bResultEncoder (checkModule env foreigns rmvar) statuses
                                                                                                            |> IO.bind
                                                                                                                (\resultMVars ->
                                                                                                                    Utils.putMVar resultDictEncoder rmvar resultMVars
                                                                                                                        |> IO.bind
                                                                                                                            (\_ ->
                                                                                                                                Utils.mapTraverse identity compare (Utils.readMVar bResultDecoder) resultMVars
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


finalizeReplArtifacts : Env -> String -> Src.Module -> DepsStatus -> ResultDict -> Dict String ModuleName.Raw BResult -> IO (Result Exit.Repl ReplArtifacts)
finalizeReplArtifacts ((Env _ root projectType _ _ _ _) as env) source ((Src.Module _ _ _ imports _ _ _ _ _) as modul) depsStatus resultMVars results =
    let
        pkg : Pkg.Name
        pkg =
            projectTypeToPkg projectType

        compileInput : Dict String ModuleName.Raw I.Interface -> IO (Result Exit.Repl ReplArtifacts)
        compileInput ifaces =
            Compile.compile pkg ifaces modul
                |> IO.fmap
                    (\result ->
                        case result of
                            Ok (Compile.Artifacts ((Can.Module name _ _ _ _ _ _ _) as canonical) annotations objects) ->
                                let
                                    h : TypeCheck.Canonical
                                    h =
                                        name

                                    m : Module
                                    m =
                                        Fresh (Src.getName modul) (I.fromModule pkg canonical annotations) objects

                                    ms : List Module
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
                        Error.BadImports <|
                            toImportErrors env resultMVars imports problems



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
------ AFTER THIS, EVERYTHING IS ABOUT HANDLING MODULES GIVEN BY FILEPATH ------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- FIND ROOT


type RootLocation
    = LInside ModuleName.Raw
    | LOutside FilePath


findRoots : Env -> NE.Nonempty FilePath -> IO (Result Exit.BuildProjectProblem (NE.Nonempty RootLocation))
findRoots env paths =
    Utils.nonEmptyListTraverse (fork resultBuildProjectProblemRootInfoEncoder << getRootInfo env) paths
        |> IO.bind
            (\mvars ->
                Utils.nonEmptyListTraverse (Utils.readMVar resultBuildProjectProblemRootInfoDecoder) mvars
                    |> IO.bind
                        (\einfos ->
                            IO.pure (Result.andThen checkRoots (Utils.sequenceNonemptyListResult einfos))
                        )
            )


checkRoots : NE.Nonempty RootInfo -> Result Exit.BuildProjectProblem (NE.Nonempty RootLocation)
checkRoots infos =
    let
        toOneOrMore : RootInfo -> ( FilePath, OneOrMore.OneOrMore RootInfo )
        toOneOrMore ((RootInfo absolute _ _) as loc) =
            ( absolute, OneOrMore.one loc )

        fromOneOrMore : RootInfo -> List RootInfo -> Result Exit.BuildProjectProblem ()
        fromOneOrMore (RootInfo _ relative _) locs =
            case locs of
                [] ->
                    Ok ()

                (RootInfo _ relative2 _) :: _ ->
                    Err (Exit.BP_MainPathDuplicate relative relative2)
    in
    Result.map (\_ -> NE.map (\(RootInfo _ _ location) -> location) infos) <|
        Utils.mapTraverseResult identity compare (OneOrMore.destruct fromOneOrMore) <|
            Utils.mapFromListWith identity OneOrMore.more <|
                List.map toOneOrMore (NE.toList infos)



-- ROOT INFO


type RootInfo
    = RootInfo FilePath FilePath RootLocation


getRootInfo : Env -> FilePath -> IO (Result Exit.BuildProjectProblem RootInfo)
getRootInfo env path =
    File.exists path
        |> IO.bind
            (\exists ->
                if exists then
                    IO.bind (getRootInfoHelp env path) (Utils.dirCanonicalizePath path)

                else
                    IO.pure (Err (Exit.BP_PathUnknown path))
            )


getRootInfoHelp : Env -> FilePath -> FilePath -> IO (Result Exit.BuildProjectProblem RootInfo)
getRootInfoHelp (Env _ _ _ srcDirs _ _ _) path absolutePath =
    let
        ( dirs, file ) =
            Utils.fpSplitFileName absolutePath

        ( final, ext ) =
            Utils.fpSplitExtension file
    in
    if ext /= ".elm" then
        IO.pure <| Err <| Exit.BP_WithBadExtension path

    else
        let
            absoluteSegments : List String
            absoluteSegments =
                Utils.fpSplitDirectories dirs ++ [ final ]
        in
        case List.filterMap (isInsideSrcDirByPath absoluteSegments) srcDirs of
            [] ->
                IO.pure <| Ok <| RootInfo absolutePath path (LOutside path)

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
                                        p1 : FilePath
                                        p1 =
                                            addRelative d1 (Utils.fpJoinPath names ++ ".elm")

                                        p2 : FilePath
                                        p2 =
                                            addRelative d2 (Utils.fpJoinPath names ++ ".elm")
                                    in
                                    IO.pure <| Err <| Exit.BP_RootNameDuplicate name p1 p2

                                _ ->
                                    IO.pure <| Ok <| RootInfo absolutePath path (LInside name)
                        )

            [ ( s, Err names ) ] ->
                IO.pure <| Err <| Exit.BP_RootNameInvalid path s names

            ( s1, _ ) :: ( s2, _ ) :: _ ->
                IO.pure <| Err <| Exit.BP_WithAmbiguousSrcDir path s1 s2


isInsideSrcDirByName : List String -> AbsoluteSrcDir -> IO Bool
isInsideSrcDirByName names srcDir =
    File.exists (addRelative srcDir (Utils.fpJoinPath names ++ ".elm"))


isInsideSrcDirByPath : List String -> AbsoluteSrcDir -> Maybe ( FilePath, Result (List String) (List String) )
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


dropPrefix : List FilePath -> List FilePath -> Maybe (List FilePath)
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


type RootStatus
    = SInside ModuleName.Raw
    | SOutsideOk Details.Local String Src.Module
    | SOutsideErr Error.Module


crawlRoot : Env -> MVar StatusDict -> RootLocation -> IO RootStatus
crawlRoot ((Env _ _ projectType _ buildID _ _) as env) mvar root =
    case root of
        LInside name ->
            Utils.newEmptyMVar
                |> IO.bind
                    (\statusMVar ->
                        Utils.takeMVar statusDictDecoder mvar
                            |> IO.bind
                                (\statusDict ->
                                    Utils.putMVar statusDictEncoder mvar (Dict.insert identity name statusMVar statusDict)
                                        |> IO.bind
                                            (\_ ->
                                                IO.bind (Utils.putMVar statusEncoder statusMVar) (crawlModule env mvar (DocsNeed False) name)
                                                    |> IO.fmap (\_ -> SInside name)
                                            )
                                )
                    )

        LOutside path ->
            File.getTime path
                |> IO.bind
                    (\time ->
                        File.readUtf8 path
                            |> IO.bind
                                (\source ->
                                    case Parse.fromByteString projectType source of
                                        Ok ((Src.Module _ _ _ imports values _ _ _ _) as modul) ->
                                            let
                                                deps : List Name.Name
                                                deps =
                                                    List.map Src.getImportName imports

                                                local : Details.Local
                                                local =
                                                    Details.Local path time deps (List.any isMain values) buildID buildID
                                            in
                                            crawlDeps env mvar deps (SOutsideOk local source modul)

                                        Err syntaxError ->
                                            IO.pure <|
                                                SOutsideErr <|
                                                    Error.Module "???" path time source (Error.BadSyntax syntaxError)
                                )
                    )



-- CHECK ROOTS


type RootResult
    = RInside ModuleName.Raw
    | ROutsideOk ModuleName.Raw I.Interface Opt.LocalGraph
    | ROutsideErr Error.Module
    | ROutsideBlocked


checkRoot : Env -> ResultDict -> RootStatus -> IO RootResult
checkRoot ((Env _ root _ _ _ _ _) as env) results rootStatus =
    case rootStatus of
        SInside name ->
            IO.pure (RInside name)

        SOutsideErr err ->
            IO.pure (ROutsideErr err)

        SOutsideOk ((Details.Local path time deps _ _ lastCompile) as local) source ((Src.Module _ _ _ imports _ _ _ _ _) as modul) ->
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
                                                    IO.pure ROutsideBlocked

                                                Just ifaces ->
                                                    compileOutside env local source ifaces modul
                                        )

                            DepsBlock ->
                                IO.pure ROutsideBlocked

                            DepsNotFound problems ->
                                IO.pure <|
                                    ROutsideErr <|
                                        Error.Module (Src.getName modul) path time source <|
                                            Error.BadImports (toImportErrors env results imports problems)
                    )


compileOutside : Env -> Details.Local -> String -> Dict String ModuleName.Raw I.Interface -> Src.Module -> IO RootResult
compileOutside (Env key _ projectType _ _ _ _) (Details.Local path time _ _ _ _) source ifaces modul =
    let
        pkg : Pkg.Name
        pkg =
            projectTypeToPkg projectType

        name : Name.Name
        name =
            Src.getName modul
    in
    Compile.compile pkg ifaces modul
        |> IO.bind
            (\result ->
                case result of
                    Ok (Compile.Artifacts canonical annotations objects) ->
                        Reporting.report key Reporting.BDone
                            |> IO.fmap (\_ -> ROutsideOk name (I.fromModule pkg canonical annotations) objects)

                    Err errors ->
                        IO.pure <| ROutsideErr <| Error.Module name path time source errors
            )



-- TO ARTIFACTS


type Root
    = Inside ModuleName.Raw
    | Outside ModuleName.Raw I.Interface Opt.LocalGraph


toArtifacts : Env -> Dependencies -> Dict String ModuleName.Raw BResult -> NE.Nonempty RootResult -> Result Exit.BuildProblem Artifacts
toArtifacts (Env _ root projectType _ _ _ _) foreigns results rootResults =
    case gatherProblemsOrMains results rootResults of
        Err (NE.Nonempty e es) ->
            Err (Exit.BuildBadModules root e es)

        Ok roots ->
            Ok <|
                Artifacts (projectTypeToPkg projectType) foreigns roots <|
                    Dict.foldr compare addInside (NE.foldr addOutside [] rootResults) results


gatherProblemsOrMains : Dict String ModuleName.Raw BResult -> NE.Nonempty RootResult -> Result (NE.Nonempty Error.Module) (NE.Nonempty Root)
gatherProblemsOrMains results (NE.Nonempty rootResult rootResults) =
    let
        addResult : RootResult -> ( List Error.Module, List Root ) -> ( List Error.Module, List Root )
        addResult result ( es, roots ) =
            case result of
                RInside n ->
                    ( es, Inside n :: roots )

                ROutsideOk n i o ->
                    ( es, Outside n i o :: roots )

                ROutsideErr e ->
                    ( e :: es, roots )

                ROutsideBlocked ->
                    ( es, roots )

        errors : List Error.Module
        errors =
            Dict.foldr compare (\_ -> addErrors) [] results
    in
    case ( rootResult, List.foldr addResult ( errors, [] ) rootResults ) of
        ( RInside n, ( [], ms ) ) ->
            Ok (NE.Nonempty (Inside n) ms)

        ( RInside _, ( e :: es, _ ) ) ->
            Err (NE.Nonempty e es)

        ( ROutsideOk n i o, ( [], ms ) ) ->
            Ok (NE.Nonempty (Outside n i o) ms)

        ( ROutsideOk _ _ _, ( e :: es, _ ) ) ->
            Err (NE.Nonempty e es)

        ( ROutsideErr e, ( es, _ ) ) ->
            Err (NE.Nonempty e es)

        ( ROutsideBlocked, ( [], _ ) ) ->
            crash "seems like guida-stuff/ is corrupted"

        ( ROutsideBlocked, ( e :: es, _ ) ) ->
            Err (NE.Nonempty e es)


addInside : ModuleName.Raw -> BResult -> List Module -> List Module
addInside name result modules =
    case result of
        RNew _ iface objs _ ->
            Fresh name iface objs :: modules

        RSame _ iface objs _ ->
            Fresh name iface objs :: modules

        RCached main _ mvar ->
            Cached name main mvar :: modules

        RNotFound _ ->
            crash (badInside name)

        RProblem _ ->
            crash (badInside name)

        RBlocked ->
            crash (badInside name)

        RForeign _ ->
            modules

        RKernel ->
            modules


badInside : ModuleName.Raw -> String
badInside name =
    "Error from `" ++ name ++ "` should have been reported already."


addOutside : RootResult -> List Module -> List Module
addOutside root modules =
    case root of
        RInside _ ->
            modules

        ROutsideOk name iface objs ->
            Fresh name iface objs :: modules

        ROutsideErr _ ->
            modules

        ROutsideBlocked ->
            modules



-- ENCODERS and DECODERS


dictRawMVarBResultEncoder : Dict String ModuleName.Raw (MVar BResult) -> Encode.Value
dictRawMVarBResultEncoder =
    E.assocListDict compare ModuleName.rawEncoder Utils.mVarEncoder


bResultEncoder : BResult -> Encode.Value
bResultEncoder bResult =
    case bResult of
        RNew local iface objects docs ->
            Encode.object
                [ ( "type", Encode.string "RNew" )
                , ( "local", Details.localEncoder local )
                , ( "iface", I.interfaceEncoder iface )
                , ( "objects", Opt.localGraphEncoder objects )
                , ( "docs"
                  , docs
                        |> Maybe.map Docs.jsonModuleEncoder
                        |> Maybe.withDefault Encode.null
                  )
                ]

        RSame local iface objects docs ->
            Encode.object
                [ ( "type", Encode.string "RSame" )
                , ( "local", Details.localEncoder local )
                , ( "iface", I.interfaceEncoder iface )
                , ( "objects", Opt.localGraphEncoder objects )
                , ( "docs", E.maybe Docs.jsonModuleEncoder docs )
                ]

        RCached main lastChange (MVar ref) ->
            Encode.object
                [ ( "type", Encode.string "RCached" )
                , ( "main", Encode.bool main )
                , ( "lastChange", Encode.int lastChange )
                , ( "mvar", Encode.int ref )
                ]

        RNotFound importProblem ->
            Encode.object
                [ ( "type", Encode.string "RNotFound" )
                , ( "importProblem", Import.problemEncoder importProblem )
                ]

        RProblem e ->
            Encode.object
                [ ( "type", Encode.string "RProblem" )
                , ( "e", Error.moduleEncoder e )
                ]

        RBlocked ->
            Encode.object [ ( "type", Encode.string "RBlocked" ) ]

        RForeign iface ->
            Encode.object
                [ ( "type", Encode.string "RForeign" )
                , ( "iface", I.interfaceEncoder iface )
                ]

        RKernel ->
            Encode.object [ ( "type", Encode.string "RKernel" ) ]


bResultDecoder : Decode.Decoder BResult
bResultDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "RNew" ->
                        Decode.map4 RNew
                            (Decode.field "local" Details.localDecoder)
                            (Decode.field "iface" I.interfaceDecoder)
                            (Decode.field "objects" Opt.localGraphDecoder)
                            (Decode.field "docs" (Decode.maybe Docs.jsonModuleDecoder))

                    "RSame" ->
                        Decode.map4 RSame
                            (Decode.field "local" Details.localDecoder)
                            (Decode.field "iface" I.interfaceDecoder)
                            (Decode.field "objects" Opt.localGraphDecoder)
                            (Decode.field "docs" (Decode.maybe Docs.jsonModuleDecoder))

                    "RCached" ->
                        Decode.map3 RCached
                            (Decode.field "main" Decode.bool)
                            (Decode.field "lastChange" Decode.int)
                            (Decode.field "mvar" (Decode.map MVar Decode.int))

                    "RNotFound" ->
                        Decode.map RNotFound
                            (Decode.field "importProblem" Import.problemDecoder)

                    "RProblem" ->
                        Decode.map RProblem
                            (Decode.field "e" Error.moduleDecoder)

                    "RBlocked" ->
                        Decode.succeed RBlocked

                    "RForeign" ->
                        Decode.map RForeign
                            (Decode.field "iface" I.interfaceDecoder)

                    "RKernel" ->
                        Decode.succeed RKernel

                    _ ->
                        Decode.fail ("Failed to decode BResult's type: " ++ type_)
            )


statusDictEncoder : StatusDict -> Encode.Value
statusDictEncoder statusDict =
    E.assocListDict compare ModuleName.rawEncoder Utils.mVarEncoder statusDict


statusDictDecoder : Decode.Decoder StatusDict
statusDictDecoder =
    D.assocListDict identity ModuleName.rawDecoder Utils.mVarDecoder


statusEncoder : Status -> Encode.Value
statusEncoder status =
    case status of
        SCached local ->
            Encode.object
                [ ( "type", Encode.string "SCached" )
                , ( "local", Details.localEncoder local )
                ]

        SChanged local iface objects docs ->
            Encode.object
                [ ( "type", Encode.string "SChanged" )
                , ( "local", Details.localEncoder local )
                , ( "iface", Encode.string iface )
                , ( "objects", Src.moduleEncoder objects )
                , ( "docs", docsNeedEncoder docs )
                ]

        SBadImport importProblem ->
            Encode.object
                [ ( "type", Encode.string "SBadImport" )
                , ( "importProblem", Import.problemEncoder importProblem )
                ]

        SBadSyntax path time source err ->
            Encode.object
                [ ( "type", Encode.string "SBadSyntax" )
                , ( "path", Encode.string path )
                , ( "time", File.timeEncoder time )
                , ( "source", Encode.string source )
                , ( "err", Syntax.errorEncoder err )
                ]

        SForeign home ->
            Encode.object
                [ ( "type", Encode.string "SForeign" )
                , ( "home", Pkg.nameEncoder home )
                ]

        SKernel ->
            Encode.object
                [ ( "type", Encode.string "SKernel" )
                ]


statusDecoder : Decode.Decoder Status
statusDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "SCached" ->
                        Decode.map SCached (Decode.field "local" Details.localDecoder)

                    "SChanged" ->
                        Decode.map4 SChanged
                            (Decode.field "local" Details.localDecoder)
                            (Decode.field "iface" Decode.string)
                            (Decode.field "objects" Src.moduleDecoder)
                            (Decode.field "docs" docsNeedDecoder)

                    "SBadImport" ->
                        Decode.map SBadImport (Decode.field "importProblem" Import.problemDecoder)

                    "SBadSyntax" ->
                        Decode.map4 SBadSyntax
                            (Decode.field "path" Decode.string)
                            (Decode.field "time" File.timeDecoder)
                            (Decode.field "source" Decode.string)
                            (Decode.field "err" Syntax.errorDecoder)

                    "SForeign" ->
                        Decode.map SForeign (Decode.field "home" Pkg.nameDecoder)

                    "SKernel" ->
                        Decode.succeed SKernel

                    _ ->
                        Decode.fail ("Failed to decode Status's type: " ++ type_)
            )


rootStatusEncoder : RootStatus -> Encode.Value
rootStatusEncoder rootStatus =
    case rootStatus of
        SInside name ->
            Encode.object
                [ ( "type", Encode.string "SInside" )
                , ( "name", ModuleName.rawEncoder name )
                ]

        SOutsideOk local source modul ->
            Encode.object
                [ ( "type", Encode.string "SOutsideOk" )
                , ( "local", Details.localEncoder local )
                , ( "source", Encode.string source )
                , ( "modul", Src.moduleEncoder modul )
                ]

        SOutsideErr err ->
            Encode.object
                [ ( "type", Encode.string "SOutsideErr" )
                , ( "err", Error.moduleEncoder err )
                ]


rootStatusDecoder : Decode.Decoder RootStatus
rootStatusDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "SInside" ->
                        Decode.map SInside (Decode.field "name" ModuleName.rawDecoder)

                    "SOutsideOk" ->
                        Decode.map3 SOutsideOk
                            (Decode.field "local" Details.localDecoder)
                            (Decode.field "source" Decode.string)
                            (Decode.field "modul" Src.moduleDecoder)

                    "SOutsideErr" ->
                        Decode.map SOutsideErr (Decode.field "err" Error.moduleDecoder)

                    _ ->
                        Decode.fail ("Failed to decode RootStatus' type: " ++ type_)
            )


resultDictEncoder : ResultDict -> Encode.Value
resultDictEncoder =
    E.assocListDict compare ModuleName.rawEncoder Utils.mVarEncoder


resultDictDecoder : Decode.Decoder ResultDict
resultDictDecoder =
    D.assocListDict identity ModuleName.rawDecoder Utils.mVarDecoder


rootResultEncoder : RootResult -> Encode.Value
rootResultEncoder rootResult =
    case rootResult of
        RInside name ->
            Encode.object
                [ ( "type", Encode.string "RInside" )
                , ( "name", ModuleName.rawEncoder name )
                ]

        ROutsideOk name iface objs ->
            Encode.object
                [ ( "type", Encode.string "ROutsideOk" )
                , ( "name", ModuleName.rawEncoder name )
                , ( "iface", I.interfaceEncoder iface )
                , ( "objs", Opt.localGraphEncoder objs )
                ]

        ROutsideErr err ->
            Encode.object
                [ ( "type", Encode.string "ROutsideErr" )
                , ( "err", Error.moduleEncoder err )
                ]

        ROutsideBlocked ->
            Encode.object
                [ ( "type", Encode.string "ROutsideBlocked" )
                ]


rootResultDecoder : Decode.Decoder RootResult
rootResultDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "RInside" ->
                        Decode.map RInside (Decode.field "name" ModuleName.rawDecoder)

                    "ROutsideOk" ->
                        Decode.map3 ROutsideOk
                            (Decode.field "name" ModuleName.rawDecoder)
                            (Decode.field "iface" I.interfaceDecoder)
                            (Decode.field "objs" Opt.localGraphDecoder)

                    "ROutsideErr" ->
                        Decode.map ROutsideErr (Decode.field "err" Error.moduleDecoder)

                    "ROutsideBlocked" ->
                        Decode.succeed ROutsideBlocked

                    _ ->
                        Decode.fail ("Failed to decode RootResult's type: " ++ type_)
            )


maybeDepEncoder : Maybe Dep -> Encode.Value
maybeDepEncoder =
    E.maybe depEncoder


maybeDepDecoder : Decode.Decoder (Maybe Dep)
maybeDepDecoder =
    Decode.maybe depDecoder


depEncoder : Dep -> Encode.Value
depEncoder =
    E.jsonPair ModuleName.rawEncoder I.interfaceEncoder


depDecoder : Decode.Decoder Dep
depDecoder =
    D.jsonPair ModuleName.rawDecoder I.interfaceDecoder


maybeDependenciesDecoder : Decode.Decoder (Maybe Dependencies)
maybeDependenciesDecoder =
    Decode.maybe (D.assocListDict ModuleName.toComparableCanonical ModuleName.canonicalDecoder I.dependencyInterfaceDecoder)


resultBuildProjectProblemRootInfoEncoder : Result Exit.BuildProjectProblem RootInfo -> Encode.Value
resultBuildProjectProblemRootInfoEncoder =
    E.result Exit.buildProjectProblemEncoder rootInfoEncoder


resultBuildProjectProblemRootInfoDecoder : Decode.Decoder (Result Exit.BuildProjectProblem RootInfo)
resultBuildProjectProblemRootInfoDecoder =
    D.result Exit.buildProjectProblemDecoder rootInfoDecoder


cachedInterfaceEncoder : CachedInterface -> Encode.Value
cachedInterfaceEncoder cachedInterface =
    case cachedInterface of
        Unneeded ->
            Encode.object
                [ ( "type", Encode.string "Unneeded" )
                ]

        Loaded iface ->
            Encode.object
                [ ( "type", Encode.string "Loaded" )
                , ( "iface", I.interfaceEncoder iface )
                ]

        Corrupted ->
            Encode.object
                [ ( "type", Encode.string "Corrupted" )
                ]


cachedInterfaceDecoder : Decode.Decoder CachedInterface
cachedInterfaceDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Unneeded" ->
                        Decode.succeed Unneeded

                    "Loaded" ->
                        Decode.map Loaded (Decode.field "iface" I.interfaceDecoder)

                    "Corrupted" ->
                        Decode.succeed Corrupted

                    _ ->
                        Decode.fail ("Failed to decode CachedInterface's type: " ++ type_)
            )


docsNeedEncoder : DocsNeed -> Encode.Value
docsNeedEncoder (DocsNeed isNeeded) =
    Encode.bool isNeeded


docsNeedDecoder : Decode.Decoder DocsNeed
docsNeedDecoder =
    Decode.map DocsNeed Decode.bool


artifactsEncoder : Artifacts -> Encode.Value
artifactsEncoder (Artifacts pkg ifaces roots modules) =
    Encode.object
        [ ( "type", Encode.string "Artifacts" )
        , ( "pkg", Pkg.nameEncoder pkg )
        , ( "ifaces", dependenciesEncoder ifaces )
        , ( "roots", E.nonempty rootEncoder roots )
        , ( "modules", Encode.list moduleEncoder modules )
        ]


artifactsDecoder : Decode.Decoder Artifacts
artifactsDecoder =
    Decode.map4 Artifacts
        (Decode.field "pkg" Pkg.nameDecoder)
        (Decode.field "ifaces" dependenciesDecoder)
        (Decode.field "roots" (D.nonempty rootDecoder))
        (Decode.field "modules" (Decode.list moduleDecoder))


dependenciesEncoder : Dependencies -> Encode.Value
dependenciesEncoder =
    E.assocListDict ModuleName.compareCanonical ModuleName.canonicalEncoder I.dependencyInterfaceEncoder


dependenciesDecoder : Decode.Decoder Dependencies
dependenciesDecoder =
    D.assocListDict ModuleName.toComparableCanonical ModuleName.canonicalDecoder I.dependencyInterfaceDecoder


rootEncoder : Root -> Encode.Value
rootEncoder root =
    case root of
        Inside name ->
            Encode.object
                [ ( "type", Encode.string "Inside" )
                , ( "name", ModuleName.rawEncoder name )
                ]

        Outside name main mvar ->
            Encode.object
                [ ( "type", Encode.string "Outside" )
                , ( "name", ModuleName.rawEncoder name )
                , ( "main", I.interfaceEncoder main )
                , ( "mvar", Opt.localGraphEncoder mvar )
                ]


rootDecoder : Decode.Decoder Root
rootDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Inside" ->
                        Decode.map Inside (Decode.field "name" ModuleName.rawDecoder)

                    "Outside" ->
                        Decode.map3 Outside
                            (Decode.field "name" ModuleName.rawDecoder)
                            (Decode.field "main" I.interfaceDecoder)
                            (Decode.field "mvar" Opt.localGraphDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Root's type: " ++ type_)
            )


moduleEncoder : Module -> Encode.Value
moduleEncoder modul =
    case modul of
        Fresh name iface objs ->
            Encode.object
                [ ( "type", Encode.string "Fresh" )
                , ( "name", ModuleName.rawEncoder name )
                , ( "iface", I.interfaceEncoder iface )
                , ( "objs", Opt.localGraphEncoder objs )
                ]

        Cached name main mvar ->
            Encode.object
                [ ( "type", Encode.string "Cached" )
                , ( "name", ModuleName.rawEncoder name )
                , ( "main", Encode.bool main )
                , ( "mvar", Utils.mVarEncoder mvar )
                ]


moduleDecoder : Decode.Decoder Module
moduleDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Fresh" ->
                        Decode.map3 Fresh
                            (Decode.field "name" ModuleName.rawDecoder)
                            (Decode.field "iface" I.interfaceDecoder)
                            (Decode.field "objs" Opt.localGraphDecoder)

                    "Cached" ->
                        Decode.map3 Cached
                            (Decode.field "name" ModuleName.rawDecoder)
                            (Decode.field "main" Decode.bool)
                            (Decode.field "mvar" Utils.mVarDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Module's type: " ++ type_)
            )


rootInfoEncoder : RootInfo -> Encode.Value
rootInfoEncoder (RootInfo absolute relative location) =
    Encode.object
        [ ( "type", Encode.string "RootInfo" )
        , ( "absolute", Encode.string absolute )
        , ( "relative", Encode.string relative )
        , ( "location", rootLocationEncoder location )
        ]


rootInfoDecoder : Decode.Decoder RootInfo
rootInfoDecoder =
    Decode.map3 RootInfo
        (Decode.field "absolute" Decode.string)
        (Decode.field "relative" Decode.string)
        (Decode.field "location" rootLocationDecoder)


rootLocationEncoder : RootLocation -> Encode.Value
rootLocationEncoder rootLocation =
    case rootLocation of
        LInside name ->
            Encode.object
                [ ( "type", Encode.string "LInside" )
                , ( "name", ModuleName.rawEncoder name )
                ]

        LOutside path ->
            Encode.object
                [ ( "type", Encode.string "LOutside" )
                , ( "path", Encode.string path )
                ]


rootLocationDecoder : Decode.Decoder RootLocation
rootLocationDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "LInside" ->
                        Decode.map LInside (Decode.field "name" ModuleName.rawDecoder)

                    "LOutside" ->
                        Decode.map LOutside (Decode.field "path" Decode.string)

                    _ ->
                        Decode.fail ("Failed to decode RootLocation's type: " ++ type_)
            )
