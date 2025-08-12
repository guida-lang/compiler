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
import Compiler.Json.Encode as E
import Compiler.Parse.Module as Parse
import Compiler.Parse.SyntaxVersion as SV
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error as Error
import Compiler.Reporting.Error.Docs as EDocs
import Compiler.Reporting.Error.Import as Import
import Compiler.Reporting.Error.Syntax as Syntax
import Compiler.Reporting.Render.Type.Localizer as L
import Data.Graph as Graph
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet
import System.TypeCheck.IO as TypeCheck
import Task exposing (Task)
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Crash exposing (crash)
import Utils.Main as Utils exposing (FilePath, MVar(..))
import Utils.Task.Extra as Task



-- ENVIRONMENT


type Env
    = Env Reporting.BKey String Parse.ProjectType (List AbsoluteSrcDir) Details.BuildID (Dict String ModuleName.Raw Details.Local) (Dict String ModuleName.Raw Details.Foreign)


makeEnv : Reporting.BKey -> FilePath -> Details.Details -> Task Never Env
makeEnv key root (Details.Details _ validOutline buildID locals foreigns _) =
    case validOutline of
        Details.ValidApp givenSrcDirs ->
            Utils.listTraverse (toAbsoluteSrcDir root) (NE.toList givenSrcDirs)
                |> Task.fmap (\srcDirs -> Env key root Parse.Application srcDirs buildID locals foreigns)

        Details.ValidPkg pkg _ _ ->
            toAbsoluteSrcDir root (Outline.RelativeSrcDir "src")
                |> Task.fmap (\srcDir -> Env key root (Parse.Package pkg) [ srcDir ] buildID locals foreigns)



-- SOURCE DIRECTORY


type AbsoluteSrcDir
    = AbsoluteSrcDir FilePath


toAbsoluteSrcDir : FilePath -> Outline.SrcDir -> Task Never AbsoluteSrcDir
toAbsoluteSrcDir root srcDir =
    Task.fmap AbsoluteSrcDir
        (Utils.dirCanonicalizePath
            (case srcDir of
                Outline.AbsoluteSrcDir dir ->
                    dir

                Outline.RelativeSrcDir dir ->
                    Utils.fpCombine root dir
            )
        )


addRelative : AbsoluteSrcDir -> FilePath -> FilePath
addRelative (AbsoluteSrcDir srcDir) path =
    Utils.fpCombine srcDir path



-- FORK


{-| PERF try using IORef semephore on file crawl phase?
described in Chapter 13 of Parallel and Concurrent Programming in Haskell by Simon Marlow
<https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch13.html#sec_conc-par-overhead>
-}
fork : (a -> BE.Encoder) -> Task Never a -> Task Never (MVar a)
fork encoder work =
    Utils.newEmptyMVar
        |> Task.bind
            (\mvar ->
                Utils.forkIO (Task.bind (Utils.putMVar encoder mvar) work)
                    |> Task.fmap (\_ -> mvar)
            )


forkWithKey : (k -> comparable) -> (k -> k -> Order) -> (b -> BE.Encoder) -> (k -> a -> Task Never b) -> Dict comparable k a -> Task Never (Dict comparable k (MVar b))
forkWithKey toComparable keyComparison encoder func dict =
    Utils.mapTraverseWithKey toComparable keyComparison (\k v -> fork encoder (func k v)) dict



-- FROM EXPOSED


fromExposed : BD.Decoder docs -> (docs -> BE.Encoder) -> Reporting.Style -> FilePath -> Details.Details -> DocsGoal docs -> NE.Nonempty ModuleName.Raw -> Task Never (Result Exit.BuildProblem docs)
fromExposed docsDecoder docsEncoder style root details docsGoal ((NE.Nonempty e es) as exposed) =
    Reporting.trackBuild docsDecoder docsEncoder style <|
        \key ->
            makeEnv key root details
                |> Task.bind
                    (\env ->
                        Details.loadInterfaces root details
                            |> Task.bind
                                (\dmvar ->
                                    -- crawl
                                    Utils.newEmptyMVar
                                        |> Task.bind
                                            (\mvar ->
                                                let
                                                    docsNeed : DocsNeed
                                                    docsNeed =
                                                        toDocsNeed docsGoal
                                                in
                                                Map.fromKeysA identity (fork statusEncoder << crawlModule env mvar docsNeed) (e :: es)
                                                    |> Task.bind
                                                        (\roots ->
                                                            Utils.putMVar statusDictEncoder mvar roots
                                                                |> Task.bind
                                                                    (\_ ->
                                                                        Utils.dictMapM_ compare (Utils.readMVar statusDecoder) roots
                                                                            |> Task.bind
                                                                                (\_ ->
                                                                                    Task.bind (Utils.mapTraverse identity compare (Utils.readMVar statusDecoder)) (Utils.readMVar statusDictDecoder mvar)
                                                                                        |> Task.bind
                                                                                            (\statuses ->
                                                                                                -- compile
                                                                                                checkMidpoint dmvar statuses
                                                                                                    |> Task.bind
                                                                                                        (\midpoint ->
                                                                                                            case midpoint of
                                                                                                                Err problem ->
                                                                                                                    Task.pure (Err (Exit.BuildProjectProblem problem))

                                                                                                                Ok foreigns ->
                                                                                                                    Utils.newEmptyMVar
                                                                                                                        |> Task.bind
                                                                                                                            (\rmvar ->
                                                                                                                                forkWithKey identity compare bResultEncoder (checkModule env foreigns rmvar) statuses
                                                                                                                                    |> Task.bind
                                                                                                                                        (\resultMVars ->
                                                                                                                                            Utils.putMVar dictRawMVarBResultEncoder rmvar resultMVars
                                                                                                                                                |> Task.bind
                                                                                                                                                    (\_ ->
                                                                                                                                                        Utils.mapTraverse identity compare (Utils.readMVar bResultDecoder) resultMVars
                                                                                                                                                            |> Task.bind
                                                                                                                                                                (\results ->
                                                                                                                                                                    writeDetails root details results
                                                                                                                                                                        |> Task.bind
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


fromPaths : Reporting.Style -> FilePath -> Details.Details -> NE.Nonempty FilePath -> Task Never (Result Exit.BuildProblem Artifacts)
fromPaths style root details paths =
    Reporting.trackBuild artifactsDecoder artifactsEncoder style <|
        \key ->
            makeEnv key root details
                |> Task.bind
                    (\env ->
                        findRoots env paths
                            |> Task.bind
                                (\elroots ->
                                    case elroots of
                                        Err problem ->
                                            Task.pure (Err (Exit.BuildProjectProblem problem))

                                        Ok lroots ->
                                            -- crawl
                                            Details.loadInterfaces root details
                                                |> Task.bind
                                                    (\dmvar ->
                                                        Utils.newMVar statusDictEncoder Dict.empty
                                                            |> Task.bind
                                                                (\smvar ->
                                                                    Utils.nonEmptyListTraverse (fork rootStatusEncoder << crawlRoot env smvar) lroots
                                                                        |> Task.bind
                                                                            (\srootMVars ->
                                                                                Utils.nonEmptyListTraverse (Utils.readMVar rootStatusDecoder) srootMVars
                                                                                    |> Task.bind
                                                                                        (\sroots ->
                                                                                            Task.bind (Utils.mapTraverse identity compare (Utils.readMVar statusDecoder)) (Utils.readMVar statusDictDecoder smvar)
                                                                                                |> Task.bind
                                                                                                    (\statuses ->
                                                                                                        checkMidpointAndRoots dmvar statuses sroots
                                                                                                            |> Task.bind
                                                                                                                (\midpoint ->
                                                                                                                    case midpoint of
                                                                                                                        Err problem ->
                                                                                                                            Task.pure (Err (Exit.BuildProjectProblem problem))

                                                                                                                        Ok foreigns ->
                                                                                                                            -- compile
                                                                                                                            Utils.newEmptyMVar
                                                                                                                                |> Task.bind
                                                                                                                                    (\rmvar ->
                                                                                                                                        forkWithKey identity compare bResultEncoder (checkModule env foreigns rmvar) statuses
                                                                                                                                            |> Task.bind
                                                                                                                                                (\resultsMVars ->
                                                                                                                                                    Utils.putMVar resultDictEncoder rmvar resultsMVars
                                                                                                                                                        |> Task.bind
                                                                                                                                                            (\_ ->
                                                                                                                                                                Utils.nonEmptyListTraverse (fork rootResultEncoder << checkRoot env resultsMVars) sroots
                                                                                                                                                                    |> Task.bind
                                                                                                                                                                        (\rrootMVars ->
                                                                                                                                                                            Utils.mapTraverse identity compare (Utils.readMVar bResultDecoder) resultsMVars
                                                                                                                                                                                |> Task.bind
                                                                                                                                                                                    (\results ->
                                                                                                                                                                                        writeDetails root details results
                                                                                                                                                                                            |> Task.bind
                                                                                                                                                                                                (\_ ->
                                                                                                                                                                                                    Task.fmap (toArtifacts env foreigns results) (Utils.nonEmptyListTraverse (Utils.readMVar rootResultDecoder) rrootMVars)
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


crawlDeps : Env -> MVar StatusDict -> List ModuleName.Raw -> a -> Task Never a
crawlDeps env mvar deps blockedValue =
    let
        crawlNew : ModuleName.Raw -> () -> Task Never (MVar Status)
        crawlNew name () =
            fork statusEncoder (crawlModule env mvar (DocsNeed False) name)
    in
    Utils.takeMVar statusDictDecoder mvar
        |> Task.bind
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
                    |> Task.bind
                        (\statuses ->
                            Utils.putMVar statusDictEncoder mvar (Dict.union statuses statusDict)
                                |> Task.bind
                                    (\_ ->
                                        Utils.dictMapM_ compare (Utils.readMVar statusDecoder) statuses
                                            |> Task.fmap (\_ -> blockedValue)
                                    )
                        )
            )


crawlModule : Env -> MVar StatusDict -> DocsNeed -> ModuleName.Raw -> Task Never Status
crawlModule ((Env _ root projectType srcDirs buildID locals foreigns) as env) mvar ((DocsNeed needsDocs) as docsNeed) name =
    let
        guidaFileName : String
        guidaFileName =
            ModuleName.toFilePath name ++ ".guida"

        elmFileName : String
        elmFileName =
            ModuleName.toFilePath name ++ ".elm"
    in
    Utils.filterM File.exists (List.map (flip addRelative guidaFileName) srcDirs)
        |> Task.bind
            (\guidaPaths ->
                case guidaPaths of
                    [ path ] ->
                        Task.pure [ path ]

                    _ ->
                        Utils.filterM File.exists (List.map (flip addRelative elmFileName) srcDirs)
                            |> Task.fmap (\elmPaths -> guidaPaths ++ elmPaths)
            )
        |> Task.bind
            (\paths ->
                case paths of
                    [ path ] ->
                        case Dict.get identity name foreigns of
                            Just (Details.Foreign dep deps) ->
                                Task.pure <| SBadImport <| Import.Ambiguous path [] dep deps

                            Nothing ->
                                File.getTime path
                                    |> Task.bind
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
                        Task.pure <| SBadImport <| Import.AmbiguousLocal (Utils.fpMakeRelative root p1) (Utils.fpMakeRelative root p2) (List.map (Utils.fpMakeRelative root) ps)

                    [] ->
                        case Dict.get identity name foreigns of
                            Just (Details.Foreign dep deps) ->
                                case deps of
                                    [] ->
                                        Task.pure <| SForeign dep

                                    d :: ds ->
                                        Task.pure <| SBadImport <| Import.AmbiguousForeign dep d ds

                            Nothing ->
                                if Name.isKernel name && Parse.isKernel projectType then
                                    File.exists ("src/" ++ ModuleName.toFilePath name ++ ".js")
                                        |> Task.fmap
                                            (\exists ->
                                                if exists then
                                                    SKernel

                                                else
                                                    SBadImport Import.NotFound
                                            )

                                else
                                    Task.pure <| SBadImport Import.NotFound
            )


crawlFile : Env -> MVar StatusDict -> DocsNeed -> ModuleName.Raw -> FilePath -> File.Time -> Details.BuildID -> Task Never Status
crawlFile ((Env _ root projectType _ buildID _ _) as env) mvar docsNeed expectedName path time lastChange =
    File.readUtf8 (Utils.fpCombine root path)
        |> Task.bind
            (\source ->
                case Parse.fromByteString (SV.fileSyntaxVersion path) projectType source of
                    Err err ->
                        Task.pure <| SBadSyntax path time source err

                    Ok ((Src.Module _ maybeActualName _ _ imports values _ _ _ _) as modul) ->
                        case maybeActualName of
                            Nothing ->
                                Task.pure <| SBadSyntax path time source (Syntax.ModuleNameUnspecified expectedName)

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
                                    Task.pure <| SBadSyntax path time source (Syntax.ModuleNameMismatch expectedName name)
            )


isMain : A.Located Src.Value -> Bool
isMain (A.At _ (Src.Value _ ( _, A.At _ name ) _ _ _)) =
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


checkModule : Env -> Dependencies -> MVar ResultDict -> ModuleName.Raw -> Status -> Task Never BResult
checkModule ((Env _ root projectType _ _ _ _) as env) foreigns resultsMVar name status =
    case status of
        SCached ((Details.Local path time deps hasMain lastChange lastCompile) as local) ->
            Utils.readMVar resultDictDecoder resultsMVar
                |> Task.bind
                    (\results ->
                        checkDeps root results deps lastCompile
                            |> Task.bind
                                (\depsStatus ->
                                    case depsStatus of
                                        DepsChange ifaces ->
                                            File.readUtf8 path
                                                |> Task.bind
                                                    (\source ->
                                                        case Parse.fromByteString (SV.fileSyntaxVersion path) projectType source of
                                                            Ok modul ->
                                                                compile env (DocsNeed False) local source ifaces modul

                                                            Err err ->
                                                                Task.pure <|
                                                                    RProblem <|
                                                                        Error.Module name path time source (Error.BadSyntax err)
                                                    )

                                        DepsSame _ _ ->
                                            Utils.newMVar cachedInterfaceEncoder Unneeded
                                                |> Task.fmap
                                                    (\mvar ->
                                                        RCached hasMain lastChange mvar
                                                    )

                                        DepsBlock ->
                                            Task.pure RBlocked

                                        DepsNotFound problems ->
                                            File.readUtf8 path
                                                |> Task.bind
                                                    (\source ->
                                                        Task.pure <|
                                                            RProblem <|
                                                                Error.Module name path time source <|
                                                                    case Parse.fromByteString (SV.fileSyntaxVersion path) projectType source of
                                                                        Ok (Src.Module _ _ _ _ imports _ _ _ _ _) ->
                                                                            Error.BadImports (toImportErrors env results imports problems)

                                                                        Err err ->
                                                                            Error.BadSyntax err
                                                    )
                                )
                    )

        SChanged ((Details.Local path time deps _ _ lastCompile) as local) source ((Src.Module _ _ _ _ imports _ _ _ _ _) as modul) docsNeed ->
            Utils.readMVar resultDictDecoder resultsMVar
                |> Task.bind
                    (\results ->
                        checkDeps root results deps lastCompile
                            |> Task.bind
                                (\depsStatus ->
                                    case depsStatus of
                                        DepsChange ifaces ->
                                            compile env docsNeed local source ifaces modul

                                        DepsSame same cached ->
                                            loadInterfaces root same cached
                                                |> Task.bind
                                                    (\maybeLoaded ->
                                                        case maybeLoaded of
                                                            Nothing ->
                                                                Task.pure RBlocked

                                                            Just ifaces ->
                                                                compile env docsNeed local source ifaces modul
                                                    )

                                        DepsBlock ->
                                            Task.pure RBlocked

                                        DepsNotFound problems ->
                                            Task.pure <|
                                                RProblem <|
                                                    Error.Module name path time source <|
                                                        Error.BadImports (toImportErrors env results imports problems)
                                )
                    )

        SBadImport importProblem ->
            Task.pure (RNotFound importProblem)

        SBadSyntax path time source err ->
            Task.pure <|
                RProblem <|
                    Error.Module name path time source <|
                        Error.BadSyntax err

        SForeign home ->
            case Utils.find ModuleName.toComparableCanonical (TypeCheck.Canonical home name) foreigns of
                I.Public iface ->
                    Task.pure (RForeign iface)

                I.Private _ _ _ ->
                    crash <| "mistakenly seeing private interface for " ++ Pkg.toChars home ++ " " ++ name

        SKernel ->
            Task.pure RKernel



-- CHECK DEPS


type DepsStatus
    = DepsChange (Dict String ModuleName.Raw I.Interface)
    | DepsSame (List Dep) (List CDep)
    | DepsBlock
    | DepsNotFound (NE.Nonempty ( ModuleName.Raw, Import.Problem ))


checkDeps : FilePath -> ResultDict -> List ModuleName.Raw -> Details.BuildID -> Task Never DepsStatus
checkDeps root results deps lastCompile =
    checkDepsHelp root results deps [] [] [] [] False 0 lastCompile


type alias Dep =
    ( ModuleName.Raw, I.Interface )


type alias CDep =
    ( ModuleName.Raw, MVar CachedInterface )


checkDepsHelp : FilePath -> ResultDict -> List ModuleName.Raw -> List Dep -> List Dep -> List CDep -> List ( ModuleName.Raw, Import.Problem ) -> Bool -> Details.BuildID -> Details.BuildID -> Task Never DepsStatus
checkDepsHelp root results deps new same cached importProblems isBlocked lastDepChange lastCompile =
    case deps of
        dep :: otherDeps ->
            Utils.readMVar bResultDecoder (Utils.find identity dep results)
                |> Task.bind
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
                    Task.pure <| DepsNotFound (NE.Nonempty p ps)

                [] ->
                    if isBlocked then
                        Task.pure <| DepsBlock

                    else if List.isEmpty new && lastDepChange <= lastCompile then
                        Task.pure <| DepsSame same cached

                    else
                        loadInterfaces root same cached
                            |> Task.bind
                                (\maybeLoaded ->
                                    case maybeLoaded of
                                        Nothing ->
                                            Task.pure DepsBlock

                                        Just ifaces ->
                                            Task.pure <| DepsChange <| Dict.union (Dict.fromList identity new) ifaces
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
            Dict.fromList identity (List.map (\(Src.Import ( _, A.At region name ) _ _) -> ( name, region )) imports)

        toError : ( Name.Name, Import.Problem ) -> Import.Error
        toError ( name, problem ) =
            Import.Error (Utils.find identity name regionDict) name unimportedModules problem
    in
    NE.map toError problems



-- LOAD CACHED INTERFACES


loadInterfaces : FilePath -> List Dep -> List CDep -> Task Never (Maybe (Dict String ModuleName.Raw I.Interface))
loadInterfaces root same cached =
    Utils.listTraverse (fork maybeDepEncoder << loadInterface root) cached
        |> Task.bind
            (\loading ->
                Utils.listTraverse (Utils.readMVar maybeDepDecoder) loading
                    |> Task.bind
                        (\maybeLoaded ->
                            case Utils.sequenceListMaybe maybeLoaded of
                                Nothing ->
                                    Task.pure Nothing

                                Just loaded ->
                                    Task.pure <| Just <| Dict.union (Dict.fromList identity loaded) (Dict.fromList identity same)
                        )
            )


loadInterface : FilePath -> CDep -> Task Never (Maybe Dep)
loadInterface root ( name, ciMvar ) =
    Utils.takeMVar cachedInterfaceDecoder ciMvar
        |> Task.bind
            (\cachedInterface ->
                case cachedInterface of
                    Corrupted ->
                        Utils.putMVar cachedInterfaceEncoder ciMvar cachedInterface
                            |> Task.fmap (\_ -> Nothing)

                    Loaded iface ->
                        Utils.putMVar cachedInterfaceEncoder ciMvar cachedInterface
                            |> Task.fmap (\_ -> Just ( name, iface ))

                    Unneeded ->
                        File.readBinary I.interfaceDecoder (Stuff.guidai root name)
                            |> Task.bind
                                (\maybeIface ->
                                    case maybeIface of
                                        Nothing ->
                                            Utils.putMVar cachedInterfaceEncoder ciMvar Corrupted
                                                |> Task.fmap (\_ -> Nothing)

                                        Just iface ->
                                            Utils.putMVar cachedInterfaceEncoder ciMvar (Loaded iface)
                                                |> Task.fmap (\_ -> Just ( name, iface ))
                                )
            )



-- CHECK PROJECT


checkMidpoint : MVar (Maybe Dependencies) -> Dict String ModuleName.Raw Status -> Task Never (Result Exit.BuildProjectProblem Dependencies)
checkMidpoint dmvar statuses =
    case checkForCycles statuses of
        Nothing ->
            Utils.readMVar maybeDependenciesDecoder dmvar
                |> Task.fmap
                    (\maybeForeigns ->
                        case maybeForeigns of
                            Nothing ->
                                Err Exit.BP_CannotLoadDependencies

                            Just fs ->
                                Ok fs
                    )

        Just (NE.Nonempty name names) ->
            Utils.readMVar maybeDependenciesDecoder dmvar
                |> Task.fmap (\_ -> Err (Exit.BP_Cycle name names))


checkMidpointAndRoots : MVar (Maybe Dependencies) -> Dict String ModuleName.Raw Status -> NE.Nonempty RootStatus -> Task Never (Result Exit.BuildProjectProblem Dependencies)
checkMidpointAndRoots dmvar statuses sroots =
    case checkForCycles statuses of
        Nothing ->
            case checkUniqueRoots statuses sroots of
                Nothing ->
                    Utils.readMVar maybeDependenciesDecoder dmvar
                        |> Task.bind
                            (\maybeForeigns ->
                                case maybeForeigns of
                                    Nothing ->
                                        Task.pure (Err Exit.BP_CannotLoadDependencies)

                                    Just fs ->
                                        Task.pure (Ok fs)
                            )

                Just problem ->
                    Utils.readMVar maybeDependenciesDecoder dmvar
                        |> Task.fmap (\_ -> Err problem)

        Just (NE.Nonempty name names) ->
            Utils.readMVar maybeDependenciesDecoder dmvar
                |> Task.fmap (\_ -> Err (Exit.BP_Cycle name names))



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


compile : Env -> DocsNeed -> Details.Local -> String -> Dict String ModuleName.Raw I.Interface -> Src.Module -> Task Never BResult
compile (Env key root projectType _ buildID _ _) docsNeed (Details.Local path time deps main lastChange _) source ifaces modul =
    let
        pkg : Pkg.Name
        pkg =
            projectTypeToPkg projectType
    in
    Compile.compile pkg ifaces modul
        |> Task.bind
            (\result ->
                case result of
                    Ok (Compile.Artifacts canonical annotations objects) ->
                        case makeDocs docsNeed canonical of
                            Err err ->
                                Task.pure <|
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

                                    guidai : String
                                    guidai =
                                        Stuff.guidai root name
                                in
                                File.writeBinary Opt.localGraphEncoder (Stuff.guidao root name) objects
                                    |> Task.bind
                                        (\_ ->
                                            File.readBinary I.interfaceDecoder guidai
                                                |> Task.bind
                                                    (\maybeOldi ->
                                                        case maybeOldi of
                                                            Just oldi ->
                                                                if oldi == iface then
                                                                    -- iface should be fully forced by equality check
                                                                    Reporting.report key Reporting.BDone
                                                                        |> Task.fmap
                                                                            (\_ ->
                                                                                let
                                                                                    local : Details.Local
                                                                                    local =
                                                                                        Details.Local path time deps main lastChange buildID
                                                                                in
                                                                                RSame local iface objects docs
                                                                            )

                                                                else
                                                                    File.writeBinary I.interfaceEncoder guidai iface
                                                                        |> Task.bind
                                                                            (\_ ->
                                                                                Reporting.report key Reporting.BDone
                                                                                    |> Task.fmap
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
                                                                File.writeBinary I.interfaceEncoder guidai iface
                                                                    |> Task.bind
                                                                        (\_ ->
                                                                            Reporting.report key Reporting.BDone
                                                                                |> Task.fmap
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
                        Task.pure <|
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


writeDetails : FilePath -> Details.Details -> Dict String ModuleName.Raw BResult -> Task Never ()
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


finalizeExposed : FilePath -> DocsGoal docs -> NE.Nonempty ModuleName.Raw -> Dict String ModuleName.Raw BResult -> Task Never (Result Exit.BuildProblem docs)
finalizeExposed root docsGoal exposed results =
    case List.foldr (addImportProblems results) [] (NE.toList exposed) of
        p :: ps ->
            Task.pure <| Err <| Exit.BuildProjectProblem (Exit.BP_MissingExposed (NE.Nonempty p ps))

        [] ->
            case Dict.foldr compare (\_ -> addErrors) [] results of
                [] ->
                    Task.fmap Ok (finalizeDocs docsGoal results)

                e :: es ->
                    Task.pure <| Err <| Exit.BuildBadModules root e es


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
    | WriteDocs (Dict String ModuleName.Raw BResult -> Task Never docs)
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


finalizeDocs : DocsGoal docs -> Dict String ModuleName.Raw BResult -> Task Never docs
finalizeDocs goal results =
    case goal of
        KeepDocs f ->
            Task.pure <| f results

        WriteDocs f ->
            f results

        IgnoreDocs val ->
            Task.pure val


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


fromRepl : FilePath -> Details.Details -> String -> Task Never (Result Exit.Repl ReplArtifacts)
fromRepl root details source =
    makeEnv Reporting.ignorer root details
        |> Task.bind
            (\((Env _ _ projectType _ _ _ _) as env) ->
                case Parse.fromByteString SV.Guida projectType source of
                    Err syntaxError ->
                        Task.pure <| Err <| Exit.ReplBadInput source <| Error.BadSyntax syntaxError

                    Ok ((Src.Module _ _ _ _ imports _ _ _ _ _) as modul) ->
                        Details.loadInterfaces root details
                            |> Task.bind
                                (\dmvar ->
                                    let
                                        deps : List Name.Name
                                        deps =
                                            List.map Src.getImportName imports
                                    in
                                    Utils.newMVar statusDictEncoder Dict.empty
                                        |> Task.bind
                                            (\mvar ->
                                                crawlDeps env mvar deps ()
                                                    |> Task.bind
                                                        (\_ ->
                                                            Task.bind (Utils.mapTraverse identity compare (Utils.readMVar statusDecoder)) (Utils.readMVar statusDictDecoder mvar)
                                                                |> Task.bind
                                                                    (\statuses ->
                                                                        checkMidpoint dmvar statuses
                                                                            |> Task.bind
                                                                                (\midpoint ->
                                                                                    case midpoint of
                                                                                        Err problem ->
                                                                                            Task.pure <| Err <| Exit.ReplProjectProblem problem

                                                                                        Ok foreigns ->
                                                                                            Utils.newEmptyMVar
                                                                                                |> Task.bind
                                                                                                    (\rmvar ->
                                                                                                        forkWithKey identity compare bResultEncoder (checkModule env foreigns rmvar) statuses
                                                                                                            |> Task.bind
                                                                                                                (\resultMVars ->
                                                                                                                    Utils.putMVar resultDictEncoder rmvar resultMVars
                                                                                                                        |> Task.bind
                                                                                                                            (\_ ->
                                                                                                                                Utils.mapTraverse identity compare (Utils.readMVar bResultDecoder) resultMVars
                                                                                                                                    |> Task.bind
                                                                                                                                        (\results ->
                                                                                                                                            writeDetails root details results
                                                                                                                                                |> Task.bind
                                                                                                                                                    (\_ ->
                                                                                                                                                        checkDeps root resultMVars deps 0
                                                                                                                                                            |> Task.bind
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


finalizeReplArtifacts : Env -> String -> Src.Module -> DepsStatus -> ResultDict -> Dict String ModuleName.Raw BResult -> Task Never (Result Exit.Repl ReplArtifacts)
finalizeReplArtifacts ((Env _ root projectType _ _ _ _) as env) source ((Src.Module _ _ _ _ imports _ _ _ _ _) as modul) depsStatus resultMVars results =
    let
        pkg : Pkg.Name
        pkg =
            projectTypeToPkg projectType

        compileInput : Dict String ModuleName.Raw I.Interface -> Task Never (Result Exit.Repl ReplArtifacts)
        compileInput ifaces =
            Compile.compile pkg ifaces modul
                |> Task.fmap
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
                |> Task.bind
                    (\maybeLoaded ->
                        case maybeLoaded of
                            Just ifaces ->
                                compileInput ifaces

                            Nothing ->
                                Task.pure <| Err <| Exit.ReplBadCache
                    )

        DepsBlock ->
            case Dict.foldr compare (\_ -> addErrors) [] results of
                [] ->
                    Task.pure <| Err <| Exit.ReplBlocked

                e :: es ->
                    Task.pure <| Err <| Exit.ReplBadLocalDeps root e es

        DepsNotFound problems ->
            Task.pure <|
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


findRoots : Env -> NE.Nonempty FilePath -> Task Never (Result Exit.BuildProjectProblem (NE.Nonempty RootLocation))
findRoots env paths =
    Utils.nonEmptyListTraverse (fork resultBuildProjectProblemRootInfoEncoder << getRootInfo env) paths
        |> Task.bind
            (\mvars ->
                Utils.nonEmptyListTraverse (Utils.readMVar resultBuildProjectProblemRootInfoDecoder) mvars
                    |> Task.bind
                        (\einfos ->
                            Task.pure (Result.andThen checkRoots (Utils.sequenceNonemptyListResult einfos))
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


getRootInfo : Env -> FilePath -> Task Never (Result Exit.BuildProjectProblem RootInfo)
getRootInfo env path =
    File.exists path
        |> Task.bind
            (\exists ->
                if exists then
                    Task.bind (getRootInfoHelp env path) (Utils.dirCanonicalizePath path)

                else
                    Task.pure (Err (Exit.BP_PathUnknown path))
            )


getRootInfoHelp : Env -> FilePath -> FilePath -> Task Never (Result Exit.BuildProjectProblem RootInfo)
getRootInfoHelp (Env _ _ _ srcDirs _ _ _) path absolutePath =
    let
        ( dirs, file ) =
            Utils.fpSplitFileName absolutePath

        ( final, ext ) =
            Utils.fpSplitExtension file
    in
    if List.member ext [ ".guida", ".elm" ] then
        let
            absoluteSegments : List String
            absoluteSegments =
                Utils.fpSplitDirectories dirs ++ [ final ]
        in
        case List.filterMap (isInsideSrcDirByPath absoluteSegments) srcDirs of
            [] ->
                Task.pure <| Ok <| RootInfo absolutePath path (LOutside path)

            [ ( _, Ok names ) ] ->
                let
                    name : String
                    name =
                        String.join "." names
                in
                Utils.filterM (isInsideSrcDirByName names ext) srcDirs
                    |> Task.bind
                        (\matchingDirs ->
                            case matchingDirs of
                                d1 :: d2 :: _ ->
                                    let
                                        p1 : FilePath
                                        p1 =
                                            addRelative d1 (Utils.fpJoinPath names ++ ext)

                                        p2 : FilePath
                                        p2 =
                                            addRelative d2 (Utils.fpJoinPath names ++ ext)
                                    in
                                    Task.pure <| Err <| Exit.BP_RootNameDuplicate name p1 p2

                                _ ->
                                    Task.pure <| Ok <| RootInfo absolutePath path (LInside name)
                        )

            [ ( s, Err names ) ] ->
                Task.pure <| Err <| Exit.BP_RootNameInvalid path s names

            ( s1, _ ) :: ( s2, _ ) :: _ ->
                Task.pure <| Err <| Exit.BP_WithAmbiguousSrcDir path s1 s2

    else
        Task.pure <| Err <| Exit.BP_WithBadExtension path


isInsideSrcDirByName : List String -> String -> AbsoluteSrcDir -> Task Never Bool
isInsideSrcDirByName names extension srcDir =
    File.exists (addRelative srcDir (Utils.fpJoinPath names ++ extension))


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


crawlRoot : Env -> MVar StatusDict -> RootLocation -> Task Never RootStatus
crawlRoot ((Env _ _ projectType _ buildID _ _) as env) mvar root =
    case root of
        LInside name ->
            Utils.newEmptyMVar
                |> Task.bind
                    (\statusMVar ->
                        Utils.takeMVar statusDictDecoder mvar
                            |> Task.bind
                                (\statusDict ->
                                    Utils.putMVar statusDictEncoder mvar (Dict.insert identity name statusMVar statusDict)
                                        |> Task.bind
                                            (\_ ->
                                                Task.bind (Utils.putMVar statusEncoder statusMVar) (crawlModule env mvar (DocsNeed False) name)
                                                    |> Task.fmap (\_ -> SInside name)
                                            )
                                )
                    )

        LOutside path ->
            File.getTime path
                |> Task.bind
                    (\time ->
                        File.readUtf8 path
                            |> Task.bind
                                (\source ->
                                    case Parse.fromByteString (SV.fileSyntaxVersion path) projectType source of
                                        Ok ((Src.Module _ _ _ _ imports values _ _ _ _) as modul) ->
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
                                            Task.pure <|
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


checkRoot : Env -> ResultDict -> RootStatus -> Task Never RootResult
checkRoot ((Env _ root _ _ _ _ _) as env) results rootStatus =
    case rootStatus of
        SInside name ->
            Task.pure (RInside name)

        SOutsideErr err ->
            Task.pure (ROutsideErr err)

        SOutsideOk ((Details.Local path time deps _ _ lastCompile) as local) source ((Src.Module _ _ _ _ imports _ _ _ _ _) as modul) ->
            checkDeps root results deps lastCompile
                |> Task.bind
                    (\depsStatus ->
                        case depsStatus of
                            DepsChange ifaces ->
                                compileOutside env local source ifaces modul

                            DepsSame same cached ->
                                loadInterfaces root same cached
                                    |> Task.bind
                                        (\maybeLoaded ->
                                            case maybeLoaded of
                                                Nothing ->
                                                    Task.pure ROutsideBlocked

                                                Just ifaces ->
                                                    compileOutside env local source ifaces modul
                                        )

                            DepsBlock ->
                                Task.pure ROutsideBlocked

                            DepsNotFound problems ->
                                Task.pure <|
                                    ROutsideErr <|
                                        Error.Module (Src.getName modul) path time source <|
                                            Error.BadImports (toImportErrors env results imports problems)
                    )


compileOutside : Env -> Details.Local -> String -> Dict String ModuleName.Raw I.Interface -> Src.Module -> Task Never RootResult
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
        |> Task.bind
            (\result ->
                case result of
                    Ok (Compile.Artifacts canonical annotations objects) ->
                        Reporting.report key Reporting.BDone
                            |> Task.fmap (\_ -> ROutsideOk name (I.fromModule pkg canonical annotations) objects)

                    Err errors ->
                        Task.pure <| ROutsideErr <| Error.Module name path time source errors
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


dictRawMVarBResultEncoder : Dict String ModuleName.Raw (MVar BResult) -> BE.Encoder
dictRawMVarBResultEncoder =
    BE.assocListDict compare ModuleName.rawEncoder Utils.mVarEncoder


bResultEncoder : BResult -> BE.Encoder
bResultEncoder bResult =
    case bResult of
        RNew local iface objects docs ->
            BE.sequence
                [ BE.unsignedInt8 0
                , Details.localEncoder local
                , I.interfaceEncoder iface
                , Opt.localGraphEncoder objects
                , BE.maybe Docs.bytesModuleEncoder docs
                ]

        RSame local iface objects docs ->
            BE.sequence
                [ BE.unsignedInt8 1
                , Details.localEncoder local
                , I.interfaceEncoder iface
                , Opt.localGraphEncoder objects
                , BE.maybe Docs.bytesModuleEncoder docs
                ]

        RCached main lastChange (MVar ref) ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.bool main
                , BE.int lastChange
                , BE.int ref
                ]

        RNotFound importProblem ->
            BE.sequence
                [ BE.unsignedInt8 3
                , Import.problemEncoder importProblem
                ]

        RProblem e ->
            BE.sequence
                [ BE.unsignedInt8 4
                , Error.moduleEncoder e
                ]

        RBlocked ->
            BE.unsignedInt8 5

        RForeign iface ->
            BE.sequence
                [ BE.unsignedInt8 6
                , I.interfaceEncoder iface
                ]

        RKernel ->
            BE.unsignedInt8 7


bResultDecoder : BD.Decoder BResult
bResultDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map4 RNew
                            Details.localDecoder
                            I.interfaceDecoder
                            Opt.localGraphDecoder
                            (BD.maybe Docs.bytesModuleDecoder)

                    1 ->
                        BD.map4 RSame
                            Details.localDecoder
                            I.interfaceDecoder
                            Opt.localGraphDecoder
                            (BD.maybe Docs.bytesModuleDecoder)

                    2 ->
                        BD.map3 RCached
                            BD.bool
                            BD.int
                            (BD.map MVar BD.int)

                    3 ->
                        BD.map RNotFound Import.problemDecoder

                    4 ->
                        BD.map RProblem Error.moduleDecoder

                    5 ->
                        BD.succeed RBlocked

                    6 ->
                        BD.map RForeign I.interfaceDecoder

                    7 ->
                        BD.succeed RKernel

                    _ ->
                        BD.fail
            )


statusDictEncoder : StatusDict -> BE.Encoder
statusDictEncoder statusDict =
    BE.assocListDict compare ModuleName.rawEncoder Utils.mVarEncoder statusDict


statusDictDecoder : BD.Decoder StatusDict
statusDictDecoder =
    BD.assocListDict identity ModuleName.rawDecoder Utils.mVarDecoder


statusEncoder : Status -> BE.Encoder
statusEncoder status =
    case status of
        SCached local ->
            BE.sequence
                [ BE.unsignedInt8 0
                , Details.localEncoder local
                ]

        SChanged local iface objects docs ->
            BE.sequence
                [ BE.unsignedInt8 1
                , Details.localEncoder local
                , BE.string iface
                , Src.moduleEncoder objects
                , docsNeedEncoder docs
                ]

        SBadImport importProblem ->
            BE.sequence
                [ BE.unsignedInt8 2
                , Import.problemEncoder importProblem
                ]

        SBadSyntax path time source err ->
            BE.sequence
                [ BE.unsignedInt8 3
                , BE.string path
                , File.timeEncoder time
                , BE.string source
                , Syntax.errorEncoder err
                ]

        SForeign home ->
            BE.sequence
                [ BE.unsignedInt8 4
                , Pkg.nameEncoder home
                ]

        SKernel ->
            BE.unsignedInt8 5


statusDecoder : BD.Decoder Status
statusDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map SCached Details.localDecoder

                    1 ->
                        BD.map4 SChanged
                            Details.localDecoder
                            BD.string
                            Src.moduleDecoder
                            docsNeedDecoder

                    2 ->
                        BD.map SBadImport Import.problemDecoder

                    3 ->
                        BD.map4 SBadSyntax
                            BD.string
                            File.timeDecoder
                            BD.string
                            Syntax.errorDecoder

                    4 ->
                        BD.map SForeign Pkg.nameDecoder

                    5 ->
                        BD.succeed SKernel

                    _ ->
                        BD.fail
            )


rootStatusEncoder : RootStatus -> BE.Encoder
rootStatusEncoder rootStatus =
    case rootStatus of
        SInside name ->
            BE.sequence
                [ BE.unsignedInt8 0
                , ModuleName.rawEncoder name
                ]

        SOutsideOk local source modul ->
            BE.sequence
                [ BE.unsignedInt8 1
                , Details.localEncoder local
                , BE.string source
                , Src.moduleEncoder modul
                ]

        SOutsideErr err ->
            BE.sequence
                [ BE.unsignedInt8 2
                , Error.moduleEncoder err
                ]


rootStatusDecoder : BD.Decoder RootStatus
rootStatusDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map SInside ModuleName.rawDecoder

                    1 ->
                        BD.map3 SOutsideOk
                            Details.localDecoder
                            BD.string
                            Src.moduleDecoder

                    2 ->
                        BD.map SOutsideErr Error.moduleDecoder

                    _ ->
                        BD.fail
            )


resultDictEncoder : ResultDict -> BE.Encoder
resultDictEncoder =
    BE.assocListDict compare ModuleName.rawEncoder Utils.mVarEncoder


resultDictDecoder : BD.Decoder ResultDict
resultDictDecoder =
    BD.assocListDict identity ModuleName.rawDecoder Utils.mVarDecoder


rootResultEncoder : RootResult -> BE.Encoder
rootResultEncoder rootResult =
    case rootResult of
        RInside name ->
            BE.sequence
                [ BE.unsignedInt8 0
                , ModuleName.rawEncoder name
                ]

        ROutsideOk name iface objs ->
            BE.sequence
                [ BE.unsignedInt8 1
                , ModuleName.rawEncoder name
                , I.interfaceEncoder iface
                , Opt.localGraphEncoder objs
                ]

        ROutsideErr err ->
            BE.sequence
                [ BE.unsignedInt8 2
                , Error.moduleEncoder err
                ]

        ROutsideBlocked ->
            BE.unsignedInt8 3


rootResultDecoder : BD.Decoder RootResult
rootResultDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map RInside ModuleName.rawDecoder

                    1 ->
                        BD.map3 ROutsideOk
                            ModuleName.rawDecoder
                            I.interfaceDecoder
                            Opt.localGraphDecoder

                    2 ->
                        BD.map ROutsideErr Error.moduleDecoder

                    3 ->
                        BD.succeed ROutsideBlocked

                    _ ->
                        BD.fail
            )


maybeDepEncoder : Maybe Dep -> BE.Encoder
maybeDepEncoder =
    BE.maybe depEncoder


maybeDepDecoder : BD.Decoder (Maybe Dep)
maybeDepDecoder =
    BD.maybe depDecoder


depEncoder : Dep -> BE.Encoder
depEncoder =
    BE.jsonPair ModuleName.rawEncoder I.interfaceEncoder


depDecoder : BD.Decoder Dep
depDecoder =
    BD.jsonPair ModuleName.rawDecoder I.interfaceDecoder


maybeDependenciesDecoder : BD.Decoder (Maybe Dependencies)
maybeDependenciesDecoder =
    BD.maybe (BD.assocListDict ModuleName.toComparableCanonical ModuleName.canonicalDecoder I.dependencyInterfaceDecoder)


resultBuildProjectProblemRootInfoEncoder : Result Exit.BuildProjectProblem RootInfo -> BE.Encoder
resultBuildProjectProblemRootInfoEncoder =
    BE.result Exit.buildProjectProblemEncoder rootInfoEncoder


resultBuildProjectProblemRootInfoDecoder : BD.Decoder (Result Exit.BuildProjectProblem RootInfo)
resultBuildProjectProblemRootInfoDecoder =
    BD.result Exit.buildProjectProblemDecoder rootInfoDecoder


cachedInterfaceEncoder : CachedInterface -> BE.Encoder
cachedInterfaceEncoder cachedInterface =
    case cachedInterface of
        Unneeded ->
            BE.unsignedInt8 0

        Loaded iface ->
            BE.sequence
                [ BE.unsignedInt8 1
                , I.interfaceEncoder iface
                ]

        Corrupted ->
            BE.unsignedInt8 2


cachedInterfaceDecoder : BD.Decoder CachedInterface
cachedInterfaceDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed Unneeded

                    1 ->
                        BD.map Loaded I.interfaceDecoder

                    2 ->
                        BD.succeed Corrupted

                    _ ->
                        BD.fail
            )


docsNeedEncoder : DocsNeed -> BE.Encoder
docsNeedEncoder (DocsNeed isNeeded) =
    BE.bool isNeeded


docsNeedDecoder : BD.Decoder DocsNeed
docsNeedDecoder =
    BD.map DocsNeed BD.bool


artifactsEncoder : Artifacts -> BE.Encoder
artifactsEncoder (Artifacts pkg ifaces roots modules) =
    BE.sequence
        [ Pkg.nameEncoder pkg
        , dependenciesEncoder ifaces
        , BE.nonempty rootEncoder roots
        , BE.list moduleEncoder modules
        ]


artifactsDecoder : BD.Decoder Artifacts
artifactsDecoder =
    BD.map4 Artifacts
        Pkg.nameDecoder
        dependenciesDecoder
        (BD.nonempty rootDecoder)
        (BD.list moduleDecoder)


dependenciesEncoder : Dependencies -> BE.Encoder
dependenciesEncoder =
    BE.assocListDict ModuleName.compareCanonical ModuleName.canonicalEncoder I.dependencyInterfaceEncoder


dependenciesDecoder : BD.Decoder Dependencies
dependenciesDecoder =
    BD.assocListDict ModuleName.toComparableCanonical ModuleName.canonicalDecoder I.dependencyInterfaceDecoder


rootEncoder : Root -> BE.Encoder
rootEncoder root =
    case root of
        Inside name ->
            BE.sequence
                [ BE.unsignedInt8 0
                , ModuleName.rawEncoder name
                ]

        Outside name main mvar ->
            BE.sequence
                [ BE.unsignedInt8 1
                , ModuleName.rawEncoder name
                , I.interfaceEncoder main
                , Opt.localGraphEncoder mvar
                ]


rootDecoder : BD.Decoder Root
rootDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map Inside ModuleName.rawDecoder

                    1 ->
                        BD.map3 Outside
                            ModuleName.rawDecoder
                            I.interfaceDecoder
                            Opt.localGraphDecoder

                    _ ->
                        BD.fail
            )


moduleEncoder : Module -> BE.Encoder
moduleEncoder modul =
    case modul of
        Fresh name iface objs ->
            BE.sequence
                [ BE.unsignedInt8 0
                , ModuleName.rawEncoder name
                , I.interfaceEncoder iface
                , Opt.localGraphEncoder objs
                ]

        Cached name main mvar ->
            BE.sequence
                [ BE.unsignedInt8 1
                , ModuleName.rawEncoder name
                , BE.bool main
                , Utils.mVarEncoder mvar
                ]


moduleDecoder : BD.Decoder Module
moduleDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map3 Fresh
                            ModuleName.rawDecoder
                            I.interfaceDecoder
                            Opt.localGraphDecoder

                    1 ->
                        BD.map3 Cached
                            ModuleName.rawDecoder
                            BD.bool
                            Utils.mVarDecoder

                    _ ->
                        BD.fail
            )


rootInfoEncoder : RootInfo -> BE.Encoder
rootInfoEncoder (RootInfo absolute relative location) =
    BE.sequence
        [ BE.string absolute
        , BE.string relative
        , rootLocationEncoder location
        ]


rootInfoDecoder : BD.Decoder RootInfo
rootInfoDecoder =
    BD.map3 RootInfo
        BD.string
        BD.string
        rootLocationDecoder


rootLocationEncoder : RootLocation -> BE.Encoder
rootLocationEncoder rootLocation =
    case rootLocation of
        LInside name ->
            BE.sequence
                [ BE.unsignedInt8 0
                , ModuleName.rawEncoder name
                ]

        LOutside path ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string path
                ]


rootLocationDecoder : BD.Decoder RootLocation
rootLocationDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map LInside ModuleName.rawDecoder

                    1 ->
                        BD.map LOutside BD.string

                    _ ->
                        BD.fail
            )
