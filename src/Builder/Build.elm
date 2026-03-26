module Builder.Build exposing
    ( Artifacts(..)
    , BResult
    , CachedInterface(..)
    , Dependencies
    , DocsGoal(..)
    , Module(..)
    , ReplArtifacts(..)
    , Root(..)
    , fromExposed
    , fromPaths
    , fromRepl
    , getRootNames
    , ignoreDocs
    , keepDocs
    , writeDocs
    )

import Basics.Extra exposing (flip)
import Builder.File as File
import Builder.Guida.Details as Details
import Builder.Guida.Outline as Outline
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
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.Docs as Docs
import Compiler.Guida.Interface as I
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Guida.Package as Pkg
import Compiler.Json.Encode as E
import Compiler.Parse.Module as Parse
import Compiler.Parse.SyntaxVersion as SV
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error as Error
import Compiler.Reporting.Error.Docs as EDocs
import Compiler.Reporting.Error.Import as Import
import Compiler.Reporting.Error.Syntax as Syntax
import Compiler.Reporting.Render.Type.Localizer as L
import Control.Concurrent.MVar as MVar exposing (MVar)
import Data.Graph as Graph
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet
import Process
import System.Directory as Dir
import System.TypeCheck.IO as TypeCheck
import Task exposing (Task)
import Utils.Crash exposing (crash)
import Utils.Main as Utils exposing (FilePath)
import Utils.Task.Extra as Task



-- ENVIRONMENT


type Env
    = Env Reporting.BKey Stuff.Root Parse.ProjectType (List AbsoluteSrcDir) Details.BuildID (Dict String ModuleName.Raw Details.Local) (Dict String ModuleName.Raw Details.Foreign)


makeEnv : Reporting.BKey -> Stuff.Root -> Details.Details -> Task Never Env
makeEnv key root (Details.Details _ validOutline buildID locals foreigns _) =
    case validOutline of
        Details.ValidApp givenSrcDirs ->
            Utils.listTraverse (toAbsoluteSrcDir (Stuff.rootPath root)) (NE.toList givenSrcDirs)
                |> Task.map (\srcDirs -> Env key root Parse.Application srcDirs buildID locals foreigns)

        Details.ValidPkg pkg _ _ ->
            toAbsoluteSrcDir (Stuff.rootPath root) (Outline.RelativeSrcDir "src")
                |> Task.map (\srcDir -> Env key root (Parse.Package pkg) [ srcDir ] buildID locals foreigns)



-- SOURCE DIRECTORY


type AbsoluteSrcDir
    = AbsoluteSrcDir FilePath


toAbsoluteSrcDir : FilePath -> Outline.SrcDir -> Task Never AbsoluteSrcDir
toAbsoluteSrcDir root srcDir =
    Task.map AbsoluteSrcDir
        (Dir.canonicalizePath
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
fork : Task Never a -> Task Never (MVar a)
fork work =
    MVar.newEmptyMVar
        |> Task.andThen
            (\mvar ->
                Process.spawn (Task.andThen (MVar.putMVar mvar) work)
                    |> Task.map (\_ -> mvar)
            )


forkWithKey : (k -> comparable) -> (k -> k -> Order) -> (k -> a -> Task Never b) -> Dict comparable k a -> Task Never (Dict comparable k (MVar b))
forkWithKey toComparable keyComparison func dict =
    Utils.mapTraverseWithKey toComparable keyComparison (\k v -> fork (func k v)) dict



-- FROM EXPOSED


fromExposed : Reporting.Style -> Stuff.Root -> Details.Details -> DocsGoal docs -> NE.Nonempty ModuleName.Raw -> Task Never (Result Exit.BuildProblem docs)
fromExposed style root details docsGoal ((NE.Nonempty e es) as exposed) =
    Reporting.trackBuild style <|
        \key ->
            makeEnv key root details
                |> Task.andThen
                    (\env ->
                        Details.loadInterfaces (Stuff.rootPath root) details
                            |> Task.andThen
                                (\dmvar ->
                                    -- crawl
                                    MVar.newEmptyMVar
                                        |> Task.andThen
                                            (\mvar ->
                                                let
                                                    docsNeed : DocsNeed
                                                    docsNeed =
                                                        toDocsNeed docsGoal
                                                in
                                                Map.fromKeysA identity (fork << crawlModule (Stuff.rootToTarget root) env mvar docsNeed) (e :: es)
                                                    |> Task.andThen
                                                        (\roots ->
                                                            MVar.putMVar mvar roots
                                                                |> Task.andThen
                                                                    (\_ ->
                                                                        Utils.dictMapM_ compare MVar.readMVar roots
                                                                            |> Task.andThen
                                                                                (\_ ->
                                                                                    Task.andThen (Utils.mapTraverse identity compare MVar.readMVar) (MVar.readMVar mvar)
                                                                                        |> Task.andThen
                                                                                            (\statuses ->
                                                                                                -- compile
                                                                                                checkMidpoint (Stuff.rootToTarget root) dmvar statuses
                                                                                                    |> Task.andThen
                                                                                                        (\midpoint ->
                                                                                                            case midpoint of
                                                                                                                Err problem ->
                                                                                                                    Task.succeed (Err (Exit.BuildProjectProblem problem))

                                                                                                                Ok foreigns ->
                                                                                                                    MVar.newEmptyMVar
                                                                                                                        |> Task.andThen
                                                                                                                            (\rmvar ->
                                                                                                                                forkWithKey identity compare (checkModule env foreigns rmvar) statuses
                                                                                                                                    |> Task.andThen
                                                                                                                                        (\resultMVars ->
                                                                                                                                            MVar.putMVar rmvar resultMVars
                                                                                                                                                |> Task.andThen
                                                                                                                                                    (\_ ->
                                                                                                                                                        Utils.mapTraverse identity compare MVar.readMVar resultMVars
                                                                                                                                                            |> Task.andThen
                                                                                                                                                                (\results ->
                                                                                                                                                                    writeDetails (Stuff.rootPath root) details results
                                                                                                                                                                        |> Task.andThen
                                                                                                                                                                            (\_ ->
                                                                                                                                                                                finalizeExposed (Stuff.rootPath root) docsGoal exposed results
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


fromPaths : Reporting.Style -> Stuff.Root -> Details.Details -> NE.Nonempty FilePath -> Task Never (Result Exit.BuildProblem Artifacts)
fromPaths style root details paths =
    Reporting.trackBuild style <|
        \key ->
            makeEnv key root details
                |> Task.andThen
                    (\env ->
                        findRoots env paths
                            |> Task.andThen
                                (\elroots ->
                                    case elroots of
                                        Err problem ->
                                            Task.succeed (Err (Exit.BuildProjectProblem problem))

                                        Ok lroots ->
                                            -- crawl
                                            Details.loadInterfaces (Stuff.rootPath root) details
                                                |> Task.andThen
                                                    (\dmvar ->
                                                        MVar.newMVar Dict.empty
                                                            |> Task.andThen
                                                                (\smvar ->
                                                                    Utils.nonEmptyListTraverse (fork << crawlRoot env smvar) lroots
                                                                        |> Task.andThen
                                                                            (\srootMVars ->
                                                                                Utils.nonEmptyListTraverse MVar.readMVar srootMVars
                                                                                    |> Task.andThen
                                                                                        (\sroots ->
                                                                                            Task.andThen (Utils.mapTraverse identity compare MVar.readMVar) (MVar.readMVar smvar)
                                                                                                |> Task.andThen
                                                                                                    (\statuses ->
                                                                                                        checkMidpointAndRoots (Stuff.rootToTarget root) dmvar statuses sroots
                                                                                                            |> Task.andThen
                                                                                                                (\midpoint ->
                                                                                                                    case midpoint of
                                                                                                                        Err problem ->
                                                                                                                            Task.succeed (Err (Exit.BuildProjectProblem problem))

                                                                                                                        Ok foreigns ->
                                                                                                                            -- compile
                                                                                                                            MVar.newEmptyMVar
                                                                                                                                |> Task.andThen
                                                                                                                                    (\rmvar ->
                                                                                                                                        forkWithKey identity compare (checkModule env foreigns rmvar) statuses
                                                                                                                                            |> Task.andThen
                                                                                                                                                (\resultsMVars ->
                                                                                                                                                    MVar.putMVar rmvar resultsMVars
                                                                                                                                                        |> Task.andThen
                                                                                                                                                            (\_ ->
                                                                                                                                                                Utils.nonEmptyListTraverse (fork << checkRoot env resultsMVars) sroots
                                                                                                                                                                    |> Task.andThen
                                                                                                                                                                        (\rrootMVars ->
                                                                                                                                                                            Utils.mapTraverse identity compare MVar.readMVar resultsMVars
                                                                                                                                                                                |> Task.andThen
                                                                                                                                                                                    (\results ->
                                                                                                                                                                                        writeDetails (Stuff.rootPath root) details results
                                                                                                                                                                                            |> Task.andThen
                                                                                                                                                                                                (\_ ->
                                                                                                                                                                                                    Task.map (toArtifacts env foreigns results) (Utils.nonEmptyListTraverse MVar.readMVar rrootMVars)
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


crawlDeps : Target -> Env -> MVar StatusDict -> List ModuleName.Raw -> a -> Task Never a
crawlDeps target env mvar deps blockedValue =
    let
        crawlNew : ModuleName.Raw -> () -> Task Never (MVar Status)
        crawlNew name () =
            fork (crawlModule target env mvar (DocsNeed False) name)
    in
    MVar.takeMVar mvar
        |> Task.andThen
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
                    |> Task.andThen
                        (\statuses ->
                            MVar.putMVar mvar (Dict.union statuses statusDict)
                                |> Task.andThen
                                    (\_ ->
                                        Utils.dictMapM_ compare MVar.readMVar statuses
                                            |> Task.map (\_ -> blockedValue)
                                    )
                        )
            )


crawlModule : Target -> Env -> MVar StatusDict -> DocsNeed -> ModuleName.Raw -> Task Never Status
crawlModule target ((Env _ root projectType srcDirs buildID locals foreigns) as env) mvar ((DocsNeed needsDocs) as docsNeed) name =
    let
        guidaFileName : String
        guidaFileName =
            ModuleName.toFilePath name ++ ".guida"

        elmFileName : String
        elmFileName =
            ModuleName.toFilePath name ++ ".elm"
    in
    Utils.filterM File.exists (List.map (flip addRelative guidaFileName) srcDirs)
        |> Task.andThen
            (\guidaPaths ->
                case guidaPaths of
                    [ path ] ->
                        Task.succeed [ path ]

                    _ ->
                        Utils.filterM File.exists (List.map (flip addRelative elmFileName) srcDirs)
                            |> Task.map (\elmPaths -> guidaPaths ++ elmPaths)
            )
        |> Task.andThen
            (\paths ->
                case paths of
                    [ path ] ->
                        case Dict.get identity name foreigns of
                            Just (Details.Foreign dep deps) ->
                                Task.succeed <| SBadImport <| Import.Ambiguous target path [] dep deps

                            Nothing ->
                                File.getTime path
                                    |> Task.andThen
                                        (\newTime ->
                                            case Dict.get identity name locals of
                                                Nothing ->
                                                    crawlFile target env mvar docsNeed name path newTime buildID

                                                Just ((Details.Local oldPath oldTime deps _ lastChange _) as local) ->
                                                    if path /= oldPath || oldTime /= newTime || needsDocs then
                                                        crawlFile target env mvar docsNeed name path newTime lastChange

                                                    else
                                                        crawlDeps target env mvar deps (SCached local)
                                        )

                    p1 :: p2 :: ps ->
                        Task.succeed <| SBadImport <| Import.AmbiguousLocal target (Utils.fpMakeRelative (Stuff.rootPath root) p1) (Utils.fpMakeRelative (Stuff.rootPath root) p2) (List.map (Utils.fpMakeRelative (Stuff.rootPath root)) ps)

                    [] ->
                        case Dict.get identity name foreigns of
                            Just (Details.Foreign dep deps) ->
                                case deps of
                                    [] ->
                                        Task.succeed <| SForeign dep

                                    d :: ds ->
                                        Task.succeed <| SBadImport <| Import.AmbiguousForeign target dep d ds

                            Nothing ->
                                if Name.isKernel (Stuff.rootToTarget root) name && Parse.isKernel projectType then
                                    File.exists ("src/" ++ ModuleName.toFilePath name ++ ".js")
                                        |> Task.map
                                            (\exists ->
                                                if exists then
                                                    SKernel

                                                else
                                                    SBadImport (Import.NotFound target)
                                            )

                                else
                                    Task.succeed <|
                                        SBadImport (Import.NotFound target)
            )


crawlFile : Target -> Env -> MVar StatusDict -> DocsNeed -> ModuleName.Raw -> FilePath -> File.Time -> Details.BuildID -> Task Never Status
crawlFile target ((Env _ root projectType _ buildID _ _) as env) mvar docsNeed expectedName path time lastChange =
    File.readUtf8 (Utils.fpCombine (Stuff.rootPath root) path)
        |> Task.andThen
            (\source ->
                case Parse.fromByteString target (SV.fileSyntaxVersion path) projectType source of
                    Err err ->
                        Task.succeed <| SBadSyntax path time source err

                    Ok ((Src.Module _ maybeActualName _ _ imports values _ _ _ _) as modul) ->
                        case maybeActualName of
                            Nothing ->
                                Task.succeed <| SBadSyntax path time source (Syntax.ModuleNameUnspecified expectedName)

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
                                    crawlDeps target env mvar deps (SChanged local source modul docsNeed)

                                else
                                    Task.succeed <| SBadSyntax path time source (Syntax.ModuleNameMismatch expectedName name)
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
            MVar.readMVar resultsMVar
                |> Task.andThen
                    (\results ->
                        checkDeps (Stuff.rootPath root) results deps lastCompile
                            |> Task.andThen
                                (\depsStatus ->
                                    case depsStatus of
                                        DepsChange ifaces ->
                                            File.readUtf8 path
                                                |> Task.andThen
                                                    (\source ->
                                                        case Parse.fromByteString (Stuff.rootToTarget root) (SV.fileSyntaxVersion path) projectType source of
                                                            Ok modul ->
                                                                compile (Stuff.rootToTarget root) env (DocsNeed False) local source ifaces modul

                                                            Err err ->
                                                                Task.succeed <|
                                                                    RProblem <|
                                                                        Error.Module name path time source (Error.BadSyntax err)
                                                    )

                                        DepsSame _ _ ->
                                            MVar.newMVar Unneeded
                                                |> Task.map
                                                    (\mvar ->
                                                        RCached hasMain lastChange mvar
                                                    )

                                        DepsBlock ->
                                            Task.succeed RBlocked

                                        DepsNotFound problems ->
                                            File.readUtf8 path
                                                |> Task.andThen
                                                    (\source ->
                                                        Task.succeed <|
                                                            RProblem <|
                                                                Error.Module name path time source <|
                                                                    case Parse.fromByteString (Stuff.rootToTarget root) (SV.fileSyntaxVersion path) projectType source of
                                                                        Ok (Src.Module _ _ _ _ imports _ _ _ _ _) ->
                                                                            Error.BadImports (toImportErrors env results imports problems)

                                                                        Err err ->
                                                                            Error.BadSyntax err
                                                    )
                                )
                    )

        SChanged ((Details.Local path time deps _ _ lastCompile) as local) source ((Src.Module _ _ _ _ imports _ _ _ _ _) as modul) docsNeed ->
            MVar.readMVar resultsMVar
                |> Task.andThen
                    (\results ->
                        checkDeps (Stuff.rootPath root) results deps lastCompile
                            |> Task.andThen
                                (\depsStatus ->
                                    case depsStatus of
                                        DepsChange ifaces ->
                                            compile (Stuff.rootToTarget root) env docsNeed local source ifaces modul

                                        DepsSame same cached ->
                                            loadInterfaces (Stuff.rootPath root) same cached
                                                |> Task.andThen
                                                    (\maybeLoaded ->
                                                        case maybeLoaded of
                                                            Nothing ->
                                                                Task.succeed RBlocked

                                                            Just ifaces ->
                                                                compile (Stuff.rootToTarget root) env docsNeed local source ifaces modul
                                                    )

                                        DepsBlock ->
                                            Task.succeed RBlocked

                                        DepsNotFound problems ->
                                            Task.succeed <|
                                                RProblem <|
                                                    Error.Module name path time source <|
                                                        Error.BadImports (toImportErrors env results imports problems)
                                )
                    )

        SBadImport importProblem ->
            Task.succeed (RNotFound importProblem)

        SBadSyntax path time source err ->
            Task.succeed <|
                RProblem <|
                    Error.Module name path time source <|
                        Error.BadSyntax err

        SForeign home ->
            case Utils.find ModuleName.toComparableCanonical (TypeCheck.Canonical home name) foreigns of
                I.Public iface ->
                    Task.succeed (RForeign iface)

                I.Private _ _ _ ->
                    crash <| "mistakenly seeing private interface for " ++ Pkg.toChars home ++ " " ++ name

        SKernel ->
            Task.succeed RKernel



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
            MVar.readMVar (Utils.find identity dep results)
                |> Task.andThen
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
                    Task.succeed <| DepsNotFound (NE.Nonempty p ps)

                [] ->
                    if isBlocked then
                        Task.succeed <| DepsBlock

                    else if List.isEmpty new && lastDepChange <= lastCompile then
                        Task.succeed <| DepsSame same cached

                    else
                        loadInterfaces root same cached
                            |> Task.andThen
                                (\maybeLoaded ->
                                    case maybeLoaded of
                                        Nothing ->
                                            Task.succeed DepsBlock

                                        Just ifaces ->
                                            Task.succeed <| DepsChange <| Dict.union (Dict.fromList identity new) ifaces
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
    Utils.listTraverse (fork << loadInterface root) cached
        |> Task.andThen
            (\loading ->
                Utils.listTraverse MVar.readMVar loading
                    |> Task.andThen
                        (\maybeLoaded ->
                            case Utils.sequenceListMaybe maybeLoaded of
                                Nothing ->
                                    Task.succeed Nothing

                                Just loaded ->
                                    Task.succeed <| Just <| Dict.union (Dict.fromList identity loaded) (Dict.fromList identity same)
                        )
            )


loadInterface : FilePath -> CDep -> Task Never (Maybe Dep)
loadInterface root ( name, ciMvar ) =
    MVar.takeMVar ciMvar
        |> Task.andThen
            (\cachedInterface ->
                case cachedInterface of
                    Corrupted ->
                        MVar.putMVar ciMvar cachedInterface
                            |> Task.map (\_ -> Nothing)

                    Loaded iface ->
                        MVar.putMVar ciMvar cachedInterface
                            |> Task.map (\_ -> Just ( name, iface ))

                    Unneeded ->
                        File.readBinary I.interfaceDecoder (Stuff.guidai root name)
                            |> Task.andThen
                                (\maybeIface ->
                                    case maybeIface of
                                        Nothing ->
                                            MVar.putMVar ciMvar Corrupted
                                                |> Task.map (\_ -> Nothing)

                                        Just iface ->
                                            MVar.putMVar ciMvar (Loaded iface)
                                                |> Task.map (\_ -> Just ( name, iface ))
                                )
            )



-- CHECK PROJECT


checkMidpoint : Target -> MVar (Maybe Dependencies) -> Dict String ModuleName.Raw Status -> Task Never (Result Exit.BuildProjectProblem Dependencies)
checkMidpoint target dmvar statuses =
    case checkForCycles statuses of
        Nothing ->
            MVar.readMVar dmvar
                |> Task.map
                    (\maybeForeigns ->
                        case maybeForeigns of
                            Nothing ->
                                Err Exit.BP_CannotLoadDependencies

                            Just fs ->
                                Ok fs
                    )

        Just (NE.Nonempty name names) ->
            MVar.readMVar dmvar
                |> Task.map (\_ -> Err (Exit.BP_Cycle target name names))


checkMidpointAndRoots : Target -> MVar (Maybe Dependencies) -> Dict String ModuleName.Raw Status -> NE.Nonempty RootStatus -> Task Never (Result Exit.BuildProjectProblem Dependencies)
checkMidpointAndRoots target dmvar statuses sroots =
    case checkForCycles statuses of
        Nothing ->
            case checkUniqueRoots statuses sroots of
                Nothing ->
                    MVar.readMVar dmvar
                        |> Task.andThen
                            (\maybeForeigns ->
                                case maybeForeigns of
                                    Nothing ->
                                        Task.succeed (Err Exit.BP_CannotLoadDependencies)

                                    Just fs ->
                                        Task.succeed (Ok fs)
                            )

                Just problem ->
                    MVar.readMVar dmvar
                        |> Task.map (\_ -> Err problem)

        Just (NE.Nonempty name names) ->
            MVar.readMVar dmvar
                |> Task.map (\_ -> Err (Exit.BP_Cycle target name names))



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


compile : Target -> Env -> DocsNeed -> Details.Local -> String -> Dict String ModuleName.Raw I.Interface -> Src.Module -> Task Never BResult
compile target (Env key root projectType _ buildID _ _) docsNeed (Details.Local path time deps main lastChange _) source ifaces modul =
    let
        pkg : Pkg.Name
        pkg =
            projectTypeToPkg projectType
    in
    Compile.compile target root pkg ifaces modul
        |> Task.andThen
            (\result ->
                case result of
                    Ok (Compile.Artifacts canonical annotations objects) ->
                        case makeDocs target docsNeed canonical of
                            Err err ->
                                Task.succeed <|
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
                                        Stuff.guidai (Stuff.rootPath root) name
                                in
                                File.writeBinary Opt.localGraphEncoder (Stuff.guidao (Stuff.rootPath root) name) objects
                                    |> Task.andThen
                                        (\_ ->
                                            File.readBinary I.interfaceDecoder guidai
                                                |> Task.andThen
                                                    (\maybeOldi ->
                                                        case maybeOldi of
                                                            Just oldi ->
                                                                if oldi == iface then
                                                                    -- iface should be fully forced by equality check
                                                                    Reporting.report key Reporting.BDone
                                                                        |> Task.map
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
                                                                        |> Task.andThen
                                                                            (\_ ->
                                                                                Reporting.report key Reporting.BDone
                                                                                    |> Task.map
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
                                                                    |> Task.andThen
                                                                        (\_ ->
                                                                            Reporting.report key Reporting.BDone
                                                                                |> Task.map
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
                        Task.succeed <|
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
            Task.succeed <| Err <| Exit.BuildProjectProblem (Exit.BP_MissingExposed (NE.Nonempty p ps))

        [] ->
            case Dict.foldr compare (\_ -> addErrors) [] results of
                [] ->
                    Task.map Ok (finalizeDocs docsGoal results)

                e :: es ->
                    Task.succeed <| Err <| Exit.BuildBadModules root e es


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


makeDocs : Target -> DocsNeed -> Can.Module -> Result EDocs.Error (Maybe Docs.Module)
makeDocs target (DocsNeed isNeeded) modul =
    if isNeeded then
        case Docs.fromModule target modul of
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
            Task.succeed <| f results

        WriteDocs f ->
            f results

        IgnoreDocs val ->
            Task.succeed val


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


fromRepl : Stuff.Root -> Details.Details -> String -> Task Never (Result Exit.Repl ReplArtifacts)
fromRepl root details source =
    makeEnv Reporting.ignorer root details
        |> Task.andThen
            (\((Env _ _ projectType _ _ _ _) as env) ->
                let
                    syntaxVersion : SV.SyntaxVersion
                    syntaxVersion =
                        case root of
                            Stuff.GuidaRoot _ ->
                                SV.Guida

                            Stuff.ElmRoot _ _ ->
                                SV.Elm
                in
                case Parse.fromByteString (Stuff.rootToTarget root) syntaxVersion projectType source of
                    Err syntaxError ->
                        Task.succeed <| Err <| Exit.ReplBadInput source <| Error.BadSyntax syntaxError

                    Ok ((Src.Module _ _ _ _ imports _ _ _ _ _) as modul) ->
                        Details.loadInterfaces (Stuff.rootPath root) details
                            |> Task.andThen
                                (\dmvar ->
                                    let
                                        deps : List Name.Name
                                        deps =
                                            List.map Src.getImportName imports
                                    in
                                    MVar.newMVar Dict.empty
                                        |> Task.andThen
                                            (\mvar ->
                                                crawlDeps (Stuff.rootToTarget root) env mvar deps ()
                                                    |> Task.andThen
                                                        (\_ ->
                                                            Task.andThen (Utils.mapTraverse identity compare MVar.readMVar) (MVar.readMVar mvar)
                                                                |> Task.andThen
                                                                    (\statuses ->
                                                                        checkMidpoint (Stuff.rootToTarget root) dmvar statuses
                                                                            |> Task.andThen
                                                                                (\midpoint ->
                                                                                    case midpoint of
                                                                                        Err problem ->
                                                                                            Task.succeed <| Err <| Exit.ReplProjectProblem problem

                                                                                        Ok foreigns ->
                                                                                            MVar.newEmptyMVar
                                                                                                |> Task.andThen
                                                                                                    (\rmvar ->
                                                                                                        forkWithKey identity compare (checkModule env foreigns rmvar) statuses
                                                                                                            |> Task.andThen
                                                                                                                (\resultMVars ->
                                                                                                                    MVar.putMVar rmvar resultMVars
                                                                                                                        |> Task.andThen
                                                                                                                            (\_ ->
                                                                                                                                Utils.mapTraverse identity compare MVar.readMVar resultMVars
                                                                                                                                    |> Task.andThen
                                                                                                                                        (\results ->
                                                                                                                                            writeDetails (Stuff.rootPath root) details results
                                                                                                                                                |> Task.andThen
                                                                                                                                                    (\_ ->
                                                                                                                                                        checkDeps (Stuff.rootPath root) resultMVars deps 0
                                                                                                                                                            |> Task.andThen
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
            Compile.compile (Stuff.rootToTarget root) root pkg ifaces modul
                |> Task.map
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
            loadInterfaces (Stuff.rootPath root) same cached
                |> Task.andThen
                    (\maybeLoaded ->
                        case maybeLoaded of
                            Just ifaces ->
                                compileInput ifaces

                            Nothing ->
                                Task.succeed <| Err <| Exit.ReplBadCache
                    )

        DepsBlock ->
            case Dict.foldr compare (\_ -> addErrors) [] results of
                [] ->
                    Task.succeed <| Err <| Exit.ReplBlocked

                e :: es ->
                    Task.succeed <| Err <| Exit.ReplBadLocalDeps (Stuff.rootPath root) e es

        DepsNotFound problems ->
            Task.succeed <|
                Err <|
                    Exit.ReplBadInput source <|
                        Error.BadImports <|
                            toImportErrors env resultMVars imports problems


{-| AFTER THIS, EVERYTHING IS ABOUT HANDLING MODULES GIVEN BY FILEPATH
-}



-- FIND ROOT


type RootLocation
    = LInside ModuleName.Raw
    | LOutside FilePath


findRoots : Env -> NE.Nonempty FilePath -> Task Never (Result Exit.BuildProjectProblem (NE.Nonempty RootLocation))
findRoots env paths =
    Utils.nonEmptyListTraverse (fork << getRootInfo env) paths
        |> Task.andThen
            (\mvars ->
                Utils.nonEmptyListTraverse MVar.readMVar mvars
                    |> Task.andThen
                        (\einfos ->
                            Task.succeed (Result.andThen checkRoots (Utils.sequenceNonemptyListResult einfos))
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
        |> Task.andThen
            (\exists ->
                if exists then
                    Task.andThen (getRootInfoHelp env path) (Dir.canonicalizePath path)

                else
                    Task.succeed (Err (Exit.BP_PathUnknown path))
            )


getRootInfoHelp : Env -> FilePath -> FilePath -> Task Never (Result Exit.BuildProjectProblem RootInfo)
getRootInfoHelp (Env _ root _ srcDirs _ _ _) path absolutePath =
    let
        ( dirs, file ) =
            Utils.fpSplitFileName absolutePath

        ( final, ext ) =
            Utils.fpSplitExtension file

        validExts : List String
        validExts =
            case root of
                Stuff.GuidaRoot _ ->
                    [ ".guida", ".elm" ]

                Stuff.ElmRoot _ _ ->
                    [ ".elm" ]
    in
    if List.member ext validExts then
        let
            absoluteSegments : List String
            absoluteSegments =
                Utils.fpSplitDirectories dirs ++ [ final ]
        in
        case List.filterMap (isInsideSrcDirByPath absoluteSegments) srcDirs of
            [] ->
                Task.succeed <| Ok <| RootInfo absolutePath path (LOutside path)

            [ ( _, Ok names ) ] ->
                let
                    name : String
                    name =
                        String.join "." names
                in
                Utils.filterM (isInsideSrcDirByName names ext) srcDirs
                    |> Task.andThen
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
                                    Task.succeed <| Err <| Exit.BP_RootNameDuplicate name p1 p2

                                _ ->
                                    Task.succeed <| Ok <| RootInfo absolutePath path (LInside name)
                        )

            [ ( s, Err names ) ] ->
                Task.succeed <| Err <| Exit.BP_RootNameInvalid path s names

            ( s1, _ ) :: ( s2, _ ) :: _ ->
                Task.succeed <| Err <| Exit.BP_WithAmbiguousSrcDir path s1 s2

    else
        Task.succeed <| Err <| Exit.BP_WithBadExtension (Stuff.rootToTarget root) path


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
crawlRoot ((Env _ root projectType _ buildID _ _) as env) mvar rootLocation =
    case rootLocation of
        LInside name ->
            MVar.newEmptyMVar
                |> Task.andThen
                    (\statusMVar ->
                        MVar.takeMVar mvar
                            |> Task.andThen
                                (\statusDict ->
                                    MVar.putMVar mvar (Dict.insert identity name statusMVar statusDict)
                                        |> Task.andThen
                                            (\_ ->
                                                Task.andThen (MVar.putMVar statusMVar) (crawlModule (Stuff.rootToTarget root) env mvar (DocsNeed False) name)
                                                    |> Task.map (\_ -> SInside name)
                                            )
                                )
                    )

        LOutside path ->
            File.getTime path
                |> Task.andThen
                    (\time ->
                        File.readUtf8 path
                            |> Task.andThen
                                (\source ->
                                    case Parse.fromByteString (Stuff.rootToTarget root) (SV.fileSyntaxVersion path) projectType source of
                                        Ok ((Src.Module _ _ _ _ imports values _ _ _ _) as modul) ->
                                            let
                                                deps : List Name.Name
                                                deps =
                                                    List.map Src.getImportName imports

                                                local : Details.Local
                                                local =
                                                    Details.Local path time deps (List.any isMain values) buildID buildID
                                            in
                                            crawlDeps (Stuff.rootToTarget root) env mvar deps (SOutsideOk local source modul)

                                        Err syntaxError ->
                                            Task.succeed <|
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
            Task.succeed (RInside name)

        SOutsideErr err ->
            Task.succeed (ROutsideErr err)

        SOutsideOk ((Details.Local path time deps _ _ lastCompile) as local) source ((Src.Module _ _ _ _ imports _ _ _ _ _) as modul) ->
            checkDeps (Stuff.rootPath root) results deps lastCompile
                |> Task.andThen
                    (\depsStatus ->
                        case depsStatus of
                            DepsChange ifaces ->
                                compileOutside env local source ifaces modul

                            DepsSame same cached ->
                                loadInterfaces (Stuff.rootPath root) same cached
                                    |> Task.andThen
                                        (\maybeLoaded ->
                                            case maybeLoaded of
                                                Nothing ->
                                                    Task.succeed ROutsideBlocked

                                                Just ifaces ->
                                                    compileOutside env local source ifaces modul
                                        )

                            DepsBlock ->
                                Task.succeed ROutsideBlocked

                            DepsNotFound problems ->
                                Task.succeed <|
                                    ROutsideErr <|
                                        Error.Module (Src.getName modul) path time source <|
                                            Error.BadImports (toImportErrors env results imports problems)
                    )


compileOutside : Env -> Details.Local -> String -> Dict String ModuleName.Raw I.Interface -> Src.Module -> Task Never RootResult
compileOutside (Env key root projectType _ _ _ _) (Details.Local path time _ _ _ _) source ifaces modul =
    let
        pkg : Pkg.Name
        pkg =
            projectTypeToPkg projectType

        name : Name.Name
        name =
            Src.getName modul
    in
    Compile.compile (Stuff.rootToTarget root) root pkg ifaces modul
        |> Task.andThen
            (\result ->
                case result of
                    Ok (Compile.Artifacts canonical annotations objects) ->
                        Reporting.report key Reporting.BDone
                            |> Task.map (\_ -> ROutsideOk name (I.fromModule pkg canonical annotations) objects)

                    Err errors ->
                        Task.succeed <| ROutsideErr <| Error.Module name path time source errors
            )



-- TO ARTIFACTS


type Root
    = Inside ModuleName.Raw
    | Outside ModuleName.Raw I.Interface Opt.LocalGraph


toArtifacts : Env -> Dependencies -> Dict String ModuleName.Raw BResult -> NE.Nonempty RootResult -> Result Exit.BuildProblem Artifacts
toArtifacts (Env _ root projectType _ _ _ _) foreigns results rootResults =
    case gatherProblemsOrMains results rootResults of
        Err (NE.Nonempty e es) ->
            Err (Exit.BuildBadModules (Stuff.rootPath root) e es)

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
