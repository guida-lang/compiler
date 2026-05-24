module Builder.Generate exposing
    ( debug
    , dev
    , prod
    , repl
    )

import Builder.Build as Build
import Builder.File as File
import Builder.Guida.Details as Details
import Builder.Guida.Outline as Outline
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.AST.Optimized as Opt
import Compiler.Data.Name as N
import Compiler.Data.NonEmptyList as NE
import Compiler.Generate.JavaScript as JS
import Compiler.Generate.Mode as Mode
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.Compiler.Type.Extract as Extract
import Compiler.Guida.Interface as I
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Guida.Package as Pkg
import Compiler.Nitpick.Debug as Nitpick
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as TypeCheck
import Task exposing (Task)
import Utils.Bytes.Decode as BD
import Utils.Main as Utils exposing (FilePath, MVar)
import Utils.Task.Extra as Task



-- NOTE: This is used by Make, Repl, and Reactor right now. But it may be
-- desireable to have Repl and Reactor to keep foreign objects in memory
-- to make things a bit faster?
-- GENERATORS


debug : Bool -> Int -> Stuff.Root -> Details.Details -> Build.Artifacts -> Task Exit.Generate String
debug withSourceMaps leadingLines root details (Build.Artifacts _ pkg ifaces roots modules) =
    loadObjects (Stuff.rootPath root) details modules
        |> Task.andThen
            (\loading ->
                loadTypes (Stuff.rootPath root) ifaces modules
                    |> Task.andThen
                        (\types ->
                            finalizeObjects loading
                                |> Task.andThen
                                    (\objects ->
                                        prepareSourceMaps withSourceMaps root
                                            |> Task.map
                                                (\sourceMaps ->
                                                    let
                                                        mode : Mode.Mode
                                                        mode =
                                                            Mode.Dev (Just types)

                                                        graph : Opt.GlobalGraph
                                                        graph =
                                                            objectsToGlobalGraph objects

                                                        mains : Dict (List String) TypeCheck.Canonical Opt.Main
                                                        mains =
                                                            gatherMains pkg objects roots
                                                    in
                                                    JS.generate (Stuff.rootToTarget root) sourceMaps leadingLines mode graph mains
                                                )
                                    )
                        )
            )


dev : Bool -> Int -> Stuff.Root -> Details.Details -> Build.Artifacts -> Task Exit.Generate String
dev withSourceMaps leadingLines root details (Build.Artifacts _ pkg _ roots modules) =
    Task.andThen finalizeObjects (loadObjects (Stuff.rootPath root) details modules)
        |> Task.andThen
            (\objects ->
                prepareSourceMaps withSourceMaps root
                    |> Task.map
                        (\sourceMaps ->
                            let
                                mode : Mode.Mode
                                mode =
                                    Mode.Dev Nothing

                                graph : Opt.GlobalGraph
                                graph =
                                    objectsToGlobalGraph objects

                                mains : Dict (List String) TypeCheck.Canonical Opt.Main
                                mains =
                                    gatherMains pkg objects roots
                            in
                            JS.generate (Stuff.rootToTarget root) sourceMaps leadingLines mode graph mains
                        )
            )


prod : Bool -> Int -> Stuff.Root -> Details.Details -> Build.Artifacts -> Task Exit.Generate String
prod withSourceMaps leadingLines root details (Build.Artifacts _ pkg _ roots modules) =
    Task.andThen finalizeObjects (loadObjects (Stuff.rootPath root) details modules)
        |> Task.andThen
            (\objects ->
                checkForDebugUses objects
                    |> Task.andThen
                        (\_ ->
                            prepareSourceMaps withSourceMaps root
                                |> Task.map
                                    (\sourceMaps ->
                                        let
                                            graph : Opt.GlobalGraph
                                            graph =
                                                objectsToGlobalGraph objects

                                            mode : Mode.Mode
                                            mode =
                                                Mode.Prod (Mode.shortenFieldNames graph)

                                            mains : Dict (List String) TypeCheck.Canonical Opt.Main
                                            mains =
                                                gatherMains pkg objects roots
                                        in
                                        JS.generate (Stuff.rootToTarget root) sourceMaps leadingLines mode graph mains
                                    )
                        )
            )


prepareSourceMaps : Bool -> Stuff.Root -> Task Exit.Generate JS.SourceMaps
prepareSourceMaps withSourceMaps root =
    if withSourceMaps then
        Outline.getAllModulePaths root
            |> Task.andThen (Utils.mapTraverse ModuleName.toComparableCanonical ModuleName.compareCanonical File.readUtf8)
            |> Task.map JS.SourceMaps
            |> Task.io

    else
        Task.succeed JS.NoSourceMaps


repl : Target -> FilePath -> Details.Details -> Bool -> Build.ReplArtifacts -> N.Name -> Task Exit.Generate String
repl target root details ansi (Build.ReplArtifacts home modules localizer annotations) name =
    Task.andThen finalizeObjects (loadObjects root details modules)
        |> Task.map
            (\objects ->
                let
                    graph : Opt.GlobalGraph
                    graph =
                        objectsToGlobalGraph objects
                in
                JS.generateForRepl target ansi localizer graph home name (Utils.find identity name annotations)
            )



-- CHECK FOR DEBUG


checkForDebugUses : Objects -> Task Exit.Generate ()
checkForDebugUses (Objects _ locals) =
    case Dict.keys compare (Dict.filter (\_ -> Nitpick.hasDebugUses) locals) of
        [] ->
            Task.succeed ()

        m :: ms ->
            Task.fail (Exit.GenerateCannotOptimizeDebugValues m ms)



-- GATHER MAINS


gatherMains : Pkg.Name -> Objects -> NE.Nonempty Build.Root -> Dict (List String) TypeCheck.Canonical Opt.Main
gatherMains pkg (Objects _ locals) roots =
    Dict.fromList ModuleName.toComparableCanonical (List.filterMap (lookupMain pkg locals) (NE.toList roots))


lookupMain : Pkg.Name -> Dict String ModuleName.Raw Opt.LocalGraph -> Build.Root -> Maybe ( TypeCheck.Canonical, Opt.Main )
lookupMain pkg locals root =
    let
        toPair : N.Name -> Opt.LocalGraph -> Maybe ( TypeCheck.Canonical, Opt.Main )
        toPair name (Opt.LocalGraph maybeMain _ _) =
            Maybe.map (Tuple.pair (TypeCheck.Canonical pkg name)) maybeMain
    in
    case root of
        Build.Inside name ->
            Maybe.andThen (toPair name) (Dict.get identity name locals)

        Build.Outside name _ g ->
            toPair name g



-- LOADING OBJECTS


type LoadingObjects
    = LoadingObjects (MVar (Maybe Opt.GlobalGraph)) (Dict String ModuleName.Raw (MVar (Maybe Opt.LocalGraph)))


loadObjects : FilePath -> Details.Details -> List Build.Module -> Task Exit.Generate LoadingObjects
loadObjects root details modules =
    Task.io
        (Details.loadObjects root details
            |> Task.andThen
                (\mvar ->
                    Utils.listTraverse (loadObject root) modules
                        |> Task.map
                            (\mvars ->
                                LoadingObjects mvar (Dict.fromList identity mvars)
                            )
                )
        )


loadObject : FilePath -> Build.Module -> Task Never ( ModuleName.Raw, MVar (Maybe Opt.LocalGraph) )
loadObject root modul =
    case modul of
        Build.Fresh name _ graph ->
            Utils.newMVar (Utils.maybeEncoder Opt.localGraphEncoder) (Just graph)
                |> Task.map (\mvar -> ( name, mvar ))

        Build.Cached name _ _ ->
            Utils.newEmptyMVar
                |> Task.andThen
                    (\mvar ->
                        Utils.forkIO (Task.andThen (Utils.putMVar (Utils.maybeEncoder Opt.localGraphEncoder) mvar) (File.readBinary Opt.localGraphDecoder (Stuff.guidao root name)))
                            |> Task.map (\_ -> ( name, mvar ))
                    )



-- FINALIZE OBJECTS


type Objects
    = Objects Opt.GlobalGraph (Dict String ModuleName.Raw Opt.LocalGraph)


finalizeObjects : LoadingObjects -> Task Exit.Generate Objects
finalizeObjects (LoadingObjects mvar mvars) =
    Task.eio identity
        (Utils.readMVar (BD.maybe Opt.globalGraphDecoder) mvar
            |> Task.andThen
                (\result ->
                    Utils.mapTraverse identity compare (Utils.readMVar (BD.maybe Opt.localGraphDecoder)) mvars
                        |> Task.map
                            (\results ->
                                case Maybe.map2 Objects result (Utils.sequenceDictMaybe identity compare results) of
                                    Just loaded ->
                                        Ok loaded

                                    Nothing ->
                                        Err Exit.GenerateCannotLoadArtifacts
                            )
                )
        )


objectsToGlobalGraph : Objects -> Opt.GlobalGraph
objectsToGlobalGraph (Objects globals locals) =
    Dict.foldr compare (\_ -> Opt.addLocalGraph) globals locals



-- LOAD TYPES


loadTypes : FilePath -> Dict (List String) TypeCheck.Canonical I.DependencyInterface -> List Build.Module -> Task Exit.Generate Extract.Types
loadTypes root ifaces modules =
    Task.eio identity
        (Utils.listTraverse (loadTypesHelp root) modules
            |> Task.andThen
                (\mvars ->
                    Utils.listTraverse (Utils.readMVar (BD.maybe Extract.typesDecoder)) mvars
                        |> Task.map
                            (\results ->
                                case Utils.sequenceListMaybe results of
                                    Just ts ->
                                        let
                                            foreigns : Extract.Types
                                            foreigns =
                                                Extract.mergeMany (Dict.values ModuleName.compareCanonical (Dict.map Extract.fromDependencyInterface ifaces))
                                        in
                                        Ok (Extract.merge foreigns (Extract.mergeMany ts))

                                    Nothing ->
                                        Err Exit.GenerateCannotLoadArtifacts
                            )
                )
        )


loadTypesHelp : FilePath -> Build.Module -> Task Never (MVar (Maybe Extract.Types))
loadTypesHelp root modul =
    case modul of
        Build.Fresh name iface _ ->
            Utils.newMVar (Utils.maybeEncoder Extract.typesEncoder) (Just (Extract.fromInterface name iface))

        Build.Cached name _ ciMVar ->
            Utils.readMVar Build.cachedInterfaceDecoder ciMVar
                |> Task.andThen
                    (\cachedInterface ->
                        case cachedInterface of
                            Build.Unneeded ->
                                Utils.newEmptyMVar
                                    |> Task.andThen
                                        (\mvar ->
                                            Utils.forkIO
                                                (File.readBinary I.interfaceDecoder (Stuff.guidai root name)
                                                    |> Task.andThen
                                                        (\maybeIface ->
                                                            Utils.putMVar (Utils.maybeEncoder Extract.typesEncoder) mvar (Maybe.map (Extract.fromInterface name) maybeIface)
                                                        )
                                                )
                                                |> Task.map (\_ -> mvar)
                                        )

                            Build.Loaded iface ->
                                Utils.newMVar (Utils.maybeEncoder Extract.typesEncoder) (Just (Extract.fromInterface name iface))

                            Build.Corrupted ->
                                Utils.newMVar (Utils.maybeEncoder Extract.typesEncoder) Nothing
                    )
