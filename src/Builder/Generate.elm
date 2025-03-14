module Builder.Generate exposing
    ( Task
    , debug
    , dev
    , prod
    , repl
    )

import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.AST.Optimized as Opt
import Compiler.Data.Name as N
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Compiler.Type.Extract as Extract
import Compiler.Elm.Interface as I
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Generate.JavaScript as JS
import Compiler.Generate.Mode as Mode
import Compiler.Nitpick.Debug as Nitpick
import Data.Map as Dict exposing (Dict)
import System.IO as IO exposing (IO)
import System.TypeCheck.IO as TypeCheck
import Utils.Bytes.Decode as BD
import Utils.Main as Utils exposing (FilePath, MVar)



-- NOTE: This is used by Make, Repl, and Reactor right now. But it may be
-- desireable to have Repl and Reactor to keep foreign objects in memory
-- to make things a bit faster?
-- GENERATORS


type alias Task a =
    Task.Task Exit.Generate a


debug : Bool -> Int -> FilePath -> Details.Details -> Build.Artifacts -> Task String
debug withSourceMaps leadingLines root details (Build.Artifacts pkg ifaces roots modules) =
    loadObjects root details modules
        |> Task.bind
            (\loading ->
                loadTypes root ifaces modules
                    |> Task.bind
                        (\types ->
                            finalizeObjects loading
                                |> Task.bind
                                    (\objects ->
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
                                        prepareSourceMaps withSourceMaps root
                                            |> Task.fmap (\sourceMaps -> JS.generate sourceMaps leadingLines mode graph mains)
                                    )
                        )
            )


dev : Bool -> Int -> FilePath -> Details.Details -> Build.Artifacts -> Task String
dev withSourceMaps leadingLines root details (Build.Artifacts pkg _ roots modules) =
    Task.bind finalizeObjects (loadObjects root details modules)
        |> Task.bind
            (\objects ->
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
                prepareSourceMaps withSourceMaps root
                    |> Task.fmap (\sourceMaps -> JS.generate sourceMaps leadingLines mode graph mains)
            )


prod : Bool -> Int -> FilePath -> Details.Details -> Build.Artifacts -> Task String
prod withSourceMaps leadingLines root details (Build.Artifacts pkg _ roots modules) =
    Task.bind finalizeObjects (loadObjects root details modules)
        |> Task.bind
            (\objects ->
                checkForDebugUses objects
                    |> Task.bind
                        (\_ ->
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
                            prepareSourceMaps withSourceMaps root
                                |> Task.fmap (\sourceMaps -> JS.generate sourceMaps leadingLines mode graph mains)
                        )
            )


prepareSourceMaps : Bool -> FilePath -> Task JS.SourceMaps
prepareSourceMaps withSourceMaps root =
    if withSourceMaps then
        Outline.getAllModulePaths root
            |> IO.bind (Utils.mapTraverse ModuleName.toComparableCanonical ModuleName.compareCanonical File.readUtf8)
            |> IO.fmap JS.SourceMaps
            |> Task.io

    else
        Task.pure JS.NoSourceMaps


repl : FilePath -> Details.Details -> Bool -> Build.ReplArtifacts -> N.Name -> Task String
repl root details ansi (Build.ReplArtifacts home modules localizer annotations) name =
    Task.bind finalizeObjects (loadObjects root details modules)
        |> Task.fmap
            (\objects ->
                let
                    graph : Opt.GlobalGraph
                    graph =
                        objectsToGlobalGraph objects
                in
                JS.generateForRepl ansi localizer graph home name (Utils.find identity name annotations)
            )



-- CHECK FOR DEBUG


checkForDebugUses : Objects -> Task ()
checkForDebugUses (Objects _ locals) =
    case Dict.keys compare (Dict.filter (\_ -> Nitpick.hasDebugUses) locals) of
        [] ->
            Task.pure ()

        m :: ms ->
            Task.throw (Exit.GenerateCannotOptimizeDebugValues m ms)



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


loadObjects : FilePath -> Details.Details -> List Build.Module -> Task LoadingObjects
loadObjects root details modules =
    Task.io
        (Details.loadObjects root details
            |> IO.bind
                (\mvar ->
                    Utils.listTraverse (loadObject root) modules
                        |> IO.fmap
                            (\mvars ->
                                LoadingObjects mvar (Dict.fromList identity mvars)
                            )
                )
        )


loadObject : FilePath -> Build.Module -> IO ( ModuleName.Raw, MVar (Maybe Opt.LocalGraph) )
loadObject root modul =
    case modul of
        Build.Fresh name _ graph ->
            Utils.newMVar (Utils.maybeEncoder Opt.localGraphEncoder) (Just graph)
                |> IO.fmap (\mvar -> ( name, mvar ))

        Build.Cached name _ _ ->
            Utils.newEmptyMVar
                |> IO.bind
                    (\mvar ->
                        Utils.forkIO (IO.bind (Utils.putMVar (Utils.maybeEncoder Opt.localGraphEncoder) mvar) (File.readBinary Opt.localGraphDecoder (Stuff.guidao root name)))
                            |> IO.fmap (\_ -> ( name, mvar ))
                    )



-- FINALIZE OBJECTS


type Objects
    = Objects Opt.GlobalGraph (Dict String ModuleName.Raw Opt.LocalGraph)


finalizeObjects : LoadingObjects -> Task Objects
finalizeObjects (LoadingObjects mvar mvars) =
    Task.eio identity
        (Utils.readMVar (BD.maybe Opt.globalGraphDecoder) mvar
            |> IO.bind
                (\result ->
                    Utils.mapTraverse identity compare (Utils.readMVar (BD.maybe Opt.localGraphDecoder)) mvars
                        |> IO.fmap
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


loadTypes : FilePath -> Dict (List String) TypeCheck.Canonical I.DependencyInterface -> List Build.Module -> Task Extract.Types
loadTypes root ifaces modules =
    Task.eio identity
        (Utils.listTraverse (loadTypesHelp root) modules
            |> IO.bind
                (\mvars ->
                    let
                        foreigns : Extract.Types
                        foreigns =
                            Extract.mergeMany (Dict.values ModuleName.compareCanonical (Dict.map Extract.fromDependencyInterface ifaces))
                    in
                    Utils.listTraverse (Utils.readMVar (BD.maybe Extract.typesDecoder)) mvars
                        |> IO.fmap
                            (\results ->
                                case Utils.sequenceListMaybe results of
                                    Just ts ->
                                        Ok (Extract.merge foreigns (Extract.mergeMany ts))

                                    Nothing ->
                                        Err Exit.GenerateCannotLoadArtifacts
                            )
                )
        )


loadTypesHelp : FilePath -> Build.Module -> IO (MVar (Maybe Extract.Types))
loadTypesHelp root modul =
    case modul of
        Build.Fresh name iface _ ->
            Utils.newMVar (Utils.maybeEncoder Extract.typesEncoder) (Just (Extract.fromInterface name iface))

        Build.Cached name _ ciMVar ->
            Utils.readMVar Build.cachedInterfaceDecoder ciMVar
                |> IO.bind
                    (\cachedInterface ->
                        case cachedInterface of
                            Build.Unneeded ->
                                Utils.newEmptyMVar
                                    |> IO.bind
                                        (\mvar ->
                                            Utils.forkIO
                                                (File.readBinary I.interfaceDecoder (Stuff.guidai root name)
                                                    |> IO.bind
                                                        (\maybeIface ->
                                                            Utils.putMVar (Utils.maybeEncoder Extract.typesEncoder) mvar (Maybe.map (Extract.fromInterface name) maybeIface)
                                                        )
                                                )
                                                |> IO.fmap (\_ -> mvar)
                                        )

                            Build.Loaded iface ->
                                Utils.newMVar (Utils.maybeEncoder Extract.typesEncoder) (Just (Extract.fromInterface name iface))

                            Build.Corrupted ->
                                Utils.newMVar (Utils.maybeEncoder Extract.typesEncoder) Nothing
                    )
