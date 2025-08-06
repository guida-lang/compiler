module Builder.Generate exposing
    ( debug
    , dev
    , prod
    , repl
    )

import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Reporting.Exit as Exit
import Utils.Task.Extra as TE
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
import Utils.Task.Extra as TE
import System.TypeCheck.IO as TypeCheck
import Task exposing (Task)
import Utils.Bytes.Decode as BD
import Utils.Main as Utils exposing (FilePath, MVar)



-- NOTE: This is used by Make, Repl, and Reactor right now. But it may be
-- desireable to have Repl and Reactor to keep foreign objects in memory
-- to make things a bit faster?
-- GENERATORS


debug : Bool -> Int -> FilePath -> Details.Details -> Build.Artifacts -> Task Exit.Generate String
debug withSourceMaps leadingLines root details (Build.Artifacts pkg ifaces roots modules) =
    loadObjects root details modules
        |> TE.bind
            (\loading ->
                loadTypes root ifaces modules
                    |> TE.bind
                        (\types ->
                            finalizeObjects loading
                                |> TE.bind
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
                                            |> TE.fmap (\sourceMaps -> JS.generate sourceMaps leadingLines mode graph mains)
                                    )
                        )
            )


dev : Bool -> Int -> FilePath -> Details.Details -> Build.Artifacts -> Task Exit.Generate String
dev withSourceMaps leadingLines root details (Build.Artifacts pkg _ roots modules) =
    TE.bind finalizeObjects (loadObjects root details modules)
        |> TE.bind
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
                    |> TE.fmap (\sourceMaps -> JS.generate sourceMaps leadingLines mode graph mains)
            )


prod : Bool -> Int -> FilePath -> Details.Details -> Build.Artifacts -> Task Exit.Generate String
prod withSourceMaps leadingLines root details (Build.Artifacts pkg _ roots modules) =
    TE.bind finalizeObjects (loadObjects root details modules)
        |> TE.bind
            (\objects ->
                checkForDebugUses objects
                    |> TE.bind
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
                                |> TE.fmap (\sourceMaps -> JS.generate sourceMaps leadingLines mode graph mains)
                        )
            )


prepareSourceMaps : Bool -> FilePath -> Task Exit.Generate JS.SourceMaps
prepareSourceMaps withSourceMaps root =
    if withSourceMaps then
        Outline.getAllModulePaths root
            |> TE.bind (Utils.mapTraverse ModuleName.toComparableCanonical ModuleName.compareCanonical File.readUtf8)
            |> TE.fmap JS.SourceMaps
            |> TE.io

    else
        TE.pure JS.NoSourceMaps


repl : FilePath -> Details.Details -> Bool -> Build.ReplArtifacts -> N.Name -> Task Exit.Generate String
repl root details ansi (Build.ReplArtifacts home modules localizer annotations) name =
    TE.bind finalizeObjects (loadObjects root details modules)
        |> TE.fmap
            (\objects ->
                let
                    graph : Opt.GlobalGraph
                    graph =
                        objectsToGlobalGraph objects
                in
                JS.generateForRepl ansi localizer graph home name (Utils.find identity name annotations)
            )



-- CHECK FOR DEBUG


checkForDebugUses : Objects -> Task Exit.Generate ()
checkForDebugUses (Objects _ locals) =
    case Dict.keys compare (Dict.filter (\_ -> Nitpick.hasDebugUses) locals) of
        [] ->
            TE.pure ()

        m :: ms ->
            TE.throw (Exit.GenerateCannotOptimizeDebugValues m ms)



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
    TE.io
        (Details.loadObjects root details
            |> TE.bind
                (\mvar ->
                    Utils.listTraverse (loadObject root) modules
                        |> TE.fmap
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
                |> TE.fmap (\mvar -> ( name, mvar ))

        Build.Cached name _ _ ->
            Utils.newEmptyMVar
                |> TE.bind
                    (\mvar ->
                        Utils.forkIO (TE.bind (Utils.putMVar (Utils.maybeEncoder Opt.localGraphEncoder) mvar) (File.readBinary Opt.localGraphDecoder (Stuff.guidao root name)))
                            |> TE.fmap (\_ -> ( name, mvar ))
                    )



-- FINALIZE OBJECTS


type Objects
    = Objects Opt.GlobalGraph (Dict String ModuleName.Raw Opt.LocalGraph)


finalizeObjects : LoadingObjects -> Task Exit.Generate Objects
finalizeObjects (LoadingObjects mvar mvars) =
    TE.eio identity
        (Utils.readMVar (BD.maybe Opt.globalGraphDecoder) mvar
            |> TE.bind
                (\result ->
                    Utils.mapTraverse identity compare (Utils.readMVar (BD.maybe Opt.localGraphDecoder)) mvars
                        |> TE.fmap
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
    TE.eio identity
        (Utils.listTraverse (loadTypesHelp root) modules
            |> TE.bind
                (\mvars ->
                    let
                        foreigns : Extract.Types
                        foreigns =
                            Extract.mergeMany (Dict.values ModuleName.compareCanonical (Dict.map Extract.fromDependencyInterface ifaces))
                    in
                    Utils.listTraverse (Utils.readMVar (BD.maybe Extract.typesDecoder)) mvars
                        |> TE.fmap
                            (\results ->
                                case Utils.sequenceListMaybe results of
                                    Just ts ->
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
                |> TE.bind
                    (\cachedInterface ->
                        case cachedInterface of
                            Build.Unneeded ->
                                Utils.newEmptyMVar
                                    |> TE.bind
                                        (\mvar ->
                                            Utils.forkIO
                                                (File.readBinary I.interfaceDecoder (Stuff.guidai root name)
                                                    |> TE.bind
                                                        (\maybeIface ->
                                                            Utils.putMVar (Utils.maybeEncoder Extract.typesEncoder) mvar (Maybe.map (Extract.fromInterface name) maybeIface)
                                                        )
                                                )
                                                |> TE.fmap (\_ -> mvar)
                                        )

                            Build.Loaded iface ->
                                Utils.newMVar (Utils.maybeEncoder Extract.typesEncoder) (Just (Extract.fromInterface name iface))

                            Build.Corrupted ->
                                Utils.newMVar (Utils.maybeEncoder Extract.typesEncoder) Nothing
                    )
