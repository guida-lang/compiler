module Builder.Generate exposing
    ( Task
    , debug
    , dev
    , prod
    , repl
    )

import Builder.Build as Build
import Builder.Elm.Details as Details
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
import Json.Decode as Decode
import System.IO as IO exposing (IO)
import System.TypeCheck.IO as TypeCheck
import Utils.Main as Utils exposing (FilePath, MVar)



-- NOTE: This is used by Make, Repl, and Reactor right now. But it may be
-- desireable to have Repl and Reactor to keep foreign objects in memory
-- to make things a bit faster?
-- GENERATORS


type alias Task a =
    Task.Task Exit.Generate a


debug : FilePath -> Details.Details -> Build.Artifacts -> Task String
debug root details (Build.Artifacts pkg ifaces roots modules) =
    loadObjects root details modules
        |> Task.bind
            (\loading ->
                loadTypes root ifaces modules
                    |> Task.bind
                        (\types ->
                            finalizeObjects loading
                                |> Task.fmap
                                    (\objects ->
                                        let
                                            mode : Mode.Mode
                                            mode =
                                                Mode.Dev (Just types)

                                            graph : Opt.GlobalGraph
                                            graph =
                                                objectsToGlobalGraph objects

                                            mains : Dict TypeCheck.Canonical Opt.Main
                                            mains =
                                                gatherMains pkg objects roots
                                        in
                                        JS.generate mode graph mains
                                    )
                        )
            )


dev : FilePath -> Details.Details -> Build.Artifacts -> Task String
dev root details (Build.Artifacts pkg _ roots modules) =
    Task.bind finalizeObjects (loadObjects root details modules)
        |> Task.fmap
            (\objects ->
                let
                    mode : Mode.Mode
                    mode =
                        Mode.Dev Nothing

                    graph : Opt.GlobalGraph
                    graph =
                        objectsToGlobalGraph objects

                    mains : Dict TypeCheck.Canonical Opt.Main
                    mains =
                        gatherMains pkg objects roots
                in
                JS.generate mode graph mains
            )


prod : FilePath -> Details.Details -> Build.Artifacts -> Task String
prod root details (Build.Artifacts pkg _ roots modules) =
    Task.bind finalizeObjects (loadObjects root details modules)
        |> Task.bind
            (\objects ->
                checkForDebugUses objects
                    |> Task.fmap
                        (\_ ->
                            let
                                graph : Opt.GlobalGraph
                                graph =
                                    objectsToGlobalGraph objects

                                mode : Mode.Mode
                                mode =
                                    Mode.Prod (Mode.shortenFieldNames graph)

                                mains : Dict TypeCheck.Canonical Opt.Main
                                mains =
                                    gatherMains pkg objects roots
                            in
                            JS.generate mode graph mains
                        )
            )


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
                JS.generateForRepl ansi localizer graph home name (Utils.find name annotations)
            )



-- CHECK FOR DEBUG


checkForDebugUses : Objects -> Task ()
checkForDebugUses (Objects _ locals) =
    case Dict.keys (Dict.filter (\_ -> Nitpick.hasDebugUses) locals) of
        [] ->
            Task.pure ()

        m :: ms ->
            Task.throw (Exit.GenerateCannotOptimizeDebugValues m ms)



-- GATHER MAINS


gatherMains : Pkg.Name -> Objects -> NE.Nonempty Build.Root -> Dict TypeCheck.Canonical Opt.Main
gatherMains pkg (Objects _ locals) roots =
    Dict.fromList ModuleName.compareCanonical (List.filterMap (lookupMain pkg locals) (NE.toList roots))


lookupMain : Pkg.Name -> Dict ModuleName.Raw Opt.LocalGraph -> Build.Root -> Maybe ( TypeCheck.Canonical, Opt.Main )
lookupMain pkg locals root =
    let
        toPair : N.Name -> Opt.LocalGraph -> Maybe ( TypeCheck.Canonical, Opt.Main )
        toPair name (Opt.LocalGraph maybeMain _ _) =
            Maybe.map (Tuple.pair (TypeCheck.Canonical pkg name)) maybeMain
    in
    case root of
        Build.Inside name ->
            Maybe.andThen (toPair name) (Dict.get name locals)

        Build.Outside name _ g ->
            toPair name g



-- LOADING OBJECTS


type LoadingObjects
    = LoadingObjects (MVar (Maybe Opt.GlobalGraph)) (Dict ModuleName.Raw (MVar (Maybe Opt.LocalGraph)))


loadObjects : FilePath -> Details.Details -> List Build.Module -> Task LoadingObjects
loadObjects root details modules =
    Task.io
        (Details.loadObjects root details
            |> IO.bind
                (\mvar ->
                    Utils.listTraverse (loadObject root) modules
                        |> IO.fmap
                            (\mvars ->
                                LoadingObjects mvar (Dict.fromList compare mvars)
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
                        Utils.forkIO (IO.bind (Utils.putMVar (Utils.maybeEncoder Opt.localGraphEncoder) mvar) (File.readBinary Opt.localGraphDecoder (Stuff.elmo root name)))
                            |> IO.fmap (\_ -> ( name, mvar ))
                    )



-- FINALIZE OBJECTS


type Objects
    = Objects Opt.GlobalGraph (Dict ModuleName.Raw Opt.LocalGraph)


finalizeObjects : LoadingObjects -> Task Objects
finalizeObjects (LoadingObjects mvar mvars) =
    Task.eio identity
        (Utils.readMVar (Decode.maybe Opt.globalGraphDecoder) mvar
            |> IO.bind
                (\result ->
                    Utils.mapTraverse compare (Utils.readMVar (Decode.maybe Opt.localGraphDecoder)) mvars
                        |> IO.fmap
                            (\results ->
                                case Maybe.map2 Objects result (Utils.sequenceDictMaybe compare results) of
                                    Just loaded ->
                                        Ok loaded

                                    Nothing ->
                                        Err Exit.GenerateCannotLoadArtifacts
                            )
                )
        )


objectsToGlobalGraph : Objects -> Opt.GlobalGraph
objectsToGlobalGraph (Objects globals locals) =
    Dict.foldr (\_ -> Opt.addLocalGraph) globals locals



-- LOAD TYPES


loadTypes : FilePath -> Dict TypeCheck.Canonical I.DependencyInterface -> List Build.Module -> Task Extract.Types
loadTypes root ifaces modules =
    Task.eio identity
        (Utils.listTraverse (loadTypesHelp root) modules
            |> IO.bind
                (\mvars ->
                    let
                        foreigns : Extract.Types
                        foreigns =
                            Extract.mergeMany (Dict.values (Dict.map Extract.fromDependencyInterface ifaces))
                    in
                    Utils.listTraverse (Utils.readMVar (Decode.maybe Extract.typesDecoder)) mvars
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
                                                (File.readBinary I.interfaceDecoder (Stuff.elmi root name)
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
