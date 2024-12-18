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
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Compiler.Type.Extract as Extract
import Compiler.Elm.Interface as I
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Generate.JavaScript as JS
import Compiler.Generate.Mode as Mode
import Compiler.Nitpick.Debug as Nitpick
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import System.IO as IO exposing (IO)
import Types as T
import Utils.Main as Utils



-- NOTE: This is used by Make, Repl, and Reactor right now. But it may be
-- desireable to have Repl and Reactor to keep foreign objects in memory
-- to make things a bit faster?
-- GENERATORS


type alias Task a =
    Task.Task Exit.Generate a


debug : T.FilePath -> Details.Details -> Build.Artifacts -> Task String
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

                                            graph : T.CASTO_GlobalGraph
                                            graph =
                                                objectsToGlobalGraph objects

                                            mains : Dict (List String) T.CEMN_Canonical T.CASTO_Main
                                            mains =
                                                gatherMains pkg objects roots
                                        in
                                        JS.generate mode graph mains
                                    )
                        )
            )


dev : T.FilePath -> Details.Details -> Build.Artifacts -> Task String
dev root details (Build.Artifacts pkg _ roots modules) =
    Task.bind finalizeObjects (loadObjects root details modules)
        |> Task.fmap
            (\objects ->
                let
                    mode : Mode.Mode
                    mode =
                        Mode.Dev Nothing

                    graph : T.CASTO_GlobalGraph
                    graph =
                        objectsToGlobalGraph objects

                    mains : Dict (List String) T.CEMN_Canonical T.CASTO_Main
                    mains =
                        gatherMains pkg objects roots
                in
                JS.generate mode graph mains
            )


prod : T.FilePath -> Details.Details -> Build.Artifacts -> Task String
prod root details (Build.Artifacts pkg _ roots modules) =
    Task.bind finalizeObjects (loadObjects root details modules)
        |> Task.bind
            (\objects ->
                checkForDebugUses objects
                    |> Task.fmap
                        (\_ ->
                            let
                                graph : T.CASTO_GlobalGraph
                                graph =
                                    objectsToGlobalGraph objects

                                mode : Mode.Mode
                                mode =
                                    Mode.Prod (Mode.shortenFieldNames graph)

                                mains : Dict (List String) T.CEMN_Canonical T.CASTO_Main
                                mains =
                                    gatherMains pkg objects roots
                            in
                            JS.generate mode graph mains
                        )
            )


repl : T.FilePath -> Details.Details -> Bool -> Build.ReplArtifacts -> T.CDN_Name -> Task String
repl root details ansi (Build.ReplArtifacts home modules localizer annotations) name =
    Task.bind finalizeObjects (loadObjects root details modules)
        |> Task.fmap
            (\objects ->
                let
                    graph : T.CASTO_GlobalGraph
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


gatherMains : T.CEP_Name -> Objects -> NE.Nonempty Build.Root -> Dict (List String) T.CEMN_Canonical T.CASTO_Main
gatherMains pkg (Objects _ locals) roots =
    Dict.fromList ModuleName.toComparableCanonical (List.filterMap (lookupMain pkg locals) (NE.toList roots))


lookupMain : T.CEP_Name -> Dict String T.CEMN_Raw T.CASTO_LocalGraph -> Build.Root -> Maybe ( T.CEMN_Canonical, T.CASTO_Main )
lookupMain pkg locals root =
    let
        toPair : T.CDN_Name -> T.CASTO_LocalGraph -> Maybe ( T.CEMN_Canonical, T.CASTO_Main )
        toPair name (T.CASTO_LocalGraph maybeMain _ _) =
            Maybe.map (Tuple.pair (T.CEMN_Canonical pkg name)) maybeMain
    in
    case root of
        Build.Inside name ->
            Maybe.andThen (toPair name) (Dict.get identity name locals)

        Build.Outside name _ g ->
            toPair name g



-- LOADING OBJECTS


type LoadingObjects
    = LoadingObjects (T.MVar (Maybe T.CASTO_GlobalGraph)) (Dict String T.CEMN_Raw (T.MVar (Maybe T.CASTO_LocalGraph)))


loadObjects : T.FilePath -> Details.Details -> List Build.Module -> Task LoadingObjects
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


loadObject : T.FilePath -> Build.Module -> IO ( T.CEMN_Raw, T.MVar (Maybe T.CASTO_LocalGraph) )
loadObject root modul =
    case modul of
        Build.Fresh name _ graph ->
            Utils.newMVar_Maybe_CASTO_LocalGraph (Just graph)
                |> IO.fmap (\mvar -> ( name, mvar ))

        Build.Cached name _ _ ->
            Utils.newEmptyMVar_Maybe_CASTO_LocalGraph
                |> IO.bind
                    (\mvar ->
                        Utils.forkIO (IO.bind (Utils.putMVar_Maybe_CASTO_LocalGraph mvar) (File.readBinary Opt.localGraphDecoder (Stuff.elmo root name)))
                            |> IO.fmap (\_ -> ( name, mvar ))
                    )



-- FINALIZE OBJECTS


type Objects
    = Objects T.CASTO_GlobalGraph (Dict String T.CEMN_Raw T.CASTO_LocalGraph)


finalizeObjects : LoadingObjects -> Task Objects
finalizeObjects (LoadingObjects mvar mvars) =
    Task.eio identity
        (Utils.readMVar_Maybe_CASTO_GlobalGraph mvar
            |> IO.bind
                (\result ->
                    Utils.mapTraverse identity compare Utils.readMVar_Maybe_CASTO_LocalGraph mvars
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


objectsToGlobalGraph : Objects -> T.CASTO_GlobalGraph
objectsToGlobalGraph (Objects globals locals) =
    Dict.foldr compare (\_ -> Opt.addLocalGraph) globals locals



-- LOAD TYPES


loadTypes : T.FilePath -> Dict (List String) T.CEMN_Canonical I.DependencyInterface -> List Build.Module -> Task Extract.Types
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


loadTypesHelp : T.FilePath -> Build.Module -> IO (T.MVar (Maybe Extract.Types))
loadTypesHelp root modul =
    case modul of
        Build.Fresh name iface _ ->
            Utils.newMVar (Utils.maybeEncoder Extract.typesEncoder) (Just (Extract.fromInterface name iface))

        Build.Cached name _ ciMVar ->
            Utils.readMVar Build.cachedInterfaceDecoder ciMVar
                |> IO.bind
                    (\cachedInterface ->
                        case cachedInterface of
                            Build.BB_Unneeded ->
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

                            Build.BB_Loaded iface ->
                                Utils.newMVar (Utils.maybeEncoder Extract.typesEncoder) (Just (Extract.fromInterface name iface))

                            Build.BB_Corrupted ->
                                Utils.newMVar (Utils.maybeEncoder Extract.typesEncoder) Nothing
                    )
