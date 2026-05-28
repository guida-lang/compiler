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


{-| Generate a debug JavaScript bundle string.

This loads compiled objects and type information, prepares source maps if
requested, and invokes the JavaScript generator in `Dev` mode with full
type annotations included to aid debugging.

-}
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


{-| Generate a development JavaScript bundle string.

Like `debug` but omits detailed type annotations; suitable for iterative
development where readable output is preferred over maximum optimization.

-}
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


{-| Generate a production JavaScript bundle string.

Performs additional checks (e.g. ensuring no debug-only constructs remain),
prepares source maps as requested, and invokes the JS generator in `Prod`
mode which applies optimizations and name shortening.

-}
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


{-| Prepare source maps for all modules in a project root.

When `withSourceMaps` is `True` this reads every source file listed by the
outline and converts them into `JS.SourceMaps`. When `False` it returns
`JS.NoSourceMaps`.

-}
prepareSourceMaps : Bool -> Stuff.Root -> Task Exit.Generate JS.SourceMaps
prepareSourceMaps withSourceMaps root =
    if withSourceMaps then
        Outline.getAllModulePaths root
            |> Task.andThen (Utils.mapTraverse ModuleName.toComparableCanonical ModuleName.compareCanonical File.readUtf8)
            |> Task.map JS.SourceMaps
            |> Task.io

    else
        Task.succeed JS.NoSourceMaps


{-| Generate code for the REPL.

This builds a small JS snippet suitable for running a single `name` in the
REPL environment, using in-memory module artifacts and optional ANSI
coloring.

-}
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


{-| Fail production builds if any modules still include debug-only
constructs that cannot be optimized away. This enforces that `prod`
builds are free of debug helpers.
-}
checkForDebugUses : Objects -> Task Exit.Generate ()
checkForDebugUses (Objects _ locals) =
    case Dict.keys compare (Dict.filter (\_ -> Nitpick.hasDebugUses) locals) of
        [] ->
            Task.succeed ()

        m :: ms ->
            Task.fail (Exit.GenerateCannotOptimizeDebugValues m ms)



-- GATHER MAINS


{-| Collect `main` entries for each root module.

Returns a dictionary mapping comparable canonical module names to their
corresponding `Opt.Main` values, used by the JS generator to stitch
entry points into the final bundle.

-}
gatherMains : Pkg.Name -> Objects -> NE.Nonempty Build.Root -> Dict (List String) TypeCheck.Canonical Opt.Main
gatherMains pkg (Objects _ locals) roots =
    Dict.fromList ModuleName.toComparableCanonical (List.filterMap (lookupMain pkg locals) (NE.toList roots))


{-| Lookup the `main` graph for a specific root module.

Handles both in-project (`Inside`) and explicit external (`Outside`)
roots by consulting the loaded local graphs.

-}
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


{-| Intermediate structure holding MVars for loaded global and local graphs.

Used while asynchronous object loads are in-flight so finalization can
wait for all pieces to be available.

-}
type LoadingObjects
    = LoadingObjects (MVar (Maybe Opt.GlobalGraph)) (Dict String ModuleName.Raw (MVar (Maybe Opt.LocalGraph)))


{-| Begin loading object graphs for all modules referenced by the build.

Returns a `LoadingObjects` value containing MVars that will eventually
hold the loaded global/local graphs; some MVars may already contain
values for newly compiled modules.

-}
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


{-| Load a single module's object graph into an MVar.

For `Fresh` modules the MVar is pre-filled. For `Cached` modules the
MVar is created empty and populated asynchronously by reading the
artifact from disk.

-}
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


{-| Fully resolved in-memory objects used by the generator.

Contains the global graph and a dictionary of per-module local graphs.

-}
type Objects
    = Objects Opt.GlobalGraph (Dict String ModuleName.Raw Opt.LocalGraph)


{-| Wait for all object-loading MVars and assemble the final `Objects`.

Fails with `Exit.GenerateCannotLoadArtifacts` if any required artifact is
missing or corrupted.

-}
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


{-| Merge the global graph with local graphs to produce a complete
`Opt.GlobalGraph` for code generation.
-}
objectsToGlobalGraph : Objects -> Opt.GlobalGraph
objectsToGlobalGraph (Objects globals locals) =
    Dict.foldr compare (\_ -> Opt.addLocalGraph) globals locals



-- LOAD TYPES


{-| Load type information for the modules, merging with foreign
dependency interfaces when available.

Returns the combined `Extract.Types` needed for generation and
debugging assistance.

-}
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


{-| Helper to load types for a single module, returning an MVar that
will eventually contain the `Extract.Types` or `Nothing` when
unavailable.
-}
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
