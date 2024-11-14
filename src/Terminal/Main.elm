port module Terminal.Main exposing (main)

import Array exposing (Array)
import Array.Extra as Array
import Compiler.Elm.Version as V
import Compiler.Reporting.Doc as D
import Data.IO as IO exposing (IO(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Terminal.Bump as Bump
import Terminal.Diff as Diff
import Terminal.Init as Init
import Terminal.Install as Install
import Terminal.Make as Make
import Terminal.Repl as Repl
import Terminal.Terminal as Terminal
import Terminal.Terminal.Chomp as Chomp
import Terminal.Terminal.Helpers as Terminal
import Terminal.Terminal.Internal as Terminal



-- PROGRAM


type alias Model =
    Array ProcessStatus


type ProcessStatus
    = Running IO.Process
    | Finished


type Msg
    = Msg Int Encode.Value


addFork : Maybe (IO ()) -> ( Model, Cmd Msg ) -> Decode.Decoder ( Model, Cmd Msg )
addFork maybeFork ( model, cmd ) =
    case Maybe.map startFork maybeFork of
        Just decoder ->
            Decode.map
                (\( process, effect, _ ) ->
                    let
                        nextIndex : Int
                        nextIndex =
                            Array.length model
                    in
                    ( Array.insertAt nextIndex (Running process) model
                    , Cmd.batch [ effectToCmd nextIndex effect, cmd ]
                    )
                )
                decoder

        Nothing ->
            Decode.succeed ( model, cmd )


port send : { index : Int, value : Encode.Value } -> Cmd msg


port recv : ({ index : Int, value : Encode.Value } -> msg) -> Sub msg


start : IO () -> Decode.Decoder ( IO.Process, IO.Effect, Maybe (IO ()) )
start (IO io) =
    io
        (\_ ->
            Decode.succeed
                ( IO.Process (Decode.fail "Exit")
                , IO.Exit "finished successfully..." 0
                , Nothing
                )
        )


startFork : IO () -> Decode.Decoder ( IO.Process, IO.Effect, Maybe (IO ()) )
startFork (IO io) =
    io
        (\_ ->
            Decode.succeed
                ( IO.Process (Decode.fail "exitFork")
                , IO.NoOp
                , Nothing
                )
        )


effectToCmd : Int -> IO.Effect -> Cmd Msg
effectToCmd index effect =
    case effect of
        IO.Exit errorMessage status ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "exit" )
                        , ( "args", Encode.list identity [ Encode.string errorMessage, Encode.int status ] )
                        ]
                }

        IO.NewIORef value ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "newIORef" )
                        , ( "args", Encode.list identity [ value ] )
                        ]
                }

        IO.ReadIORef id ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "readIORef" )
                        , ( "args", Encode.list Encode.int [ id ] )
                        ]
                }

        IO.VectorUnsafeLast array ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "vectorUnsafeLast" )
                        , ( "args", Encode.list identity [ array ] )
                        ]
                }

        IO.MVectorRead i array ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "mVectorRead" )
                        , ( "args", Encode.list identity [ Encode.int i, array ] )
                        ]
                }

        IO.WriteIORef id value ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "writeIORef" )
                        , ( "args"
                          , Encode.list identity
                                [ Encode.int id
                                , value
                                ]
                          )
                        ]
                }

        IO.GetLine ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "getLine" )
                        , ( "args", Encode.list identity [] )
                        ]
                }

        IO.HPutStr (IO.Handle fd) content ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "fwrite" )
                        , ( "args"
                          , Encode.list identity
                                [ Encode.int fd
                                , Encode.string content
                                ]
                          )
                        ]
                }

        IO.Write path value ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "write" )
                        , ( "args", Encode.list identity [ Encode.string path, value ] )
                        ]
                }

        IO.WriteString path str ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "writeString" )
                        , ( "args", Encode.list Encode.string [ path, str ] )
                        ]
                }

        IO.PutStrLn str ->
            effectToCmd index (IO.HPutStr IO.stdout (str ++ "\n"))

        IO.DirDoesFileExist filename ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirDoesFileExist" )
                        , ( "args", Encode.list Encode.string [ filename ] )
                        ]
                }

        IO.DirDoesDirectoryExist path ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirDoesDirectoryExist" )
                        , ( "args", Encode.list Encode.string [ path ] )
                        ]
                }

        IO.EnvLookupEnv varname ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "envLookupEnv" )
                        , ( "args", Encode.list Encode.string [ varname ] )
                        ]
                }

        IO.EnvGetProgName ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "envGetProgName" )
                        , ( "args", Encode.list identity [] )
                        ]
                }

        IO.EnvGetArgs ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "envGetArgs" )
                        , ( "args", Encode.list identity [] )
                        ]
                }

        IO.BinaryDecodeFileOrFail filename ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "binaryDecodeFileOrFail" )
                        , ( "args", Encode.list Encode.string [ filename ] )
                        ]
                }

        IO.DirCreateDirectoryIfMissing createParents filename ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirCreateDirectoryIfMissing" )
                        , ( "args"
                          , Encode.list identity
                                [ Encode.bool createParents
                                , Encode.string filename
                                ]
                          )
                        ]
                }

        IO.DirRemoveFile path ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirRemoveFile" )
                        , ( "args", Encode.list Encode.string [ path ] )
                        ]
                }

        IO.DirRemoveDirectoryRecursive path ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirRemoveDirectoryRecursive" )
                        , ( "args", Encode.list Encode.string [ path ] )
                        ]
                }

        IO.Read path ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "fread" )
                        , ( "args", Encode.list Encode.string [ path ] )
                        ]
                }

        IO.HttpFetch method url headers ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "httpFetch" )
                        , ( "args"
                          , Encode.list identity
                                [ Encode.string method
                                , Encode.string url
                                , Encode.object (List.map (Tuple.mapSecond Encode.string) headers)
                                ]
                          )
                        ]
                }

        IO.HttpUpload url headers parts ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "httpUpload" )
                        , ( "args"
                          , Encode.list identity
                                [ Encode.string url
                                , Encode.object (List.map (Tuple.mapSecond Encode.string) headers)
                                , parts
                                ]
                          )
                        ]
                }

        IO.DirGetAppUserDataDirectory app ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirGetAppUserDataDirectory" )
                        , ( "args", Encode.list Encode.string [ app ] )
                        ]
                }

        IO.DirGetCurrentDirectory ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirGetCurrentDirectory" )
                        , ( "args", Encode.list identity [] )
                        ]
                }

        IO.DirGetModificationTime filename ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirGetModificationTime" )
                        , ( "args", Encode.list Encode.string [ filename ] )
                        ]
                }

        IO.DirCanonicalizePath path ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirCanonicalizePath" )
                        , ( "args", Encode.list Encode.string [ path ] )
                        ]
                }

        IO.DirWithCurrentDirectory path ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirWithCurrentDirectory" )
                        , ( "args", Encode.list Encode.string [ path ] )
                        ]
                }

        IO.GetArchive method url ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "getArchive" )
                        , ( "args", Encode.list Encode.string [ method, url ] )
                        ]
                }

        IO.LockFile path ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "lockFile" )
                        , ( "args", Encode.list Encode.string [ path ] )
                        ]
                }

        IO.UnlockFile path ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "unlockFile" )
                        , ( "args", Encode.list Encode.string [ path ] )
                        ]
                }

        IO.NewEmptyMVar ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "newEmptyMVar" )
                        , ( "args", Encode.list identity [] )
                        ]
                }

        IO.ReadMVar id ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "readMVar" )
                        , ( "args", Encode.list Encode.int [ id ] )
                        ]
                }

        IO.TakeMVar id ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "takeMVar" )
                        , ( "args", Encode.list Encode.int [ id ] )
                        ]
                }

        IO.PutMVar id value ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "putMVar" )
                        , ( "args"
                          , Encode.list identity
                                [ Encode.int id
                                , value
                                ]
                          )
                        ]
                }

        IO.ReplGetInputLine prompt ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "replGetInputLine" )
                        , ( "args", Encode.list Encode.string [ prompt ] )
                        ]
                }

        IO.ReplGetInputLineWithInitial prompt ( left, right ) ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "replGetInputLineWithInitial" )
                        , ( "args", Encode.list Encode.string [ prompt, left, right ] )
                        ]
                }

        IO.HClose (IO.Handle fd) ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "hClose" )
                        , ( "args", Encode.list Encode.int [ fd ] )
                        ]
                }

        IO.HFileSize (IO.Handle fd) ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "hFileSize" )
                        , ( "args", Encode.list Encode.int [ fd ] )
                        ]
                }

        IO.HFlush (IO.Handle fd) ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "hFlush" )
                        , ( "args", Encode.list Encode.int [ fd ] )
                        ]
                }

        IO.WithFile filename mode ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "withFile" )
                        , ( "args"
                          , Encode.list Encode.string
                                [ filename
                                , case mode of
                                    IO.ReadMode ->
                                        "r"

                                    IO.WriteMode ->
                                        "w"

                                    IO.AppendMode ->
                                        "a"

                                    IO.ReadWriteMode ->
                                        "w+"
                                ]
                          )
                        ]
                }

        IO.DirFindExecutable name ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirFindExecutable" )
                        , ( "args", Encode.list Encode.string [ name ] )
                        ]
                }

        IO.ProcWithCreateProcess createProcess ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "procWithCreateProcess" )
                        , ( "args"
                          , Encode.list identity
                                [ Encode.object
                                    [ ( "cmdspec"
                                      , case createProcess.cmdspec of
                                            IO.RawCommand cmd args ->
                                                Encode.object
                                                    [ ( "type", Encode.string "RawCommand" )
                                                    , ( "cmd", Encode.string cmd )
                                                    , ( "args", Encode.list Encode.string args )
                                                    ]
                                      )
                                    , ( "stdin"
                                      , case createProcess.std_in of
                                            IO.Inherit ->
                                                Encode.string "inherit"

                                            IO.UseHandle (IO.Handle handle) ->
                                                Encode.int handle

                                            IO.CreatePipe ->
                                                Encode.string "pipe"

                                            IO.NoStream ->
                                                Encode.string "ignore"
                                      )
                                    , ( "stdout"
                                      , case createProcess.std_out of
                                            IO.Inherit ->
                                                Encode.string "inherit"

                                            IO.UseHandle (IO.Handle handle) ->
                                                Encode.int handle

                                            IO.CreatePipe ->
                                                Encode.string "pipe"

                                            IO.NoStream ->
                                                Encode.string "ignore"
                                      )
                                    , ( "stderr"
                                      , case createProcess.std_err of
                                            IO.Inherit ->
                                                Encode.string "inherit"

                                            IO.UseHandle (IO.Handle handle) ->
                                                Encode.int handle

                                            IO.CreatePipe ->
                                                Encode.string "pipe"

                                            IO.NoStream ->
                                                Encode.string "ignore"
                                      )
                                    ]
                                ]
                          )
                        ]
                }

        IO.ProcWaitForProcess ph ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "procWaitForProcess" )
                        , ( "args", Encode.list Encode.int [ ph ] )
                        ]
                }

        IO.StatePut value ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "statePut" )
                        , ( "args", Encode.list identity [ value ] )
                        ]
                }

        IO.StateGet ->
            send
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "stateGet" )
                        , ( "args", Encode.list identity [] )
                        ]
                }

        IO.NoOp ->
            Task.succeed Encode.null
                |> Task.perform (Msg index)


step : Encode.Value -> IO.Process -> Result Decode.Error ( IO.Process, IO.Effect, Maybe (IO ()) )
step value (IO.Process decoder) =
    Decode.decodeValue decoder value



-- MAIN


main : Program () Model Msg
main =
    Platform.worker
        { init =
            \() ->
                let
                    decoder : Decode.Decoder ( IO.Process, IO.Effect, Maybe (IO ()) )
                    decoder =
                        start main_
                in
                case Decode.decodeValue decoder Encode.null of
                    Ok ( process, effect, _ ) ->
                        ( Array.fromList [ Running process ]
                        , effectToCmd 0 effect
                        )

                    Err err ->
                        ( Array.empty, effectToCmd 0 (IO.Exit (Decode.errorToString err) 1) )
        , update =
            \msg model ->
                case msg of
                    Msg index value ->
                        case Array.get index model of
                            Just (Running process) ->
                                case step value process of
                                    Ok ( nextProcess, effect, maybeFork ) ->
                                        Decode.decodeValue
                                            (addFork maybeFork
                                                ( Array.set index (Running nextProcess) model
                                                , effectToCmd index effect
                                                )
                                            )
                                            Encode.null
                                            |> Result.withDefault ( model, Cmd.none )

                                    Err (Decode.Failure "exitFork" _) ->
                                        ( Array.set index Finished model, Cmd.none )

                                    Err err ->
                                        ( model
                                        , effectToCmd index (IO.Exit (Decode.errorToString err) 255)
                                        )

                            Just Finished ->
                                ( model
                                , effectToCmd index (IO.Exit ("Process has already finished! Index: " ++ String.fromInt index) 255)
                                )

                            Nothing ->
                                ( model
                                , effectToCmd index (IO.Exit ("Could not find process! Index: " ++ String.fromInt index) 255)
                                )
        , subscriptions = \_ -> recv (\{ index, value } -> Msg index value)
        }


main_ : IO ()
main_ =
    Terminal.app intro
        outro
        [ repl
        , init
        , make
        , install
        , bump
        , diff
        ]


intro : D.Doc
intro =
    D.vcat
        [ D.fillSep
            [ D.fromChars "Hi,"
            , D.fromChars "thank"
            , D.fromChars "you"
            , D.fromChars "for"
            , D.fromChars "trying"
            , D.fromChars "out"
            , D.green (D.fromChars "Elm")
            , D.green (D.fromChars (V.toChars V.compiler))
                |> D.a (D.fromChars ".")
            , D.fromChars "I hope you like it!"
            ]
        , D.fromChars ""
        , D.black (D.fromChars "-------------------------------------------------------------------------------")
        , D.black (D.fromChars "I highly recommend working through <https://guide.elm-lang.org> to get started.")
        , D.black (D.fromChars "It teaches many important concepts, including how to use `elm` in the terminal.")
        , D.black (D.fromChars "-------------------------------------------------------------------------------")
        ]


outro : D.Doc
outro =
    D.fillSep <|
        (List.map D.fromChars <|
            String.words <|
                "Be sure to ask on the Elm slack if you run into trouble! Folks are friendly and happy to help out. They hang out there because it is fun, so be kind to get the best results!"
        )



-- INIT


init : Terminal.Command
init =
    let
        summary : String
        summary =
            "Start an Elm project. It creates a starter elm.json file and provides a link explaining what to do from there."

        details : String
        details =
            "The `init` command helps start Elm projects:"

        example : D.Doc
        example =
            reflow
                "It will ask permission to create an elm.json file, the one thing common to all Elm projects. It also provides a link explaining what to do from there."
    in
    Terminal.Command "init" (Terminal.Common summary) details example Terminal.noArgs Terminal.noFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompExactly (Chomp.pure ())
                ]
                (Chomp.pure ()
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags Terminal.noFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Init.run args flags)



-- REPL


repl : Terminal.Command
repl =
    let
        summary : String
        summary =
            "Open up an interactive programming session. Type in Elm expressions like (2 + 2) or (String.length \"test\") and see if they equal four!"

        details : String
        details =
            "The `repl` command opens up an interactive programming session:"

        example : D.Doc
        example =
            reflow
                "Start working through <https://guide.elm-lang.org> to learn how to use this! It has a whole chapter that uses the REPL for everything, so that is probably the quickest way to get started."

        replFlags : Terminal.Flags
        replFlags =
            Terminal.flags
                |> Terminal.more (Terminal.flag "interpreter" interpreter "Path to a alternate JS interpreter, like node or nodejs.")
                |> Terminal.more (Terminal.onOff "no-colors" "Turn off the colors in the REPL. This can help if you are having trouble reading the values. Some terminals use a custom color scheme that diverges significantly from the standard ANSI colors, so another path may be to pick a more standard color scheme.")
    in
    Terminal.Command "repl" (Terminal.Common summary) details example Terminal.noArgs replFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompExactly (Chomp.pure ())
                ]
                (Chomp.pure Repl.Flags
                    |> Chomp.apply (Chomp.chompNormalFlag "interpreter" interpreter Just)
                    |> Chomp.apply (Chomp.chompOnOffFlag "no-colors")
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags replFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Repl.run args flags)


interpreter : Terminal.Parser
interpreter =
    Terminal.Parser
        { singular = "interpreter"
        , plural = "interpreters"
        , suggest = \_ -> IO.pure []
        , examples = \_ -> IO.pure [ "node", "nodejs" ]
        }



-- MAKE


make : Terminal.Command
make =
    let
        details : String
        details =
            "The `make` command compiles Elm code into JS or HTML:"

        example : D.Doc
        example =
            stack
                [ reflow "For example:"
                , D.indent 4 <| D.green (D.fromChars "elm make src/Main.elm")
                , reflow "This tries to compile an Elm file named src/Main.elm, generating an index.html file if possible."
                ]

        makeFlags : Terminal.Flags
        makeFlags =
            Terminal.flags
                |> Terminal.more (Terminal.onOff "debug" "Turn on the time-travelling debugger. It allows you to rewind and replay events. The events can be imported/exported into a file, which makes for very precise bug reports!")
                |> Terminal.more (Terminal.onOff "optimize" "Turn on optimizations to make code smaller and faster. For example, the compiler renames record fields to be as short as possible and unboxes values to reduce allocation.")
                |> Terminal.more (Terminal.flag "output" Make.output "Specify the name of the resulting JS file. For example --output=assets/elm.js to generate the JS at assets/elm.js or --output=/dev/null to generate no output at all!")
                |> Terminal.more (Terminal.flag "report" Make.reportType "You can say --report=json to get error messages as JSON. This is only really useful if you are an editor plugin. Humans should avoid it!")
                |> Terminal.more (Terminal.flag "docs" Make.docsFile "Generate a JSON file of documentation for a package. Eventually it will be possible to preview docs with `reactor` because it is quite hard to deal with these JSON files directly.")
    in
    Terminal.Command "make" Terminal.Uncommon details example (Terminal.zeroOrMore Terminal.elmFile) makeFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompMultiple (Chomp.pure identity) Terminal.elmFile Terminal.parseElmFile
                ]
                (Chomp.pure Make.Flags
                    |> Chomp.apply (Chomp.chompOnOffFlag "debug")
                    |> Chomp.apply (Chomp.chompOnOffFlag "optimize")
                    |> Chomp.apply (Chomp.chompNormalFlag "output" Make.output Make.parseOutput)
                    |> Chomp.apply (Chomp.chompNormalFlag "report" Make.reportType Make.parseReportType)
                    |> Chomp.apply (Chomp.chompNormalFlag "docs" Make.docsFile Make.parseDocsFile)
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags makeFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Make.run args flags)



-- INSTALL


install : Terminal.Command
install =
    let
        details : String
        details =
            "The `install` command fetches packages from <https://package.elm-lang.org> for use in your project:"

        example : D.Doc
        example =
            stack
                [ reflow
                    "For example, if you want to get packages for HTTP and JSON, you would say:"
                , D.indent 4 <|
                    D.green <|
                        D.vcat <|
                            [ D.fromChars "elm install elm/http"
                            , D.fromChars "elm install elm/json"
                            ]
                , reflow
                    "Notice that you must say the AUTHOR name and PROJECT name! After running those commands, you could say `import Http` or `import Json.Decode` in your code."
                , reflow
                    "What if two projects use different versions of the same package? No problem! Each project is independent, so there cannot be conflicts like that!"
                ]

        installArgs : Terminal.Args
        installArgs =
            Terminal.oneOf
                [ Terminal.require0
                , Terminal.require1 Terminal.package
                ]
    in
    Terminal.Command "install" Terminal.Uncommon details example installArgs Terminal.noFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompExactly (Chomp.pure Install.NoArgs)
                , Chomp.chompExactly
                    (Chomp.pure Install.Install
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.package Terminal.parsePackage
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                    )
                ]
                (Chomp.pure ()
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags Terminal.noFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Install.run args flags)



-- BUMP


bump : Terminal.Command
bump =
    let
        details : String
        details =
            "The `bump` command figures out the next version number based on API changes:"

        example : D.Doc
        example =
            reflow
                "Say you just published version 1.0.0, but then decided to remove a function. I will compare the published API to what you have locally, figure out that it is a MAJOR change, and bump your version number to 2.0.0. I do this with all packages, so there cannot be MAJOR changes hiding in PATCH releases in Elm!"
    in
    Terminal.Command "bump" Terminal.Uncommon details example Terminal.noArgs Terminal.noFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompExactly (Chomp.pure ())
                ]
                (Chomp.pure ()
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags Terminal.noFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Bump.run args flags)



-- DIFF


diff : Terminal.Command
diff =
    let
        details : String
        details =
            "The `diff` command detects API changes:"

        example : D.Doc
        example =
            stack
                [ reflow
                    "For example, to see what changed in the HTML package between versions 1.0.0 and 2.0.0, you can say:"
                , D.indent 4 <| D.green <| D.fromChars "elm diff elm/html 1.0.0 2.0.0"
                , reflow
                    "Sometimes a MAJOR change is not actually very big, so this can help you plan your upgrade timelines."
                ]

        diffArgs : Terminal.Args
        diffArgs =
            Terminal.oneOf
                [ Terminal.require0
                , Terminal.require1 Terminal.version
                , Terminal.require2 Terminal.version Terminal.version
                , Terminal.require3 Terminal.package Terminal.version Terminal.version
                ]
    in
    Terminal.Command "diff" Terminal.Uncommon details example diffArgs Terminal.noFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompExactly (Chomp.pure Diff.CodeVsLatest)
                , Chomp.chompExactly
                    (Chomp.pure Diff.CodeVsExactly
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.version Terminal.parseVersion
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                    )
                , Chomp.chompExactly
                    (Chomp.pure Diff.LocalInquiry
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.version Terminal.parseVersion
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.version Terminal.parseVersion
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                    )
                , Chomp.chompExactly
                    (Chomp.pure Diff.GlobalInquiry
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.package Terminal.parsePackage
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.version Terminal.parseVersion
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.version Terminal.parseVersion
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                    )
                ]
                (Chomp.pure ()
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags Terminal.noFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Diff.run args flags)



-- HELPERS


stack : List D.Doc -> D.Doc
stack docs =
    D.vcat <| List.intersperse (D.fromChars "") docs


reflow : String -> D.Doc
reflow string =
    D.fillSep <| List.map D.fromChars <| String.words string
