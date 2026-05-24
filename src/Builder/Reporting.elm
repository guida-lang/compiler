module Builder.Reporting exposing
    ( BKey
    , BMsg(..)
    , DKey
    , DMsg(..)
    , Key
    , Style
    , ask
    , attempt
    , attemptWithStyle
    , ignorer
    , json
    , report
    , reportGenerate
    , silent
    , terminal
    , trackBuild
    , trackDetails
    )

import Builder.Reporting.Exit as Exit
import Builder.Reporting.Exit.Help as Help
import Compiler.Data.NonEmptyList as NE
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Guida.Package as Pkg
import Compiler.Guida.Version as V
import Compiler.Json.Encode as Encode
import Compiler.Reporting.Doc as D
import System.Exit as Exit
import System.IO as IO
import Task exposing (Task)
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Main as Utils exposing (Chan, MVar)



-- STYLE


type Style
    = Silent
    | Json
    | Terminal (MVar ())


silent : Style
silent =
    Silent


json : Style
json =
    Json


terminal : Task Never Style
terminal =
    Task.map Terminal (Utils.newMVar (\_ -> BE.bool True) ())



-- ATTEMPT


attempt : (x -> Help.Report) -> Task Never (Result x a) -> Task Never a
attempt toReport work =
    work
        -- |> IO.catch reportExceptionsNicely
        |> Task.andThen
            (\result ->
                case result of
                    Ok a ->
                        Task.succeed a

                    Err x ->
                        Exit.toStderr (toReport x)
                            |> Task.andThen (\_ -> Exit.exitFailure)
            )


attemptWithStyle : Style -> (x -> Help.Report) -> Task Never (Result x a) -> Task Never a
attemptWithStyle style toReport work =
    work
        -- |> IO.catch reportExceptionsNicely
        |> Task.andThen
            (\result ->
                case result of
                    Ok a ->
                        Task.succeed a

                    Err x ->
                        case style of
                            Silent ->
                                Exit.exitFailure

                            Json ->
                                Utils.builderHPutBuilder IO.stderr (Encode.encodeUgly (Exit.toJson (toReport x)))
                                    |> Task.andThen (\_ -> Exit.exitFailure)

                            Terminal mvar ->
                                Utils.readMVar (BD.map (\_ -> ()) BD.bool) mvar
                                    |> Task.andThen (\_ -> Exit.toStderr (toReport x))
                                    |> Task.andThen (\_ -> Exit.exitFailure)
            )



-- MARKS


goodMark : D.Doc
goodMark =
    D.green
        (if isWindows then
            D.fromChars "+"

         else
            D.fromChars "●"
        )


badMark : D.Doc
badMark =
    D.red
        (if isWindows then
            D.fromChars "X"

         else
            D.fromChars "✗"
        )


isWindows : Bool
isWindows =
    -- TODO Info.os == "mingw32"
    False



-- KEY


type Key msg
    = Key (msg -> Task Never ())


report : Key msg -> msg -> Task Never ()
report (Key send) msg =
    send msg


ignorer : Key msg
ignorer =
    Key (\_ -> Task.succeed ())



-- ASK


ask : D.Doc -> Task Never Bool
ask doc =
    Help.toStdout doc
        |> Task.andThen (\_ -> askHelp)


askHelp : Task Never Bool
askHelp =
    IO.hFlush IO.stdout
        |> Task.andThen (\_ -> IO.getLine)
        |> Task.andThen
            (\input ->
                case input of
                    "" ->
                        Task.succeed True

                    "Y" ->
                        Task.succeed True

                    "y" ->
                        Task.succeed True

                    "n" ->
                        Task.succeed False

                    _ ->
                        IO.putStr "Must type 'y' for yes or 'n' for no: "
                            |> Task.andThen (\_ -> askHelp)
            )



-- DETAILS


type alias DKey =
    Key DMsg


trackDetails : Style -> (DKey -> Task Never a) -> Task Never a
trackDetails style callback =
    case style of
        Silent ->
            callback (Key (\_ -> Task.succeed ()))

        Json ->
            callback (Key (\_ -> Task.succeed ()))

        Terminal mvar ->
            Utils.newChan Utils.mVarEncoder
                |> Task.andThen
                    (\chan ->
                        Utils.forkIO
                            (Utils.takeMVar (BD.succeed ()) mvar
                                |> Task.andThen (\_ -> detailsLoop chan (DState 0 0 0 0 0 0 0))
                                |> Task.andThen (\_ -> Utils.putMVar (\_ -> BE.bool True) mvar ())
                            )
                            |> Task.andThen
                                (\_ ->
                                    let
                                        encoder : Maybe DMsg -> BE.Encoder
                                        encoder =
                                            BE.maybe dMsgEncoder
                                    in
                                    callback (Key (Utils.writeChan encoder chan << Just))
                                        |> Task.andThen
                                            (\answer ->
                                                Utils.writeChan encoder chan Nothing
                                                    |> Task.map (\_ -> answer)
                                            )
                                )
                    )


detailsLoop : Chan (Maybe DMsg) -> DState -> Task Never ()
detailsLoop chan ((DState total _ _ _ _ built _) as state) =
    Utils.readChan (BD.maybe dMsgDecoder) chan
        |> Task.andThen
            (\msg ->
                case msg of
                    Just dmsg ->
                        Task.andThen (detailsLoop chan) (detailsStep dmsg state)

                    Nothing ->
                        IO.putStrLn
                            (clear (toBuildProgress total total)
                                (if built == total then
                                    "Dependencies ready!"

                                 else
                                    "Dependency problem!"
                                )
                            )
            )


type DState
    = DState Int Int Int Int Int Int Int


type DMsg
    = DStart Int
    | DCached
    | DRequested
    | DReceived Pkg.Name V.Version
    | DFailed Pkg.Name V.Version
    | DBuilt
    | DBroken


detailsStep : DMsg -> DState -> Task Never DState
detailsStep msg (DState total cached rqst rcvd failed built broken) =
    case msg of
        DStart numDependencies ->
            Task.succeed (DState numDependencies 0 0 0 0 0 0)

        DCached ->
            putTransition (DState total (cached + 1) rqst rcvd failed built broken)

        DRequested ->
            (if rqst == 0 then
                IO.putStrLn "Starting downloads...\n"

             else
                Task.succeed ()
            )
                |> Task.map (\_ -> DState total cached (rqst + 1) rcvd failed built broken)

        DReceived pkg vsn ->
            putDownload goodMark pkg vsn
                |> Task.andThen (\_ -> putTransition (DState total cached rqst (rcvd + 1) failed built broken))

        DFailed pkg vsn ->
            putDownload badMark pkg vsn
                |> Task.andThen (\_ -> putTransition (DState total cached rqst rcvd (failed + 1) built broken))

        DBuilt ->
            putBuilt (DState total cached rqst rcvd failed (built + 1) broken)

        DBroken ->
            putBuilt (DState total cached rqst rcvd failed built (broken + 1))


putDownload : D.Doc -> Pkg.Name -> V.Version -> Task Never ()
putDownload mark pkg vsn =
    Help.toStdout
        (D.indent 2
            (mark
                |> D.plus (D.fromPackage pkg)
                |> D.plus (D.fromVersion vsn)
                |> D.a (D.fromChars "\n")
            )
        )


putTransition : DState -> Task Never DState
putTransition ((DState total cached _ rcvd failed built broken) as state) =
    if cached + rcvd + failed < total then
        Task.succeed state

    else
        let
            char : Char
            char =
                if rcvd + failed == 0 then
                    '\u{000D}'

                else
                    '\n'
        in
        putStrFlush (String.cons char (toBuildProgress (built + broken + failed) total))
            |> Task.map (\_ -> state)


putBuilt : DState -> Task Never DState
putBuilt ((DState total cached _ rcvd failed built broken) as state) =
    (if total == cached + rcvd + failed then
        putStrFlush (String.cons '\u{000D}' (toBuildProgress (built + broken + failed) total))

     else
        Task.succeed ()
    )
        |> Task.map (\_ -> state)


toBuildProgress : Int -> Int -> String
toBuildProgress built total =
    "Verifying dependencies (" ++ String.fromInt built ++ "/" ++ String.fromInt total ++ ")"


clear : String -> String -> String
clear before after =
    String.cons '\u{000D}'
        (String.repeat (String.length before) " "
            ++ String.cons '\u{000D}' after
        )



-- BUILD


type alias BKey =
    Key BMsg


type alias BResult a =
    Result Exit.BuildProblem a


trackBuild : BD.Decoder a -> (a -> BE.Encoder) -> Style -> (a -> ( Int, Bool, Bool )) -> (BKey -> Task Never (BResult a)) -> Task Never (BResult a)
trackBuild decoder encoder style extractWarningInfo callback =
    case style of
        Silent ->
            callback (Key (\_ -> Task.succeed ()))

        Json ->
            callback (Key (\_ -> Task.succeed ()))

        Terminal mvar ->
            Utils.newChan Utils.mVarEncoder
                |> Task.andThen
                    (\chan ->
                        let
                            chanEncoder : Result BMsg (BResult a) -> BE.Encoder
                            chanEncoder =
                                BE.result bMsgEncoder (bResultEncoder encoder)
                        in
                        Utils.forkIO
                            (Utils.takeMVar (BD.succeed ()) mvar
                                |> Task.andThen (\_ -> putStrFlush "Compiling ...")
                                |> Task.andThen (\_ -> buildLoop decoder chan 0 extractWarningInfo)
                                |> Task.andThen (\_ -> Utils.putMVar (\_ -> BE.bool True) mvar ())
                            )
                            |> Task.andThen (\_ -> callback (Key (Utils.writeChan chanEncoder chan << Err)))
                            |> Task.andThen
                                (\result ->
                                    Utils.writeChan chanEncoder chan (Ok result)
                                        |> Task.map (\_ -> result)
                                )
                    )


type BMsg
    = BDone


buildLoop : BD.Decoder a -> Chan (Result BMsg (BResult a)) -> Int -> (a -> ( Int, Bool, Bool )) -> Task Never ()
buildLoop decoder chan done extractWarningInfo =
    Utils.readChan (BD.result bMsgDecoder (bResultDecoder decoder)) chan
        |> Task.andThen
            (\msg ->
                case msg of
                    Err BDone ->
                        let
                            done1 : Int
                            done1 =
                                done + 1
                        in
                        putStrFlush ("\u{000D}Compiling (" ++ String.fromInt done1 ++ ")")
                            |> Task.andThen (\_ -> buildLoop decoder chan done1 extractWarningInfo)

                    Ok result ->
                        let
                            message : String
                            message =
                                toFinalMessage done extractWarningInfo result

                            width : Int
                            width =
                                12 + String.length (String.fromInt done)
                        in
                        IO.putStrLn
                            (if String.length message < width then
                                String.cons '\u{000D}' (String.repeat width " ")
                                    ++ String.cons '\u{000D}' message

                             else
                                String.cons '\u{000D}' message
                            )
            )


toFinalMessage : Int -> (a -> ( Int, Bool, Bool )) -> BResult a -> String
toFinalMessage done extractWarningInfo result =
    case result of
        Ok value ->
            let
                ( warningCount, suppressWarnings, denyWarnings ) =
                    extractWarningInfo value
            in
            if denyWarnings && warningCount > 0 then
                let
                    warningWord : String
                    warningWord =
                        if warningCount == 1 then
                            "warning"

                        else
                            "warnings"
                in
                "Failed! (" ++ String.fromInt warningCount ++ " " ++ warningWord ++ ")"

            else
                let
                    warningNote : String
                    warningNote =
                        if suppressWarnings || warningCount == 0 then
                            ""

                        else if warningCount == 1 then
                            " (1 warning)"

                        else
                            " (" ++ String.fromInt warningCount ++ " warnings)"
                in
                case done of
                    0 ->
                        "Success!" ++ warningNote

                    1 ->
                        "Success! Compiled 1 module." ++ warningNote

                    n ->
                        "Success! Compiled " ++ String.fromInt n ++ " modules." ++ warningNote

        Err problem ->
            case problem of
                Exit.BuildBadModules _ _ [] ->
                    "Detected problems in 1 module."

                Exit.BuildBadModules _ _ ps ->
                    "Detected problems in " ++ String.fromInt (1 + List.length ps) ++ " modules."

                Exit.BuildProjectProblem _ ->
                    "Detected a problem."



-- GENERATE


reportGenerate : Style -> NE.Nonempty ModuleName.Raw -> String -> Task Never ()
reportGenerate style names output =
    case style of
        Silent ->
            Task.succeed ()

        Json ->
            Task.succeed ()

        Terminal mvar ->
            Utils.readMVar (BD.map (\_ -> ()) BD.bool) mvar
                |> Task.andThen
                    (\_ ->
                        let
                            cnames : NE.Nonempty String
                            cnames =
                                NE.map (ModuleName.toChars >> String.fromList) names
                        in
                        IO.putStrLn (String.cons '\n' (toGenDiagram cnames output))
                    )


toGenDiagram : NE.Nonempty String -> String -> String
toGenDiagram (NE.Nonempty name names) output =
    let
        width : Int
        width =
            3 + List.foldr (max << String.length) (String.length name) names
    in
    case names of
        [] ->
            toGenLine width name (String.cons '>' (String.cons ' ' output ++ "\n"))

        _ :: _ ->
            Utils.unlines
                (toGenLine width name (String.cons vtop (String.cons hbar (String.cons hbar (String.cons '>' (String.cons ' ' output)))))
                    :: List.reverse (List.map2 (toGenLine width) (List.reverse names) (String.fromChar vbottom :: List.repeat (List.length names - 1) (String.fromChar vmiddle)))
                )


toGenLine : Int -> String -> String -> String
toGenLine width name end =
    "    "
        ++ name
        ++ String.cons ' ' (String.repeat (width - String.length name) (String.fromChar hbar))
        ++ end


hbar : Char
hbar =
    if isWindows then
        '-'

    else
        '─'


vtop : Char
vtop =
    if isWindows then
        '+'

    else
        '┬'


vmiddle : Char
vmiddle =
    if isWindows then
        '+'

    else
        '┤'


vbottom : Char
vbottom =
    if isWindows then
        '+'

    else
        '┘'



--


putStrFlush : String -> Task Never ()
putStrFlush str =
    IO.hPutStr IO.stdout str
        |> Task.andThen (\_ -> IO.hFlush IO.stdout)



-- ENCODERS and DECODERS


dMsgEncoder : DMsg -> BE.Encoder
dMsgEncoder dMsg =
    case dMsg of
        DStart numDependencies ->
            BE.sequence
                [ BE.unsignedInt8 0
                , BE.int numDependencies
                ]

        DCached ->
            BE.unsignedInt8 1

        DRequested ->
            BE.unsignedInt8 2

        DReceived pkg vsn ->
            BE.sequence
                [ BE.unsignedInt8 3
                , Pkg.nameEncoder pkg
                , V.versionEncoder vsn
                ]

        DFailed pkg vsn ->
            BE.sequence
                [ BE.unsignedInt8 4
                , Pkg.nameEncoder pkg
                , V.versionEncoder vsn
                ]

        DBuilt ->
            BE.unsignedInt8 5

        DBroken ->
            BE.unsignedInt8 6


dMsgDecoder : BD.Decoder DMsg
dMsgDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map DStart BD.int

                    1 ->
                        BD.succeed DCached

                    2 ->
                        BD.succeed DRequested

                    3 ->
                        BD.map2 DReceived
                            Pkg.nameDecoder
                            V.versionDecoder

                    4 ->
                        BD.map2 DFailed
                            Pkg.nameDecoder
                            V.versionDecoder

                    5 ->
                        BD.succeed DBuilt

                    6 ->
                        BD.succeed DBroken

                    _ ->
                        BD.fail
            )


bMsgEncoder : BMsg -> BE.Encoder
bMsgEncoder _ =
    BE.unsignedInt8 0


bMsgDecoder : BD.Decoder BMsg
bMsgDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed BDone

                    _ ->
                        BD.fail
            )


bResultEncoder : (a -> BE.Encoder) -> BResult a -> BE.Encoder
bResultEncoder encoder bResult =
    BE.result Exit.buildProblemEncoder encoder bResult


bResultDecoder : BD.Decoder a -> BD.Decoder (BResult a)
bResultDecoder decoder =
    BD.result Exit.buildProblemDecoder decoder
