module Builder.Reporting exposing
    ( BKey
    , BMsg(..)
    , BResult
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
import Control.Concurrent.MVar as MVar exposing (MVar)
import Process
import System.Exit as Exit
import System.IO as IO
import Task exposing (Task)
import Utils.Crash exposing (crash)
import Utils.Main as Utils exposing (Chan)
import Utils.System.IO as IO
import Utils.Task.Extra as Task



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
    Task.map Terminal (MVar.newMVar ())



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
                            |> Task.andThen (\_ -> crash "Exit.exitFailure")
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
                                crash "Exit.exitFailure"

                            Json ->
                                Utils.builderHPutBuilder IO.stderr (Encode.encodeUgly (Exit.toJson (toReport x)))
                                    |> Task.andThen (\_ -> crash "Exit.exitFailure")

                            Terminal mvar ->
                                MVar.readMVar mvar
                                    |> Task.andThen (\_ -> Exit.toStderr (toReport x))
                                    |> Task.andThen (\_ -> crash "Exit.exitFailure")
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
            Utils.newChan
                |> Task.andThen
                    (\chan ->
                        Process.spawn
                            (MVar.takeMVar mvar
                                |> Task.andThen (\_ -> detailsLoop chan (DState 0 0 0 0 0 0 0))
                                |> Task.andThen (\_ -> MVar.putMVar mvar ())
                            )
                            |> Task.andThen
                                (\_ ->
                                    callback (Key (Utils.writeChan chan << Just))
                                        |> Task.andThen
                                            (\answer ->
                                                Utils.writeChan chan Nothing
                                                    |> Task.map (\_ -> answer)
                                            )
                                )
                    )


detailsLoop : Chan (Maybe DMsg) -> DState -> Task Never ()
detailsLoop chan ((DState total _ _ _ _ built _) as state) =
    Utils.readChan chan
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


trackBuild : Style -> (BKey -> Task Never (BResult a)) -> Task Never (BResult a)
trackBuild style callback =
    case style of
        Silent ->
            callback (Key (\_ -> Task.succeed ()))

        Json ->
            callback (Key (\_ -> Task.succeed ()))

        Terminal mvar ->
            Utils.newChan
                |> Task.andThen
                    (\chan ->
                        Process.spawn
                            (MVar.takeMVar mvar
                                |> Task.andThen (\_ -> putStrFlush "Compiling ...")
                                |> Task.andThen (\_ -> buildLoop chan 0)
                                |> Task.andThen (\_ -> MVar.putMVar mvar ())
                            )
                            |> Task.andThen (\_ -> callback (Key (Utils.writeChan chan << Err)))
                            |> Task.andThen
                                (\result ->
                                    Utils.writeChan chan (Ok result)
                                        |> Task.map (\_ -> result)
                                )
                    )


type BMsg
    = BDone


buildLoop : Chan (Result BMsg (BResult a)) -> Int -> Task Never ()
buildLoop chan done =
    Utils.readChan chan
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
                            |> Task.andThen (\_ -> buildLoop chan done1)

                    Ok result ->
                        let
                            message : String
                            message =
                                toFinalMessage done result

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


toFinalMessage : Int -> BResult a -> String
toFinalMessage done result =
    case result of
        Ok _ ->
            case done of
                0 ->
                    "Success!"

                1 ->
                    "Success! Compiled 1 module."

                n ->
                    "Success! Compiled " ++ String.fromInt n ++ " modules."

        Err problem ->
            case problem of
                Exit.BuildBadModules _ _ [] ->
                    "Detected problems in 1 module."

                Exit.BuildBadModules _ _ (_ :: ps) ->
                    "Detected problems in " ++ String.fromInt (2 + List.length ps) ++ " modules."

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
            MVar.readMVar mvar
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
