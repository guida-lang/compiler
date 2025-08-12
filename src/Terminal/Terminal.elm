module Terminal.Terminal exposing
    ( app
    , flag
    , flags
    , more
    , noArgs
    , noFlags
    , onOff
    , oneOf
    , require0
    , require1
    , require2
    , require3
    , zeroOrMore
    )

import Compiler.Elm.Version as V
import Compiler.Reporting.Doc as D
import List.Extra as List
import System.Exit as Exit
import System.IO as IO
import Task exposing (Task)
import Terminal.Terminal.Error as Error
import Terminal.Terminal.Internal exposing (Args(..), Command(..), CompleteArgs(..), Flag(..), Flags(..), Parser, RequiredArgs(..), toName)
import Utils.Main as Utils
import Utils.Task.Extra as Task



-- APP


app : D.Doc -> D.Doc -> List Command -> Task Never ()
app intro outro commands =
    Utils.envGetArgs
        |> Task.bind
            (\argStrings ->
                case argStrings of
                    [] ->
                        Error.exitWithOverview intro outro commands

                    [ "--help" ] ->
                        Error.exitWithOverview intro outro commands

                    [ "--version" ] ->
                        IO.hPutStrLn IO.stdout (V.toChars V.compiler)
                            |> Task.bind (\_ -> Exit.exitSuccess)

                    command :: chunks ->
                        case List.find (\cmd -> toName cmd == command) commands of
                            Nothing ->
                                Error.exitWithUnknown command (List.map toName commands)

                            Just (Command _ _ details example args_ flags_ callback) ->
                                if List.member "--help" chunks then
                                    Error.exitWithHelp (Just command) details example args_ flags_

                                else
                                    case callback chunks of
                                        Ok res ->
                                            res

                                        Err err ->
                                            Error.exitWithError err
            )



-- FLAGS


{-| -}
noFlags : Flags
noFlags =
    FDone


{-| -}
flags : Flags
flags =
    FDone


{-| -}
more : Flag -> Flags -> Flags
more f fs =
    FMore fs f



-- FLAG


{-| -}
flag : String -> Parser -> String -> Flag
flag =
    Flag


{-| -}
onOff : String -> String -> Flag
onOff =
    OnOff



-- FANCY ARGS


{-| -}
args : RequiredArgs
args =
    Done


exactly : RequiredArgs -> Args
exactly requiredArgs =
    Args [ Exactly requiredArgs ]


exclamantionMark : RequiredArgs -> Parser -> RequiredArgs
exclamantionMark =
    Required



-- questionMark : RequiredArgs -> Parser -> Args
-- questionMark requiredArgs optionalArg =
--     Args [ Optional requiredArgs optionalArg ]


dotdotdot : RequiredArgs -> Parser -> Args
dotdotdot requiredArgs repeatedArg =
    Args [ Multiple requiredArgs repeatedArg ]


oneOf : List Args -> Args
oneOf listOfArgs =
    Args (List.concatMap (\(Args a) -> a) listOfArgs)



-- -- SIMPLE ARGS


noArgs : Args
noArgs =
    exactly args



-- required : Parser -> Args
-- required parser =
--     require1 identity parser
-- optional : Parser -> Args
-- optional parser =
--     questionMark args parser


zeroOrMore : Parser -> Args
zeroOrMore parser =
    dotdotdot args parser



-- oneOrMore : Parser -> Args
-- oneOrMore parser =
--     exclamantionMark args (dotdotdot parser parser)


require0 : Args
require0 =
    exactly args


require1 : Parser -> Args
require1 a =
    exactly (exclamantionMark args a)


require2 : Parser -> Parser -> Args
require2 a b =
    exactly (exclamantionMark (exclamantionMark args a) b)


require3 : Parser -> Parser -> Parser -> Args
require3 a b c =
    exactly (exclamantionMark (exclamantionMark (exclamantionMark args a) b) c)



-- require4 : (a -> b -> c -> d -> args) -> Parser a -> Parser b -> Parser c -> Parser d -> Args args
-- require4 func a b c d =
--     exactly (exclamantionMark (exclamantionMark (exclamantionMark (exclamantionMark (args func) a) b) c) d)
-- require5 : (a -> b -> c -> d -> e -> args) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Args args
-- require5 func a b c d e =
--     exactly (exclamantionMark (exclamantionMark (exclamantionMark (exclamantionMark (exclamantionMark (args func) a) b) c) d) e)
