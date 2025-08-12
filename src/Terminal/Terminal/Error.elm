module Terminal.Terminal.Error exposing
    ( exitWithError
    , exitWithHelp
    , exitWithOverview
    , exitWithUnknown
    )

import Compiler.Reporting.Suggest as Suggest
import List.Extra as List
import Prelude
import System.Exit as Exit
import System.IO as IO
import Task exposing (Task)
import Terminal.Terminal.Internal
    exposing
        ( ArgError(..)
        , Args(..)
        , Command(..)
        , CompleteArgs(..)
        , Error(..)
        , Expectation(..)
        , Flag(..)
        , FlagError(..)
        , Flags(..)
        , Parser(..)
        , RequiredArgs(..)
        , Summary(..)
        , toName
        )
import Text.PrettyPrint.ANSI.Leijen as P
import Utils.Main as Utils
import Utils.Task.Extra as Task



-- EXIT


exitSuccess : List P.Doc -> Task Never a
exitSuccess =
    exitWith Exit.ExitSuccess


exitFailure : List P.Doc -> Task Never a
exitFailure =
    exitWith (Exit.ExitFailure 1)


exitWith : Exit.ExitCode -> List P.Doc -> Task Never a
exitWith code docs =
    IO.hIsTerminalDevice IO.stderr
        |> Task.bind
            (\isTerminal ->
                let
                    adjust : P.Doc -> P.Doc
                    adjust =
                        if isTerminal then
                            identity

                        else
                            P.plain
                in
                P.displayIO IO.stderr
                    (P.renderPretty 1
                        80
                        (adjust (P.vcat (List.concatMap (\d -> [ d, P.text "" ]) docs)))
                    )
                    |> Task.bind (\_ -> IO.hPutStrLn IO.stderr "")
                    |> Task.bind (\_ -> Exit.exitWith code)
            )


getExeName : Task Never String
getExeName =
    Task.fmap Utils.fpTakeFileName Utils.envGetProgName


stack : List P.Doc -> P.Doc
stack docs =
    P.vcat <| List.intersperse (P.text "") docs


reflow : String -> P.Doc
reflow string =
    P.fillSep <| List.map P.text <| String.words string



-- HELP


exitWithHelp : Maybe String -> String -> P.Doc -> Args -> Flags -> Task Never a
exitWithHelp maybeCommand details example (Args args) flags =
    toCommand maybeCommand
        |> Task.bind
            (\command ->
                exitSuccess <|
                    [ reflow details
                    , P.indent 4 <| P.cyan <| P.vcat <| List.map (argsToDoc command) args
                    , example
                    ]
                        ++ (case flagsToDocs flags [] of
                                [] ->
                                    []

                                (_ :: _) as docs ->
                                    [ P.text "You can customize this command with the following flags:"
                                    , P.indent 4 <| stack docs
                                    ]
                           )
            )


toCommand : Maybe String -> Task Never String
toCommand maybeCommand =
    getExeName
        |> Task.fmap
            (\exeName ->
                case maybeCommand of
                    Nothing ->
                        exeName

                    Just command ->
                        exeName ++ " " ++ command
            )


argsToDoc : String -> CompleteArgs -> P.Doc
argsToDoc command args =
    case args of
        Exactly required ->
            argsToDocHelp command required []

        Multiple required (Parser { plural }) ->
            argsToDocHelp command required [ "zero or more " ++ plural ]


argsToDocHelp : String -> RequiredArgs -> List String -> P.Doc
argsToDocHelp command args names =
    case args of
        Done ->
            P.hang 4 <|
                P.hsep <|
                    List.map P.text <|
                        (command :: List.map toToken names)

        Required others (Parser { singular }) ->
            argsToDocHelp command others (singular :: names)


toToken : String -> String
toToken string =
    "<"
        ++ String.map
            (\c ->
                if c == ' ' then
                    '-'

                else
                    c
            )
            string
        ++ ">"


flagsToDocs : Flags -> List P.Doc -> List P.Doc
flagsToDocs flags docs =
    case flags of
        FDone ->
            docs

        FMore more flag ->
            let
                flagDoc : P.Doc
                flagDoc =
                    P.vcat <|
                        case flag of
                            Flag name (Parser { singular }) description ->
                                [ P.dullcyan <| P.text <| "--" ++ name ++ "=" ++ toToken singular
                                , P.indent 4 <| reflow description
                                ]

                            OnOff name description ->
                                [ P.dullcyan <| P.text <| "--" ++ name
                                , P.indent 4 <| reflow description
                                ]
            in
            flagsToDocs more (flagDoc :: docs)



-- OVERVIEW


exitWithOverview : P.Doc -> P.Doc -> List Command -> Task Never a
exitWithOverview intro outro commands =
    getExeName
        |> Task.bind
            (\exeName ->
                exitSuccess
                    [ intro
                    , P.text "The most common commands are:"
                    , P.indent 4 <| stack <| List.filterMap (toSummary exeName) commands
                    , P.text "There are a bunch of other commands as well though. Here is a full list:"
                    , P.indent 4 <| P.dullcyan <| toCommandList exeName commands
                    , P.text "Adding the --help flag gives a bunch of additional details about each one."
                    , outro
                    ]
            )


toSummary : String -> Command -> Maybe P.Doc
toSummary exeName (Command name summary _ _ (Args args) _ _) =
    case summary of
        Uncommon ->
            Nothing

        Common summaryString ->
            Just <|
                P.vcat
                    [ P.cyan <| argsToDoc (exeName ++ " " ++ name) (Prelude.head args)
                    , P.indent 4 <| reflow summaryString
                    ]


toCommandList : String -> List Command -> P.Doc
toCommandList exeName commands =
    let
        names : List String
        names =
            List.map toName commands

        width : Int
        width =
            Utils.listMaximum compare (List.map String.length names)

        toExample : String -> P.Doc
        toExample name =
            P.text
                (exeName
                    ++ " "
                    ++ name
                    ++ String.repeat (width - String.length name) " "
                    ++ " --help"
                )
    in
    P.vcat (List.map toExample names)



-- UNKNOWN


exitWithUnknown : String -> List String -> Task Never a
exitWithUnknown unknown knowns =
    let
        nearbyKnowns : List ( Int, String )
        nearbyKnowns =
            List.takeWhile (\( r, _ ) -> r <= 3) (Suggest.rank unknown identity knowns)

        suggestions : List P.Doc
        suggestions =
            case List.map toGreen (List.map Tuple.second nearbyKnowns) of
                [] ->
                    []

                [ nearby ] ->
                    [ P.text "Try", nearby, P.text "instead?" ]

                [ a, b ] ->
                    [ P.text "Try", a, P.text "or", b, P.text "instead?" ]

                (_ :: _ :: _ :: _) as abcs ->
                    P.text "Try"
                        :: List.map (P.a (P.text ",")) (Prelude.init abcs)
                        ++ [ P.text "or", Prelude.last abcs, P.text "instead?" ]
    in
    getExeName
        |> Task.bind
            (\exeName ->
                exitFailure
                    [ P.fillSep <|
                        [ P.text "There"
                        , P.text "is"
                        , P.text "no"
                        , toRed unknown
                        , P.text "command."
                        ]
                            ++ suggestions
                    , reflow <| "Run `" ++ exeName ++ "` with no arguments to get more hints."
                    ]
            )



-- ERROR TO DOC


exitWithError : Error -> Task Never a
exitWithError err =
    Task.bind exitFailure
        (case err of
            BadFlag flagError ->
                flagErrorToDocs flagError

            BadArgs argErrors ->
                case argErrors of
                    [] ->
                        Task.pure
                            [ reflow <| "I was not expecting any arguments for this command."
                            , reflow <| "Try removing them?"
                            ]

                    [ argError ] ->
                        argErrorToDocs argError

                    _ :: _ :: _ ->
                        argErrorToDocs <| Prelude.head <| List.sortBy toArgErrorRank argErrors
        )


toArgErrorRank :
    ArgError
    -> Int -- lower is better
toArgErrorRank err =
    case err of
        ArgBad _ _ ->
            0

        ArgMissing _ ->
            1

        ArgExtras _ ->
            2


toGreen : String -> P.Doc
toGreen str =
    P.green (P.text str)


toYellow : String -> P.Doc
toYellow str =
    P.yellow (P.text str)


toRed : String -> P.Doc
toRed str =
    P.red (P.text str)



-- ARG ERROR TO DOC


argErrorToDocs : ArgError -> Task Never (List P.Doc)
argErrorToDocs argError =
    case argError of
        ArgMissing (Expectation tipe makeExamples) ->
            makeExamples
                |> Task.fmap
                    (\examples ->
                        [ P.fillSep
                            [ P.text "The"
                            , P.text "arguments"
                            , P.text "you"
                            , P.text "have"
                            , P.text "are"
                            , P.text "fine,"
                            , P.text "but"
                            , P.text "in"
                            , P.text "addition,"
                            , P.text "I"
                            , P.text "was"
                            , P.text "expecting"
                            , P.text "a"
                            , toYellow (toToken tipe)
                            , P.text "value."
                            , P.text "For"
                            , P.text "example:"
                            ]
                        , P.indent 4 <| P.green <| P.vcat <| List.map P.text examples
                        ]
                    )

        ArgBad string (Expectation tipe makeExamples) ->
            makeExamples
                |> Task.fmap
                    (\examples ->
                        [ P.text "I am having trouble with this argument:"
                        , P.indent 4 <| toRed string
                        , P.fillSep <|
                            [ P.text "It"
                            , P.text "is"
                            , P.text "supposed"
                            , P.text "to"
                            , P.text "be"
                            , P.text "a"
                            , toYellow (toToken tipe)
                            , P.text "value,"
                            , P.text "like"
                            ]
                                ++ (if List.length examples == 1 then
                                        [ P.text "this:" ]

                                    else
                                        [ P.text "one"
                                        , P.text "of"
                                        , P.text "these:"
                                        ]
                                   )
                        , P.indent 4 <| P.green <| P.vcat <| List.map P.text examples
                        ]
                    )

        ArgExtras extras ->
            let
                ( these, them ) =
                    case extras of
                        [ _ ] ->
                            ( "this argument", "it" )

                        _ ->
                            ( "these arguments", "them" )
            in
            Task.pure
                [ reflow <| "I was not expecting " ++ these ++ ":"
                , P.indent 4 <| P.red <| P.vcat <| List.map P.text extras
                , reflow <| "Try removing " ++ them ++ "?"
                ]



-- FLAG ERROR TO DOC


flagErrorHelp : String -> String -> List P.Doc -> Task Never (List P.Doc)
flagErrorHelp summary original explanation =
    Task.pure <|
        [ reflow summary
        , P.indent 4 (toRed original)
        ]
            ++ explanation


flagErrorToDocs : FlagError -> Task Never (List P.Doc)
flagErrorToDocs flagError =
    case flagError of
        FlagWithValue flagName value ->
            flagErrorHelp
                "This on/off flag was given a value:"
                ("--" ++ flagName ++ "=" ++ value)
                [ P.text "An on/off flag either exists or not. It cannot have an equals sign and value.\nMaybe you want this instead?"
                , P.indent 4 <| toGreen <| "--" ++ flagName
                ]

        FlagWithNoValue flagName (Expectation tipe makeExamples) ->
            makeExamples
                |> Task.bind
                    (\examples ->
                        flagErrorHelp
                            "This flag needs more information:"
                            ("--" ++ flagName)
                            [ P.fillSep
                                [ P.text "It"
                                , P.text "needs"
                                , P.text "a"
                                , toYellow (toToken tipe)
                                , P.text "like"
                                , P.text "this:"
                                ]
                            , P.indent 4 <|
                                P.vcat <|
                                    List.map toGreen <|
                                        case List.take 4 examples of
                                            [] ->
                                                [ "--" ++ flagName ++ "=" ++ toToken tipe ]

                                            _ :: _ ->
                                                List.map (\example -> "--" ++ flagName ++ "=" ++ example) examples
                            ]
                    )

        FlagWithBadValue flagName badValue (Expectation tipe makeExamples) ->
            makeExamples
                |> Task.bind
                    (\examples ->
                        flagErrorHelp
                            "This flag was given a bad value:"
                            ("--" ++ flagName ++ "=" ++ badValue)
                            [ P.fillSep <|
                                [ P.text "I"
                                , P.text "need"
                                , P.text "a"
                                , P.text "valid"
                                , toYellow (toToken tipe)
                                , P.text "value."
                                , P.text "For"
                                , P.text "example:"
                                ]
                            , P.indent 4 <|
                                P.vcat <|
                                    List.map toGreen <|
                                        case List.take 4 examples of
                                            [] ->
                                                [ "--" ++ flagName ++ "=" ++ toToken tipe ]

                                            _ :: _ ->
                                                List.map (\example -> "--" ++ flagName ++ "=" ++ example) examples
                            ]
                    )

        FlagUnknown unknown flags ->
            flagErrorHelp "I do not recognize this flag:"
                unknown
                (let
                    unknownName : String
                    unknownName =
                        List.takeWhile ((/=) '=') (List.dropWhile ((==) '-') (String.toList unknown))
                            |> String.fromList
                 in
                 case getNearbyFlags unknownName flags [] of
                    [] ->
                        []

                    [ thisOne ] ->
                        [ P.fillSep
                            [ P.text "Maybe"
                            , P.text "you"
                            , P.text "want"
                            , P.green thisOne
                            , P.text "instead?"
                            ]
                        ]

                    suggestions ->
                        [ P.fillSep
                            [ P.text "Maybe"
                            , P.text "you"
                            , P.text "want"
                            , P.text "one"
                            , P.text "of"
                            , P.text "these"
                            , P.text "instead?"
                            ]
                        , P.indent 4 <| P.green <| P.vcat suggestions
                        ]
                )


getNearbyFlags : String -> Flags -> List ( Int, String ) -> List P.Doc
getNearbyFlags unknown flags unsortedFlags =
    case flags of
        FDone ->
            List.map P.text <|
                List.map Tuple.second <|
                    List.sortBy Tuple.first <|
                        case List.filter (\( d, _ ) -> d < 3) unsortedFlags of
                            [] ->
                                unsortedFlags

                            nearbyUnsortedFlags ->
                                nearbyUnsortedFlags

        FMore more flag ->
            getNearbyFlags unknown more (getNearbyFlagsHelp unknown flag :: unsortedFlags)


getNearbyFlagsHelp : String -> Flag -> ( Int, String )
getNearbyFlagsHelp unknown flag =
    case flag of
        OnOff flagName _ ->
            ( Suggest.distance unknown flagName
            , "--" ++ flagName
            )

        Flag flagName (Parser { singular }) _ ->
            ( Suggest.distance unknown flagName
            , "--" ++ flagName ++ "=" ++ toToken singular
            )
