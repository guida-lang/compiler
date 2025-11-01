module Compiler.Reporting.Error.Json exposing
    ( Context(..)
    , FailureToReport(..)
    , Reason(..)
    , toReport
    )

import Builder.Reporting.Exit.Help as Help
import Compiler.Data.NonEmptyList as NE
import Compiler.Json.Decode exposing (DecodeExpectation(..), Error(..), ParseError(..), Problem(..), StringProblem(..))
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Code as Code
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- TO REPORT


toReport : String -> FailureToReport x -> Error x -> Reason -> Help.Report
toReport path ftr err reason =
    case err of
        DecodeProblem bytes problem ->
            problemToReport path ftr (Code.toSource bytes) CRoot problem reason

        ParseProblem bytes parseError ->
            parseErrorToReport path (Code.toSource bytes) parseError reason


type Reason
    = ExplicitReason String


because : Reason -> String -> String
because (ExplicitReason iNeedThings) problem =
    iNeedThings ++ " " ++ problem



-- PARSE ERROR TO REPORT


parseErrorToReport : String -> Code.Source -> ParseError -> Reason -> Help.Report
parseErrorToReport path source parseError reason =
    let
        toSnippet : String -> Int -> Int -> ( String, D.Doc ) -> Help.Report
        toSnippet title row col ( problem, details ) =
            let
                pos : A.Position
                pos =
                    A.Position row col

                surroundings : A.Region
                surroundings =
                    A.Region (A.Position (max 1 (row - 2)) 1) pos

                region : A.Region
                region =
                    A.Region pos pos
            in
            Help.jsonReport title (Just path) <|
                Code.toSnippet source
                    surroundings
                    (Just region)
                    ( D.reflow (because reason problem)
                    , details
                    )
    in
    case parseError of
        Start row col ->
            toSnippet "EXPECTING A VALUE"
                row
                col
                ( "I was expecting to see a JSON value next:"
                , D.stack
                    [ D.fillSep
                        [ D.fromChars "Try"
                        , D.fromChars "something"
                        , D.fromChars "like"
                        , D.dullyellow (D.fromChars "\"this\"")
                        , D.fromChars "or"
                        , D.dullyellow (D.fromChars "42")
                        , D.fromChars "to"
                        , D.fromChars "move"
                        , D.fromChars "on"
                        , D.fromChars "to"
                        , D.fromChars "better"
                        , D.fromChars "hints!"
                        ]
                    , D.toSimpleNote <|
                        "The JSON specification does not allow trailing commas, so you can sometimes get this error in arrays that have an extra comma at the end. In that case, remove that last comma or add another array entry after it!"
                    ]
                )

        ObjectField row col ->
            toSnippet "EXTRA COMMA"
                row
                col
                ( "I was partway through parsing a JSON object when I got stuck here:"
                , D.stack
                    [ D.fillSep
                        [ D.fromChars "I"
                        , D.fromChars "saw"
                        , D.fromChars "a"
                        , D.fromChars "comma"
                        , D.fromChars "right"
                        , D.fromChars "before"
                        , D.fromChars "I"
                        , D.fromChars "got"
                        , D.fromChars "stuck"
                        , D.fromChars "here,"
                        , D.fromChars "so"
                        , D.fromChars "I"
                        , D.fromChars "was"
                        , D.fromChars "expecting"
                        , D.fromChars "to"
                        , D.fromChars "see"
                        , D.fromChars "a"
                        , D.fromChars "field"
                        , D.fromChars "name"
                        , D.fromChars "like"
                        , D.dullyellow (D.fromChars "\"type\"")
                        , D.fromChars "or"
                        , D.dullyellow (D.fromChars "\"dependencies\"")
                        , D.fromChars "next."
                        ]
                    , D.reflow <|
                        "This error is commonly caused by trailing commas in JSON objects. Those are actually disallowed by <https://json.org> so check the previous line for a trailing comma that may need to be deleted."
                    , objectNote
                    ]
                )

        ObjectColon row col ->
            toSnippet "EXPECTING COLON"
                row
                col
                ( "I was partway through parsing a JSON object when I got stuck here:"
                , D.stack
                    [ D.reflow "I was expecting to see a colon next."
                    , objectNote
                    ]
                )

        ObjectEnd row col ->
            toSnippet "UNFINISHED OBJECT"
                row
                col
                ( "I was partway through parsing a JSON object when I got stuck here:"
                , D.stack
                    [ D.reflow "I was expecting to see a comma or a closing curly brace next."
                    , D.reflow "Is a comma missing on the previous line? Is an array missing a closing square bracket? It is often something tricky like that!"
                    , objectNote
                    ]
                )

        ArrayEnd row col ->
            toSnippet "UNFINISHED ARRAY"
                row
                col
                ( "I was partway through parsing a JSON array when I got stuck here:"
                , D.stack
                    [ D.reflow "I was expecting to see a comma or a closing square bracket next."
                    , D.reflow "Is a comma missing on the previous line? It is often something like that!"
                    ]
                )

        StringProblem stringProblem row col ->
            case stringProblem of
                BadStringEnd ->
                    toSnippet "ENDLESS STRING"
                        row
                        col
                        ( "I got to the end of the line without seeing the closing double quote:2"
                        , D.fillSep
                            [ D.fromChars "Strings"
                            , D.fromChars "look"
                            , D.fromChars "like"
                            , D.green (D.fromChars "\"this\"")
                            , D.fromChars "with"
                            , D.fromChars "double"
                            , D.fromChars "quotes"
                            , D.fromChars "on"
                            , D.fromChars "each"
                            , D.fromChars "end."
                            , D.fromChars "Is"
                            , D.fromChars "the"
                            , D.fromChars "closing"
                            , D.fromChars "double"
                            , D.fromChars "quote"
                            , D.fromChars "missing"
                            , D.fromChars "in"
                            , D.fromChars "your"
                            , D.fromChars "code?"
                            ]
                        )

                BadStringControlChar ->
                    toSnippet "UNEXPECTED CONTROL CHARACTER"
                        row
                        col
                        ( "I ran into a control character unexpectedly:"
                        , D.reflow
                            "These are characters that represent tabs, backspaces, newlines, and a bunch of other invisible characters. They all come before 20 in the ASCII range, and they are disallowed by the JSON specificaiton. Maybe a copy/paste added one of these invisible characters to your JSON?"
                        )

                BadStringEscapeChar ->
                    toSnippet "UNKNOWN ESCAPE"
                        row
                        col
                        ( "Backslashes always start escaped characters, but I do not recognize this one:"
                        , D.stack
                            [ D.reflow "Valid escape characters include:"
                            , D.dullyellow <|
                                D.indent 4 <|
                                    D.vcat
                                        [ D.fromChars "\\\""
                                        , D.fromChars "\\\\"
                                        , D.fromChars "\\/"
                                        , D.fromChars "\\b"
                                        , D.fromChars "\\f"
                                        , D.fromChars "\\n"
                                        , D.fromChars "\\r"
                                        , D.fromChars "\\t"
                                        , D.fromChars "\\u003D"
                                        ]
                            , D.reflow "Do you want one of those instead? Maybe you need \\\\ to escape a backslash?"
                            ]
                        )

                BadStringEscapeHex ->
                    toSnippet "BAD HEX ESCAPE"
                        row
                        col
                        ( "This is not a valid hex escape:"
                        , D.fillSep
                            [ D.fromChars "Valid"
                            , D.fromChars "hex"
                            , D.fromChars "escapes"
                            , D.fromChars "in"
                            , D.fromChars "JSON"
                            , D.fromChars "are"
                            , D.fromChars "between"
                            , D.green (D.fromChars "\\u0000")
                            , D.fromChars "and"
                            , D.green (D.fromChars "\\uFFFF")
                            , D.fromChars "and"
                            , D.fromChars "always"
                            , D.fromChars "have"
                            , D.fromChars "exactly"
                            , D.fromChars "four"
                            , D.fromChars "digits."
                            ]
                        )

        NoLeadingZeros row col ->
            toSnippet "BAD NUMBER"
                row
                col
                ( "Numbers cannot start with zeros like this:"
                , D.reflow "Try deleting the leading zeros?"
                )

        NoFloats row col ->
            toSnippet "UNEXPECTED NUMBER"
                row
                col
                ( "I got stuck while trying to parse this number:"
                , D.reflow
                    "I do not accept floating point numbers like 3.1415 right now. That kind of JSON value is not needed for any of the uses that Guida has for now."
                )

        BadEnd row col ->
            toSnippet "JSON PROBLEM"
                row
                col
                ( "I was partway through parsing some JSON when I got stuck here:"
                , D.reflow
                    "I am not really sure what is wrong. This sometimes means there is extra stuff after a valid JSON value?"
                )


objectNote : D.Doc
objectNote =
    D.stack
        [ D.toSimpleNote "Here is an example of a valid JSON object for reference:"
        , D.vcat
            [ D.indent 4 (D.fromChars "{")
            , D.indent 6
                (D.dullyellow (D.fromChars "\"name\"")
                    |> D.a (D.fromChars ": ")
                    |> D.a (D.dullyellow (D.fromChars "\"Tom\""))
                    |> D.a (D.fromChars ",")
                )
            , D.indent 6
                (D.dullyellow (D.fromChars "\"age\"")
                    |> D.a (D.fromChars ": ")
                    |> D.a (D.dullyellow (D.fromChars "42"))
                )
            , D.indent 4 (D.fromChars "}")
            ]
        , D.reflow
            "Notice that (1) the field names are in double quotes and (2) there is no trailing comma after the last entry. Both are strict requirements in JSON!"
        ]



-- PROBLEM TO REPORT


type Context
    = CRoot
    | CField String Context
    | CIndex Int Context


problemToReport : String -> FailureToReport x -> Code.Source -> Context -> Problem x -> Reason -> Help.Report
problemToReport path (FailureToReport ftr) source context problem reason =
    case problem of
        Field field prob ->
            problemToReport path (FailureToReport ftr) source (CField field context) prob reason

        Index index prob ->
            problemToReport path (FailureToReport ftr) source (CIndex index context) prob reason

        OneOf p ps ->
            -- NOTE: only displays the deepest problem. This works well for the kind
            -- of JSON used by Guida, but probably would not work well in general.
            let
                (NE.Nonempty prob _) =
                    NE.sortBy (negate << getMaxDepth) (NE.Nonempty p ps)
            in
            problemToReport path (FailureToReport ftr) source context prob reason

        Failure region x ->
            ftr path source context region x

        Expecting region expectation ->
            expectationToReport path source context region expectation reason


getMaxDepth : Problem x -> Int
getMaxDepth problem =
    case problem of
        Field _ prob ->
            1 + getMaxDepth prob

        Index _ prob ->
            1 + getMaxDepth prob

        OneOf p ps ->
            Utils.listMaximum compare (getMaxDepth p :: List.map getMaxDepth ps)

        Failure _ _ ->
            0

        Expecting _ _ ->
            0


type FailureToReport x
    = FailureToReport (String -> Code.Source -> Context -> A.Region -> x -> Help.Report)


expectationToReport : String -> Code.Source -> Context -> A.Region -> DecodeExpectation -> Reason -> Help.Report
expectationToReport path source context (A.Region start end) expectation reason =
    let
        (A.Position sr _) =
            start

        (A.Position er _) =
            end

        region : A.Region
        region =
            if sr == er then
                crash "region"

            else
                A.Region start start

        introduction : String
        introduction =
            case context of
                CRoot ->
                    "I ran into some trouble here:"

                CField field _ ->
                    "I ran into trouble with the value of the \"" ++ field ++ "\" field:"

                CIndex index (CField field _) ->
                    "When looking at the \""
                        ++ field
                        ++ "\" field, I ran into trouble with the "
                        ++ D.intToOrdinal index
                        ++ " entry:"

                CIndex index _ ->
                    "I ran into trouble with the " ++ D.intToOrdinal index ++ " index of this array:"

        toSnippet : String -> List D.Doc -> Help.Report
        toSnippet title aThing =
            Help.jsonReport title (Just path) <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow (because reason introduction)
                    , D.fillSep <|
                        [ D.fromChars "I"
                        , D.fromChars "was"
                        , D.fromChars "expecting"
                        , D.fromChars "to"
                        , D.fromChars "run"
                        , D.fromChars "into"
                        ]
                            ++ aThing
                    )
    in
    case expectation of
        TObject ->
            toSnippet "EXPECTING OBJECT"
                [ D.fromChars "an"
                , D.green (D.fromChars "OBJECT")
                    |> D.a (D.fromChars ".")
                ]

        TArray ->
            toSnippet "EXPECTING ARRAY"
                [ D.fromChars "an"
                , D.green (D.fromChars "ARRAY")
                    |> D.a (D.fromChars ".")
                ]

        TString ->
            toSnippet "EXPECTING STRING"
                [ D.fromChars "a"
                , D.green (D.fromChars "STRING")
                    |> D.a (D.fromChars ".")
                ]

        TInt ->
            toSnippet "EXPECTING INT"
                [ D.fromChars "an"
                , D.green (D.fromChars "INT")
                    |> D.a (D.fromChars ".")
                ]

        TObjectWith field ->
            toSnippet "MISSING FIELD"
                [ D.fromChars "an"
                , D.green (D.fromChars "OBJECT")
                , D.fromChars "with"
                , D.fromChars "a"
                , D.green (D.fromChars ("\"" ++ field ++ "\""))
                , D.fromChars "field."
                ]

        TArrayPair len ->
            toSnippet "EXPECTING PAIR"
                [ D.fromChars "an"
                , D.green (D.fromChars "ARRAY")
                , D.fromChars "with"
                , D.green (D.fromChars "TWO")
                , D.fromChars "entries."
                , D.fromChars "This"
                , D.fromChars "array"
                , D.fromChars "has"
                , D.fromInt len
                , if len == 1 then
                    D.fromChars "element."

                  else
                    D.fromChars "elements."
                ]
