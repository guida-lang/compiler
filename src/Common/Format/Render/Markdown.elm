module Common.Format.Render.Markdown exposing (..)

import Common.Format.Cheapskate.Types exposing (..)
import Maybe.Extra as Maybe
import Url
import Utils.Main as Utils


formatMarkdown : (String -> Maybe String) -> Blocks -> String
formatMarkdown formatCode blocks =
    let
        needsInitialBlanks =
            case blocks of
                (Para inlines) :: _ ->
                    case inlines of
                        (Str a) :: (Str b) :: _ ->
                            if (a == "@") && (b == "docs") then
                                True

                            else
                                False

                        _ ->
                            False

                [] ->
                    False

                _ ->
                    True

        needsTrailingBlanks =
            case blocks of
                [] ->
                    False

                _ :: [] ->
                    needsInitialBlanks

                _ ->
                    True
    in
    formatMarkdown_ formatCode False needsInitialBlanks needsTrailingBlanks blocks


mapWithPrev : (Maybe a -> a -> b) -> List a -> List b
mapWithPrev f list =
    case list of
        [] ->
            []

        first :: rest ->
            f Nothing first :: List.map2 (\prev next -> f (Just prev) next) (first :: rest) rest


formatMarkdown_ : (String -> Maybe String) -> Bool -> Bool -> Bool -> List Block -> String
formatMarkdown_ formatCode isListItem needsInitialBlanks needsTrailingBlanks blocks =
    let
        intersperse =
            case ( isListItem, blocks ) of
                ( True, [ Para _, List _ _ _ ] ) ->
                    identity

                _ ->
                    List.intersperse "\n"

        contextFor prev =
            case prev of
                Just (List _ _ _) ->
                    AfterIndentedList

                _ ->
                    Normal
    in
    (if needsInitialBlanks then
        "\n\n"

     else
        ""
    )
        ++ (String.concat <| intersperse <| mapWithPrev (\prev -> formatMardownBlock formatCode (contextFor prev)) blocks)
        ++ (if needsTrailingBlanks then
                "\n"

            else
                ""
           )


type Context
    = Normal
    | AfterIndentedList


formatMardownBlock : (String -> Maybe String) -> Context -> Block -> String
formatMardownBlock formatCode context block =
    case block of
        ElmDocs terms ->
            (String.join "\n" <| List.map ((++) "@docs " << String.join ", ") terms) ++ "\n"

        Para inlines ->
            (String.concat <| List.map (formatMarkdownInline True) inlines) ++ "\n"

        Header level inlines ->
            "\n" ++ String.repeat level "#" ++ " " ++ (String.concat <| List.map (formatMarkdownInline True) inlines) ++ "\n"

        Blockquote blocks ->
            formatMarkdown_ formatCode False False False blocks
                |> prefix_ "> " "> "

        List tight (Bullet _) items ->
            String.concat <|
                (if tight then
                    identity

                 else
                    List.intersperse "\n"
                )
                <|
                    List.map (prefix_ "  - " "    " << formatMarkdown_ formatCode True False False) items

        List tight (Numbered _ _) items ->
            String.concat <|
                (if tight then
                    identity

                 else
                    List.intersperse "\n"
                )
                <|
                    List.map (formatListItem formatCode) <|
                        List.indexedMap Tuple.pair items

        CodeBlock (CodeAttr { codeLang }) code ->
            let
                isElm =
                    codeLang == "elm" || codeLang == ""

                formatted =
                    Maybe.withDefault (ensureNewline code) <|
                        if isElm then
                            formatCode code

                        else
                            Nothing

                ensureNewline text =
                    if String.endsWith "\n" text then
                        text

                    else
                        text ++ "\n"

                canIndent =
                    case context of
                        Normal ->
                            True

                        AfterIndentedList ->
                            False
            in
            if isElm && canIndent then
                Utils.unlines <| List.map ((++) "    ") <| lines formatted

            else
                "```" ++ codeLang ++ "\n" ++ formatted ++ "```\n"

        HtmlBlock text ->
            text ++ "\n"

        HRule ->
            "---\n"

        ReferencesBlock refs ->
            String.concat <| List.map formatRef refs


lines : String -> List String
lines str =
    case List.reverse (String.lines str) of
        "" :: rest ->
            List.reverse rest

        result ->
            List.reverse result


formatListItem : (String -> Maybe String) -> ( Int, Blocks ) -> String
formatListItem formatCode ( i, item ) =
    let
        pref =
            if i < 10 then
                String.fromInt i ++ ".  "

            else
                String.fromInt i ++ ". "
    in
    prefix_ pref "    " <| formatMarkdown_ formatCode True False False item


formatRef : ( String, String, String ) -> String
formatRef ( label, url, title ) =
    "["
        ++ label
        ++ "]: "
        ++ url
        ++ (if title == "" then
                ""

            else
                " \"" ++ title ++ "\""
           )
        ++ "\n"


prefix_ : String -> String -> String -> String
prefix_ preFirst preRest =
    Utils.unlines << prefix preFirst preRest << lines


prefix : String -> String -> List String -> List String
prefix preFirst preRest list =
    case list of
        [] ->
            []

        first :: rest ->
            (preFirst ++ first) :: List.map ((++) preRest) rest


formatMarkdownInline : Bool -> Inline -> String
formatMarkdownInline fixSpecialChars inline =
    let
        fix c =
            case c of
                '\\' ->
                    "\\\\"

                -- TODO: only at the beginning of words
                '`' ->
                    "\\`"

                '_' ->
                    "\\_"

                '*' ->
                    "\\*"

                -- TODO: {}  curly braces (when?)
                -- TODO: []  square brackets (when?)
                -- TODO: ()  parentheses (when?)
                -- TODO: #   hash mark (only at the beginning of lines, and within header lines?)
                -- TODO: -   minus sign (hyphen) (only at the beginning of lines?)
                -- TODO: +   plus sign (when?)
                -- TODO: .   dot (when?)
                -- TODO: !   exclamation mark (when?)
                _ ->
                    String.fromChar c
    in
    case inline of
        Str text ->
            (if fixSpecialChars then
                String.concat << List.map fix << String.toList

             else
                identity
            )
                text

        Space ->
            " "

        SoftBreak ->
            "\n"

        LineBreak ->
            "\n"

        Emph inlines ->
            "_" ++ (String.concat <| List.map (formatMarkdownInline True) <| inlines) ++ "_"

        -- TODO: escaping
        Strong inlines ->
            "**" ++ (String.concat <| List.map (formatMarkdownInline True) <| inlines) ++ "**"

        -- TODO: escaping
        Code text ->
            case longestSpanOf '`' text of
                NoSpan ->
                    "`" ++ text ++ "`"

                Span n ->
                    let
                        delimiter =
                            String.repeat (n + 1) "`"
                    in
                    delimiter ++ " " ++ text ++ " " ++ delimiter

        Link inlines (Url url) title ->
            let
                text =
                    String.concat <| List.map (formatMarkdownInline fixSpecialChars) inlines

                textRaw =
                    String.concat <| List.map (formatMarkdownInline False) inlines

                isValidAutolink =
                    Url.fromString >> Maybe.isJust
            in
            if textRaw == url && title == "" && isValidAutolink url then
                if fixSpecialChars then
                    "<" ++ url ++ ">"

                else
                    url

            else
                "["
                    ++ text
                    ++ "]("
                    ++ url
                    ++ (if title == "" then
                            ""

                        else
                            " \"" ++ title ++ "\""
                       )
                    ++ ")"

        Link inlines (Ref ref) _ ->
            let
                text =
                    String.concat <| List.map (formatMarkdownInline fixSpecialChars) inlines
            in
            if text == ref || ref == "" then
                "[" ++ text ++ "]"

            else
                "[" ++ text ++ "][" ++ ref ++ "]"

        Image inlines url title ->
            "!["
                ++ (String.concat <| List.map (formatMarkdownInline fixSpecialChars) inlines)
                ++ "]("
                ++ url
                ++ (if title == "" then
                        ""

                    else
                        " \"" ++ title ++ "\""
                   )
                ++ ")"

        Entity text ->
            text

        RawHtml text ->
            text



-- TEXT EXTRA


type LongestSpanResult
    = NoSpan
    | Span Int



{- >= 1 -}


longestSpanOf : Char -> String -> LongestSpanResult
longestSpanOf char input =
    let
        step c ( currentSpan, longest ) =
            if c == char then
                ( Just (1 + Maybe.withDefault 0 currentSpan)
                , longest
                )

            else
                ( -- clear the current span
                  Nothing
                , -- and update the longest
                  endCurrentSpan ( currentSpan, longest )
                )

        endCurrentSpan acc =
            case acc of
                ( Nothing, longest ) ->
                    longest

                ( Just current, longest ) ->
                    max current longest
    in
    case String.foldl step ( Nothing, 0 ) input |> endCurrentSpan of
        0 ->
            NoSpan

        positive ->
            Span positive
