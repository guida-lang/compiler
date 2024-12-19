module Compiler.Reporting.Error.Pattern exposing (toReport)

import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Report as Report
import Types as T



-- TO REPORT


toReport : Code.Source -> T.CNPM_Error -> Report.Report
toReport source err =
    case err of
        T.CNPM_Redundant caseRegion patternRegion index ->
            Report.Report "REDUNDANT PATTERN" patternRegion [] <|
                Code.toSnippet source
                    caseRegion
                    (Just patternRegion)
                    ( D.reflow <|
                        "The "
                            ++ D.intToOrdinal index
                            ++ " pattern is redundant:"
                    , D.reflow <|
                        "Any value with this shape will be handled by a previous pattern, so it should be removed."
                    )

        T.CNPM_Incomplete region context unhandled ->
            case context of
                T.CNPM_BadArg ->
                    Report.Report "UNSAFE PATTERN" region [] <|
                        Code.toSnippet source
                            region
                            Nothing
                            ( D.fromChars "This pattern does not cover all possibilities:"
                            , D.stack
                                [ D.fromChars "Other possibilities include:"
                                , unhandledPatternsToDocBlock unhandled
                                , D.reflow <|
                                    "I would have to crash if I saw one of those! So rather than pattern matching in function arguments, put a `case` in the function body to account for all possibilities."
                                ]
                            )

                T.CNPM_BadDestruct ->
                    Report.Report "UNSAFE PATTERN" region [] <|
                        Code.toSnippet source
                            region
                            Nothing
                            ( D.fromChars "This pattern does not cover all possible values:"
                            , D.stack
                                [ D.fromChars "Other possibilities include:"
                                , unhandledPatternsToDocBlock unhandled
                                , D.reflow <|
                                    "I would have to crash if I saw one of those! You can use `let` to deconstruct values only if there is ONE possibility. Switch to a `case` expression to account for all possibilities."
                                , D.toSimpleHint <|
                                    "Are you calling a function that definitely returns values with a very specific shape? Try making the return type of that function more specific!"
                                ]
                            )

                T.CNPM_BadCase ->
                    Report.Report "MISSING PATTERNS" region [] <|
                        Code.toSnippet source
                            region
                            Nothing
                            ( D.fromChars "This `case` does not have branches for all possibilities:"
                            , D.stack
                                [ D.fromChars "Missing possibilities include:"
                                , unhandledPatternsToDocBlock unhandled
                                , D.reflow <|
                                    "I would have to crash if I saw one of those. Add branches for them!"
                                , D.link "Hint"
                                    "If you want to write the code for each branch later, use `Debug.todo` as a placeholder. Read"
                                    "missing-patterns"
                                    "for more guidance on this workflow."
                                ]
                            )



-- PATTERN TO DOC


unhandledPatternsToDocBlock : List T.CNPM_Pattern -> D.Doc
unhandledPatternsToDocBlock unhandledPatterns =
    D.indent 4 <|
        D.dullyellow <|
            D.vcat <|
                List.map (patternToDoc Unambiguous) unhandledPatterns


type Context
    = Arg
    | Head
    | Unambiguous


patternToDoc : Context -> T.CNPM_Pattern -> D.Doc
patternToDoc context pattern =
    case delist pattern [] of
        NonList T.CNPM_Anything ->
            D.fromChars "_"

        NonList (T.CNPM_Literal literal) ->
            case literal of
                T.CNPM_Chr chr ->
                    D.fromChars ("'" ++ chr ++ "'")

                T.CNPM_Str str ->
                    D.fromChars ("\"" ++ str ++ "\"")

                T.CNPM_Int int ->
                    D.fromChars (String.fromInt int)

        NonList (T.CNPM_Ctor _ "#0" []) ->
            D.fromChars "()"

        NonList (T.CNPM_Ctor _ "#2" [ a, b ]) ->
            D.fromChars "( "
                |> D.a (patternToDoc Unambiguous a)
                |> D.a (D.fromChars ", ")
                |> D.a (patternToDoc Unambiguous b)
                |> D.a (D.fromChars " )")

        NonList (T.CNPM_Ctor _ "#3" [ a, b, c ]) ->
            D.fromChars "( "
                |> D.a (patternToDoc Unambiguous a)
                |> D.a (D.fromChars ", ")
                |> D.a (patternToDoc Unambiguous b)
                |> D.a (D.fromChars ", ")
                |> D.a (patternToDoc Unambiguous c)
                |> D.a (D.fromChars " )")

        NonList (T.CNPM_Ctor _ name args) ->
            let
                ctorDoc : D.Doc
                ctorDoc =
                    D.hsep (D.fromChars name :: List.map (patternToDoc Arg) args)
            in
            if context == Arg && List.length args > 0 then
                D.fromChars "("
                    |> D.a ctorDoc
                    |> D.a (D.fromChars ")")

            else
                ctorDoc

        FiniteList [] ->
            D.fromChars "[]"

        FiniteList entries ->
            let
                entryDocs : List D.Doc
                entryDocs =
                    List.map (patternToDoc Unambiguous) entries
            in
            D.fromChars "["
                |> D.a (D.hcat (List.intersperse (D.fromChars ",") entryDocs))
                |> D.a (D.fromChars "]")

        Conses conses finalPattern ->
            let
                consDoc : D.Doc
                consDoc =
                    List.foldr
                        (\hd tl ->
                            patternToDoc Head hd
                                |> D.a (D.fromChars " :: ")
                                |> D.a tl
                        )
                        (patternToDoc Unambiguous finalPattern)
                        conses
            in
            if context == Unambiguous then
                consDoc

            else
                D.fromChars "("
                    |> D.a consDoc
                    |> D.a (D.fromChars ")")


type Structure
    = FiniteList (List T.CNPM_Pattern)
    | Conses (List T.CNPM_Pattern) T.CNPM_Pattern
    | NonList T.CNPM_Pattern


delist : T.CNPM_Pattern -> List T.CNPM_Pattern -> Structure
delist pattern revEntries =
    case pattern of
        T.CNPM_Ctor _ "[]" [] ->
            FiniteList revEntries

        T.CNPM_Ctor _ "::" [ hd, tl ] ->
            delist tl (hd :: revEntries)

        _ ->
            case revEntries of
                [] ->
                    NonList pattern

                _ ->
                    Conses (List.reverse revEntries) pattern
