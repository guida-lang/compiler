module Compiler.Reporting.Error.Canonicalize exposing (toReport)

import Compiler.Data.Index as Index
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Report as Report
import Compiler.Reporting.Suggest as Suggest
import Data.Map as Dict
import Data.Set as EverySet exposing (EverySet)
import Types as T



-- KIND


toKindInfo : T.CREC_VarKind -> T.CDN_Name -> ( D.Doc, D.Doc, D.Doc )
toKindInfo kind name =
    case kind of
        T.CREC_BadOp ->
            ( D.fromChars "an"
            , D.fromChars "operator"
            , D.fromChars "("
                |> D.a (D.fromName name)
                |> D.a (D.fromChars ")")
            )

        T.CREC_BadVar ->
            ( D.fromChars "a"
            , D.fromChars "value"
            , D.fromChars "`"
                |> D.a (D.fromName name)
                |> D.a (D.fromChars "`")
            )

        T.CREC_BadPattern ->
            ( D.fromChars "a"
            , D.fromChars "pattern"
            , D.fromChars "`"
                |> D.a (D.fromName name)
                |> D.a (D.fromChars "`")
            )

        T.CREC_BadType ->
            ( D.fromChars "a"
            , D.fromChars "type"
            , D.fromChars "`"
                |> D.a (D.fromName name)
                |> D.a (D.fromChars "`")
            )



-- TO REPORT


toReport : Code.Source -> T.CREC_Error -> Report.Report
toReport source err =
    case err of
        T.CREC_AnnotationTooShort region name index leftovers ->
            let
                numTypeArgs : Int
                numTypeArgs =
                    Index.toMachine index

                numDefArgs : Int
                numDefArgs =
                    numTypeArgs + leftovers
            in
            Report.Report "BAD TYPE ANNOTATION" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("The type annotation for `"
                            ++ name
                            ++ "` says it can accept "
                            ++ D.args numTypeArgs
                            ++ ", but the definition says it has "
                            ++ D.args numDefArgs
                            ++ ":"
                        )
                    , D.reflow
                        ("Is the type annotation missing something? Should some argument"
                            ++ (if leftovers == 1 then
                                    ""

                                else
                                    "s"
                               )
                            ++ " be deleted? Maybe some parentheses are missing?"
                        )
                    )

        T.CREC_AmbiguousVar region maybePrefix name h hs ->
            ambiguousName source region maybePrefix name h hs "variable"

        T.CREC_AmbiguousType region maybePrefix name h hs ->
            ambiguousName source region maybePrefix name h hs "type"

        T.CREC_AmbiguousVariant region maybePrefix name h hs ->
            ambiguousName source region maybePrefix name h hs "variant"

        T.CREC_AmbiguousBinop region name h hs ->
            ambiguousName source region Nothing name h hs "operator"

        T.CREC_BadArity region badArityContext name expected actual ->
            let
                thing : String
                thing =
                    case badArityContext of
                        T.CREC_TypeArity ->
                            "type"

                        T.CREC_PatternArity ->
                            "variant"
            in
            if actual < expected then
                Report.Report "TOO FEW ARGS" region [] <|
                    Code.toSnippet source
                        region
                        Nothing
                        ( D.reflow
                            ("The `"
                                ++ name
                                ++ "` "
                                ++ thing
                                ++ " needs "
                                ++ D.args expected
                                ++ ", but I see "
                                ++ String.fromInt actual
                                ++ " instead:"
                            )
                        , D.reflow
                            "What is missing? Are some parentheses misplaced?"
                        )

            else
                Report.Report "TOO MANY ARGS" region [] <|
                    Code.toSnippet source
                        region
                        Nothing
                        ( D.reflow
                            ("The `"
                                ++ name
                                ++ "` "
                                ++ thing
                                ++ " needs "
                                ++ D.args expected
                                ++ ", but I see "
                                ++ String.fromInt actual
                                ++ " instead:"
                            )
                        , if actual - expected == 1 then
                            D.fromChars "Which is the extra one? Maybe some parentheses are missing?"

                          else
                            D.fromChars "Which are the extra ones? Maybe some parentheses are missing?"
                        )

        T.CREC_Binop region op1 op2 ->
            Report.Report "INFIX PROBLEM" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("You cannot mix (" ++ op1 ++ ") and (" ++ op2 ++ ") without parentheses.")
                    , D.reflow
                        "I do not know how to group these expressions. Add parentheses for me!"
                    )

        T.CREC_DuplicateDecl name r1 r2 ->
            nameClash source r1 r2 <|
                "This file has multiple `"
                    ++ name
                    ++ "` declarations."

        T.CREC_DuplicateType name r1 r2 ->
            nameClash source r1 r2 <|
                "This file defines multiple `"
                    ++ name
                    ++ "` types."

        T.CREC_DuplicateCtor name r1 r2 ->
            nameClash source r1 r2 <|
                "This file defines multiple `"
                    ++ name
                    ++ "` type constructors."

        T.CREC_DuplicateBinop name r1 r2 ->
            nameClash source r1 r2 <|
                "This file defines multiple ("
                    ++ name
                    ++ ") operators."

        T.CREC_DuplicateField name r1 r2 ->
            nameClash source r1 r2 <|
                "This record has multiple `"
                    ++ name
                    ++ "` fields."

        T.CREC_DuplicateAliasArg typeName name r1 r2 ->
            nameClash source r1 r2 <|
                "The `"
                    ++ typeName
                    ++ "` type alias has multiple `"
                    ++ name
                    ++ "` type variables."

        T.CREC_DuplicateUnionArg typeName name r1 r2 ->
            nameClash source r1 r2 <|
                "The `"
                    ++ typeName
                    ++ "` type has multiple `"
                    ++ name
                    ++ "` type variables."

        T.CREC_DuplicatePattern context name r1 r2 ->
            nameClash source r1 r2 <|
                case context of
                    T.CREC_DPLambdaArgs ->
                        "This anonymous function has multiple `" ++ name ++ "` arguments."

                    T.CREC_DPFuncArgs funcName ->
                        "The `" ++ funcName ++ "` function has multiple `" ++ name ++ "` arguments."

                    T.CREC_DPCaseBranch ->
                        "This `case` pattern has multiple `" ++ name ++ "` variables."

                    T.CREC_DPLetBinding ->
                        "This `let` expression defines `" ++ name ++ "` more than once!"

                    T.CREC_DPDestruct ->
                        "This pattern contains multiple `" ++ name ++ "` variables."

        T.CREC_EffectNotFound region name ->
            Report.Report "EFFECT PROBLEM" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("You have declared that `" ++ name ++ "` is an effect type:")
                    , D.reflow
                        ("But I cannot find a custom type named `" ++ name ++ "` in this file!")
                    )

        T.CREC_EffectFunctionNotFound region name ->
            Report.Report "EFFECT PROBLEM" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("This kind of effect module must define a `" ++ name ++ "` function.")
                    , D.reflow
                        ("But I cannot find `" ++ name ++ "` in this file!")
                    )

        T.CREC_ExportDuplicate name r1 r2 ->
            let
                messageThatEndsWithPunctuation : String
                messageThatEndsWithPunctuation =
                    "You are trying to expose `" ++ name ++ "` multiple times!"
            in
            Report.Report "REDUNDANT EXPORT" r2 [] <|
                Code.toPair source
                    r1
                    r2
                    ( D.reflow messageThatEndsWithPunctuation
                    , D.fromChars "Remove one of them and you should be all set!"
                    )
                    ( D.reflow (messageThatEndsWithPunctuation ++ " Once here:")
                    , D.fromChars "And again right here:"
                    , D.fromChars "Remove one of them and you should be all set!"
                    )

        T.CREC_ExportNotFound region kind rawName possibleNames ->
            let
                suggestions : List String
                suggestions =
                    List.take 4 <| Suggest.sort rawName identity possibleNames
            in
            Report.Report "UNKNOWN EXPORT" region suggestions <|
                let
                    ( a, thing, name ) =
                        toKindInfo kind rawName
                in
                D.stack
                    [ D.fillSep
                        [ D.fromChars "You"
                        , D.fromChars "are"
                        , D.fromChars "trying"
                        , D.fromChars "to"
                        , D.fromChars "expose"
                        , a
                        , thing
                        , D.fromChars "named"
                        , name
                        , D.fromChars "but"
                        , D.fromChars "I"
                        , D.fromChars "cannot"
                        , D.fromChars "find"
                        , D.fromChars "its"
                        , D.fromChars "definition."
                        ]
                    , case List.map D.fromChars suggestions of
                        [] ->
                            D.reflow "I do not see any super similar names in this file. Is the definition missing?"

                        [ alt ] ->
                            D.fillSep
                                [ D.fromChars "Maybe"
                                , D.fromChars "you"
                                , D.fromChars "want"
                                , D.dullyellow alt
                                , D.fromChars "instead?"
                                ]

                        alts ->
                            D.stack
                                [ D.fromChars "These names seem close though:"
                                , D.indent 4 <| D.vcat <| List.map D.dullyellow alts
                                ]
                    ]

        T.CREC_ExportOpenAlias region name ->
            Report.Report "BAD EXPORT" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("The (..) syntax is for exposing variants of a custom type. It cannot be used with a type alias like `"
                            ++ name
                            ++ "` though."
                        )
                    , D.reflow
                        "Remove the (..) and you should be fine!"
                    )

        T.CREC_ImportCtorByName region ctor tipe ->
            Report.Report "BAD IMPORT" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("You are trying to import the `"
                            ++ ctor
                            ++ "` variant by name:"
                        )
                    , D.fillSep
                        [ D.fromChars "Try"
                        , D.fromChars "importing"
                        , D.green (D.fromName tipe |> D.a (D.fromChars "(..)"))
                        , D.fromChars "instead."
                        , D.fromChars "The"
                        , D.fromChars "dots"
                        , D.fromChars "mean"
                        , D.fromChars "“expose"
                        , D.fromChars "the"
                        , D.fromName tipe
                        , D.fromChars "type"
                        , D.fromChars "and"
                        , D.fromChars "all"
                        , D.fromChars "its"
                        , D.fromChars "variants"
                        , D.fromChars "so"
                        , D.fromChars "it"
                        , D.fromChars "gives"
                        , D.fromChars "you"
                        , D.fromChars "access"
                        , D.fromChars "to"
                        , D.fromName ctor |> D.a (D.fromChars ".")
                        ]
                    )

        T.CREC_ImportNotFound region name _ ->
            --
            -- NOTE: this should always be detected by `builder`
            -- So this error should never actually get printed out.
            --
            Report.Report "UNKNOWN IMPORT" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("I could not find a `" ++ name ++ "` module to import!")
                    , D.empty
                    )

        T.CREC_ImportOpenAlias region name ->
            Report.Report "BAD IMPORT" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("The `" ++ name ++ "` type alias cannot be followed by (..) like this:")
                    , D.reflow
                        "Remove the (..) and it should work."
                    )

        T.CREC_ImportExposingNotFound region (T.CEMN_Canonical _ home) value possibleNames ->
            let
                suggestions : List String
                suggestions =
                    List.take 4 <| Suggest.sort home identity possibleNames
            in
            Report.Report "BAD IMPORT" region suggestions <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("The `"
                            ++ home
                            ++ "` module does not expose `"
                            ++ value
                            ++ "`:"
                        )
                    , case List.map D.fromChars suggestions of
                        [] ->
                            D.fromChars "I cannot find any super similar exposed names. Maybe it is private?"

                        [ alt ] ->
                            D.fillSep
                                [ D.fromChars "Maybe"
                                , D.fromChars "you"
                                , D.fromChars "want"
                                , D.dullyellow alt
                                , D.fromChars "instead?"
                                ]

                        alts ->
                            D.stack
                                [ D.fromChars "These names seem close though:"
                                , D.indent 4 <| D.vcat <| List.map D.dullyellow alts
                                ]
                    )

        T.CREC_NotFoundVar region prefix name possibleNames ->
            notFound source region prefix name "variable" possibleNames

        T.CREC_NotFoundType region prefix name possibleNames ->
            notFound source region prefix name "type" possibleNames

        T.CREC_NotFoundVariant region prefix name possibleNames ->
            notFound source region prefix name "variant" possibleNames

        T.CREC_NotFoundBinop region op locals ->
            if op == "===" then
                Report.Report "UNKNOWN OPERATOR" region [ "==" ] <|
                    Code.toSnippet source
                        region
                        Nothing
                        ( D.fromChars "Elm does not have a (===) operator like JavaScript."
                        , D.fromChars "Switch to (==) instead."
                        )

            else if op == "!=" || op == "!==" then
                Report.Report "UNKNOWN OPERATOR" region [ "/=" ] <|
                    Code.toSnippet source
                        region
                        Nothing
                        ( D.reflow
                            "Elm uses a different name for the “not equal” operator:"
                        , D.stack
                            [ D.reflow "Switch to (/=) instead."
                            , D.toSimpleNote
                                ("Our (/=) operator is supposed to look like a real “not equal” sign (≠). I hope that history will remember ("
                                    ++ op
                                    ++ ") as a weird and temporary choice."
                                )
                            ]
                        )

            else if op == "**" then
                Report.Report "UNKNOWN OPERATOR" region [ "^", "*" ] <|
                    Code.toSnippet source
                        region
                        Nothing
                        ( D.reflow
                            "I do not recognize the (**) operator:"
                        , D.reflow
                            "Switch to (^) for exponentiation. Or switch to (*) for multiplication."
                        )

            else if op == "%" then
                Report.Report "UNKNOWN OPERATOR" region [] <|
                    Code.toSnippet source
                        region
                        Nothing
                        ( D.reflow
                            "Elm does not use (%) as the remainder operator:"
                        , D.stack
                            [ D.reflow
                                "If you want the behavior of (%) like in JavaScript, switch to: <https://package.elm-lang.org/packages/elm/core/latest/Basics#remainderBy>"
                            , D.reflow
                                "If you want modular arithmetic like in math, switch to: <https://package.elm-lang.org/packages/elm/core/latest/Basics#modBy>"
                            , D.reflow
                                "The difference is how things work when negative numbers are involved."
                            ]
                        )

            else
                let
                    suggestions : List String
                    suggestions =
                        List.take 2 <| Suggest.sort op identity (EverySet.toList compare locals)

                    format : D.Doc -> D.Doc
                    format altOp =
                        D.green
                            (D.fromChars "("
                                |> D.a altOp
                                |> D.a (D.fromChars ")")
                            )
                in
                Report.Report "UNKNOWN OPERATOR" region suggestions <|
                    Code.toSnippet source
                        region
                        Nothing
                        ( D.reflow
                            ("I do not recognize the (" ++ op ++ ") operator.")
                        , D.fillSep
                            ([ D.fromChars "Is"
                             , D.fromChars "there"
                             , D.fromChars "an"
                             , D.fromChars "`import`"
                             , D.fromChars "and"
                             , D.fromChars "`exposing`"
                             , D.fromChars "entry"
                             , D.fromChars "for"
                             , D.fromChars "it?"
                             ]
                                ++ (case List.map D.fromChars suggestions of
                                        [] ->
                                            []

                                        alts ->
                                            [ D.fromChars "Maybe", D.fromChars "you", D.fromChars "want" ]
                                                ++ D.commaSep (D.fromChars "or") format alts
                                                ++ [ D.fromChars "instead?" ]
                                   )
                            )
                        )

        T.CREC_PatternHasRecordCtor region name ->
            Report.Report "BAD PATTERN" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("You can construct records by using `"
                            ++ name
                            ++ "` as a function, but it is not available in pattern matching like this:"
                        )
                    , D.reflow
                        "I recommend matching the record as a variable and unpacking it later."
                    )

        T.CREC_PortPayloadInvalid region portName _ invalidPayload ->
            let
                formatDetails : ( String, D.Doc ) -> Report.Report
                formatDetails ( aBadKindOfThing, elaboration ) =
                    Report.Report "PORT ERROR" region [] <|
                        Code.toSnippet source
                            region
                            Nothing
                            ( D.reflow
                                ("The `" ++ portName ++ "` port is trying to transmit " ++ aBadKindOfThing ++ ":")
                            , D.stack
                                [ elaboration
                                , D.link "Hint"
                                    "Ports are not a traditional FFI, so if you have tons of annoying ports, definitely read"
                                    "ports"
                                    "to learn how they are meant to work. They require a different mindset!"
                                ]
                            )
            in
            formatDetails <|
                case invalidPayload of
                    T.CREC_ExtendedRecord ->
                        ( "an extended record"
                        , D.reflow
                            "But the exact shape of the record must be known at compile time. No type variables!"
                        )

                    T.CREC_Function ->
                        ( "a function"
                        , D.reflow
                            "But functions cannot be sent in and out ports. If we allowed functions in from JS they may perform some side-effects. If we let functions out, they could produce incorrect results because Elm optimizations assume there are no side-effects."
                        )

                    T.CREC_TypeVariable name ->
                        ( "an unspecified type"
                        , D.reflow
                            ("But type variables like `" ++ name ++ "` cannot flow through ports. I need to know exactly what type of data I am getting, so I can guarantee that unexpected data cannot sneak in and crash the Elm program.")
                        )

                    T.CREC_UnsupportedType name ->
                        ( "a `" ++ name ++ "` value"
                        , D.stack
                            [ D.reflow "I cannot handle that. The types that CAN flow in and out of Elm include:"
                            , D.indent 4 <|
                                D.reflow
                                    "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays, tuples, records, and JSON values."
                            , D.reflow
                                "Since JSON values can flow through, you can use JSON encoders and decoders to allow other types through as well. More advanced users often just do everything with encoders and decoders for more control and better errors."
                            ]
                        )

        T.CREC_PortTypeInvalid region name portProblem ->
            let
                formatDetails : ( String, D.Doc ) -> Report.Report
                formatDetails ( before, after ) =
                    Report.Report "BAD PORT" region [] <|
                        Code.toSnippet source
                            region
                            Nothing
                            ( D.reflow before
                            , D.stack
                                [ after
                                , D.link "Hint"
                                    "Read"
                                    "ports"
                                    "for more advice. For example, do not end up with one port per JS function!"
                                ]
                            )
            in
            formatDetails <|
                case portProblem of
                    T.CREC_CmdNoArg ->
                        ( "The `" ++ name ++ "` port cannot be just a command."
                        , D.reflow
                            "It can be (() -> Cmd msg) if you just need to trigger a JavaScript function, but there is often a better way to set things up."
                        )

                    T.CREC_CmdExtraArgs n ->
                        ( "The `" ++ name ++ "` port can only send ONE value out to JavaScript."
                        , let
                            theseItemsInSomething : String
                            theseItemsInSomething =
                                if n == 2 then
                                    "both of these items into a tuple or record"

                                else if n == 3 then
                                    "these " ++ String.fromInt n ++ " items into a tuple or record"

                                else
                                    "these " ++ String.fromInt n ++ " items into a record"
                          in
                          D.reflow <| "You can put " ++ theseItemsInSomething ++ " to send them out though."
                        )

                    T.CREC_CmdBadMsg ->
                        ( "The `" ++ name ++ "` port cannot send any messages to the `update` function."
                        , D.reflow
                            "It must produce a (Cmd msg) type. Notice the lower case `msg` type variable. The command will trigger some JS code, but it will not send anything particular back to Elm."
                        )

                    T.CREC_SubBad ->
                        ( "There is something off about this `" ++ name ++ "` port declaration."
                        , D.stack
                            [ D.reflow
                                "To receive messages from JavaScript, you need to define a port like this:"
                            , D.indent 4 <| D.dullyellow <| D.fromChars <| "port " ++ name ++ " : (Int -> msg) -> Sub msg"
                            , D.reflow
                                "Now every time JS sends an `Int` to this port, it is converted to a `msg`. And if you subscribe, those `msg` values will be piped into your `update` function. The only thing you can customize here is the `Int` type."
                            ]
                        )

                    T.CREC_NotCmdOrSub ->
                        ( "I am confused about the `" ++ name ++ "` port declaration."
                        , D.reflow
                            "Ports need to produce a command (Cmd) or a subscription (Sub) but this is neither. I do not know how to handle this."
                        )

        T.CREC_RecursiveAlias region name args tipe others ->
            aliasRecursionReport source region name args tipe others

        T.CREC_RecursiveDecl region name names ->
            let
                makeTheory : String -> String -> D.Doc
                makeTheory question details =
                    D.fillSep <| List.map (D.dullyellow << D.fromChars) (String.words question) ++ List.map D.fromChars (String.words details)
            in
            Report.Report "CYCLIC DEFINITION" region [] <|
                Code.toSnippet source region Nothing <|
                    case names of
                        [] ->
                            ( D.reflow <| "The `" ++ name ++ "` value is defined directly in terms of itself, causing an infinite loop."
                            , D.stack
                                [ makeTheory "Are you trying to mutate a variable?" <| "Elm does not have mutation, so when I see " ++ name ++ " defined in terms of " ++ name ++ ", I treat it as a recursive definition. Try giving the new value a new name!"
                                , makeTheory "Maybe you DO want a recursive value?" <| "To define " ++ name ++ " we need to know what " ++ name ++ " is, so let’s expand it. Wait, but now we need to know what " ++ name ++ " is, so let’s expand it... This will keep going infinitely!"
                                , D.link "Hint" "The root problem is often a typo in some variable name, but I recommend reading" "bad-recursion" "for more detailed advice, especially if you actually do need a recursive value."
                                ]
                            )

                        _ :: _ ->
                            ( D.reflow <| "The `" ++ name ++ "` definition is causing a very tricky infinite loop."
                            , D.stack
                                [ D.reflow <| "The `" ++ name ++ "` value depends on itself through the following chain of definitions:"
                                , D.cycle 4 name names
                                , D.link "Hint" "The root problem is often a typo in some variable name, but I recommend reading" "bad-recursion" "for more detailed advice, especially if you actually do want mutually recursive values."
                                ]
                            )

        T.CREC_RecursiveLet (T.CRA_At region name) names ->
            Report.Report "CYCLIC VALUE" region [] <|
                Code.toSnippet source region Nothing <|
                    case names of
                        [] ->
                            let
                                makeTheory : String -> String -> D.Doc
                                makeTheory question details =
                                    D.fillSep <| List.map (D.dullyellow << D.fromChars) (String.words question) ++ List.map D.fromChars (String.words details)
                            in
                            ( D.reflow <| "The `" ++ name ++ "` value is defined directly in terms of itself, causing an infinite loop."
                            , D.stack
                                [ makeTheory "Are you trying to mutate a variable?" <| "Elm does not have mutation, so when I see " ++ name ++ " defined in terms of " ++ name ++ ", I treat it as a recursive definition. Try giving the new value a new name!"
                                , makeTheory "Maybe you DO want a recursive value?" <| "To define " ++ name ++ " we need to know what " ++ name ++ " is, so let’s expand it. Wait, but now we need to know what " ++ name ++ " is, so let’s expand it... This will keep going infinitely!"
                                , D.link "Hint" "The root problem is often a typo in some variable name, but I recommend reading" "bad-recursion" "for more detailed advice, especially if you actually do need a recursive value."
                                ]
                            )

                        _ ->
                            ( D.reflow <| "I do not allow cyclic values in `let` expressions."
                            , D.stack
                                [ D.reflow <| "The `" ++ name ++ "` value depends on itself through the following chain of definitions:"
                                , D.cycle 4 name names
                                , D.link "Hint" "The root problem is often a typo in some variable name, but I recommend reading" "bad-recursion" "for more detailed advice, especially if you actually do want mutually recursive values."
                                ]
                            )

        T.CREC_Shadowing name r1 r2 ->
            let
                advice : D.Doc
                advice =
                    D.stack
                        [ D.reflow <| "Think of a more helpful name for one of them and you should be all set!"
                        , D.link "Note" "Linters advise against shadowing, so Elm makes “best practices” the default. Read" "shadowing" "for more details on this choice."
                        ]
            in
            Report.Report "SHADOWING" r2 [] <|
                Code.toPair source
                    r1
                    r2
                    ( D.fromChars "These variables cannot have the same name:"
                    , advice
                    )
                    ( D.reflow <| "The name `" ++ name ++ "` is first defined here:"
                    , D.fromChars "But then it is defined AGAIN over here:"
                    , advice
                    )

        T.CREC_TupleLargerThanThree region ->
            Report.Report "BAD TUPLE" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.fromChars "I only accept tuples with two or three items. This has too many:"
                    , D.stack
                        [ D.reflow <| "I recommend switching to records. Each item will be named, and you can use the `point.x` syntax to access them."
                        , D.link "Note" "Read" "tuples" "for more comprehensive advice on working with large chunks of data in Elm."
                        ]
                    )

        T.CREC_TypeVarsUnboundInUnion unionRegion typeName allVars unbound unbounds ->
            unboundTypeVars source unionRegion [ D.fromChars "type" ] typeName allVars unbound unbounds

        T.CREC_TypeVarsMessedUpInAlias aliasRegion typeName allVars unusedVars unboundVars ->
            case ( unusedVars, unboundVars ) of
                ( unused :: unuseds, [] ) ->
                    let
                        backQuote : T.CDN_Name -> D.Doc
                        backQuote name =
                            D.fromChars "`"
                                |> D.a (D.fromName name)
                                |> D.a (D.fromChars "`")

                        allUnusedNames : List T.CDN_Name
                        allUnusedNames =
                            List.map Tuple.first unusedVars

                        ( ( title, subRegion ), ( overview, stuff ) ) =
                            case unuseds of
                                [] ->
                                    ( ( "UNUSED TYPE VARIABLE"
                                      , Just (Tuple.second unused)
                                      )
                                    , ( [ D.fromChars "Type"
                                        , D.fromChars "alias"
                                        , backQuote typeName
                                        , D.fromChars "does"
                                        , D.fromChars "not"
                                        , D.fromChars "use"
                                        , D.fromChars "the"
                                        , backQuote (Tuple.first unused)
                                        , D.fromChars "type"
                                        , D.fromChars "variable."
                                        ]
                                      , [ D.dullyellow (backQuote (Tuple.first unused)) ]
                                      )
                                    )

                                _ :: _ ->
                                    ( ( "UNUSED TYPE VARIABLES"
                                      , Nothing
                                      )
                                    , ( [ D.fromChars "Type", D.fromChars "variables" ]
                                            ++ D.commaSep (D.fromChars "and") identity (List.map D.fromName allUnusedNames)
                                            ++ [ D.fromChars "are"
                                               , D.fromChars "unused"
                                               , D.fromChars "in"
                                               , D.fromChars "the"
                                               , backQuote typeName
                                               , D.fromChars "definition."
                                               ]
                                      , D.commaSep (D.fromChars "and") D.dullyellow (List.map D.fromName allUnusedNames)
                                      )
                                    )
                    in
                    Report.Report title aliasRegion [] <|
                        Code.toSnippet source
                            aliasRegion
                            subRegion
                            ( D.fillSep overview
                            , D.stack
                                [ D.fillSep <| [ D.fromChars "I", D.fromChars "recommend", D.fromChars "removing" ] ++ stuff ++ [ D.fromChars "from", D.fromChars "the", D.fromChars "declaration,", D.fromChars "like", D.fromChars "this:" ]
                                , D.indent 4 <| D.hsep <| [ D.fromChars "type", D.fromChars "alias", D.green (D.fromName typeName) ] ++ List.map D.fromName (List.filter (\var -> not (List.member var allUnusedNames)) allVars) ++ [ D.fromChars "=", D.fromChars "..." ]
                                , D.reflow <| "Why? Well, if I allowed `type alias Height a = Float` I would need to answer some weird questions. Is `Height Bool` the same as `Float`? Is `Height Bool` the same as `Height Int`? My solution is to not need to ask them!"
                                ]
                            )

                ( [], unbound :: unbounds ) ->
                    unboundTypeVars source aliasRegion [ D.fromChars "type", D.fromChars "alias" ] typeName allVars unbound unbounds

                _ ->
                    let
                        unused : List T.CDN_Name
                        unused =
                            List.map Tuple.first unusedVars

                        unbound : List T.CDN_Name
                        unbound =
                            List.map Tuple.first unboundVars

                        theseAreUsed : List D.Doc
                        theseAreUsed =
                            case unbound of
                                [ x ] ->
                                    [ D.fromChars "Type"
                                    , D.fromChars "variable"
                                    , D.dullyellow
                                        (D.fromChars "`"
                                            |> D.a (D.fromName x)
                                            |> D.a (D.fromChars "`")
                                        )
                                    , D.fromChars "appears"
                                    , D.fromChars "in"
                                    , D.fromChars "the"
                                    , D.fromChars "definition,"
                                    , D.fromChars "but"
                                    , D.fromChars "I"
                                    , D.fromChars "do"
                                    , D.fromChars "not"
                                    , D.fromChars "see"
                                    , D.fromChars "it"
                                    , D.fromChars "declared."
                                    ]

                                _ ->
                                    [ D.fromChars "Type", D.fromChars "variables" ]
                                        ++ D.commaSep (D.fromChars "and") D.dullyellow (List.map D.fromName unbound)
                                        ++ [ D.fromChars "are"
                                           , D.fromChars "used"
                                           , D.fromChars "in"
                                           , D.fromChars "the"
                                           , D.fromChars "definition,"
                                           , D.fromChars "but"
                                           , D.fromChars "I"
                                           , D.fromChars "do"
                                           , D.fromChars "not"
                                           , D.fromChars "see"
                                           , D.fromChars "them"
                                           , D.fromChars "declared."
                                           ]

                        butTheseAreUnused : List D.Doc
                        butTheseAreUnused =
                            case unused of
                                [ x ] ->
                                    [ D.fromChars "Likewise,"
                                    , D.fromChars "type"
                                    , D.fromChars "variable"
                                    , D.dullyellow
                                        (D.fromChars "`"
                                            |> D.a (D.fromName x)
                                            |> D.a (D.fromChars "`")
                                        )
                                    , D.fromChars "is"
                                    , D.fromChars "delared,"
                                    , D.fromChars "but"
                                    , D.fromChars "not"
                                    , D.fromChars "used."
                                    ]

                                _ ->
                                    [ D.fromChars "Likewise,", D.fromChars "type", D.fromChars "variables" ]
                                        ++ D.commaSep (D.fromChars "and") D.dullyellow (List.map D.fromName unused)
                                        ++ [ D.fromChars "are", D.fromChars "delared,", D.fromChars "but", D.fromChars "not", D.fromChars "used." ]
                    in
                    Report.Report "TYPE VARIABLE PROBLEMS" aliasRegion [] <|
                        Code.toSnippet source
                            aliasRegion
                            Nothing
                            ( D.reflow <| "Type alias `" ++ typeName ++ "` has some type variable problems."
                            , D.stack
                                [ D.fillSep <| theseAreUsed ++ butTheseAreUnused
                                , D.reflow <| "My guess is that a definition like this will work better:"
                                , D.indent 4 <| D.hsep <| [ D.fromChars "type", D.fromChars "alias", D.fromName typeName ] ++ List.map D.fromName (List.filter (\var -> not (List.member var unused)) allVars) ++ List.map (D.green << D.fromName) unbound ++ [ D.fromChars "=", D.fromChars "..." ]
                                ]
                            )


unboundTypeVars : Code.Source -> T.CRA_Region -> List D.Doc -> T.CDN_Name -> List T.CDN_Name -> ( T.CDN_Name, T.CRA_Region ) -> List ( T.CDN_Name, T.CRA_Region ) -> Report.Report
unboundTypeVars source declRegion tipe typeName allVars ( unboundVar, varRegion ) unboundVars =
    let
        backQuote : T.CDN_Name -> D.Doc
        backQuote name =
            D.fromChars "`"
                |> D.a (D.fromName name)
                |> D.a (D.fromChars "`")

        ( title, subRegion, overview ) =
            case List.map Tuple.first unboundVars of
                [] ->
                    ( "UNBOUND TYPE VARIABLE"
                    , Just varRegion
                    , [ D.fromChars "The", backQuote typeName ]
                        ++ tipe
                        ++ [ D.fromChars "uses"
                           , D.fromChars "an"
                           , D.fromChars "unbound"
                           , D.fromChars "type"
                           , D.fromChars "variable"
                           , D.dullyellow (backQuote unboundVar)
                           , D.fromChars "in"
                           , D.fromChars "its"
                           , D.fromChars "definition:"
                           ]
                    )

                vars ->
                    ( "UNBOUND TYPE VARIABLES"
                    , Nothing
                    , [ D.fromChars "Type", D.fromChars "variables" ]
                        ++ D.commaSep (D.fromChars "and") D.dullyellow (D.fromName unboundVar :: List.map D.fromName vars)
                        ++ [ D.fromChars "are"
                           , D.fromChars "unbound"
                           , D.fromChars "in"
                           , D.fromChars "the"
                           , backQuote typeName
                           ]
                        ++ tipe
                        ++ [ D.fromChars "definition:" ]
                    )
    in
    Report.Report title declRegion [] <|
        Code.toSnippet source
            declRegion
            subRegion
            ( D.fillSep overview
            , D.stack
                [ D.reflow "You probably need to change the declaration to something like this:"
                , D.indent 4 <|
                    D.hsep <|
                        tipe
                            ++ D.fromName typeName
                            :: List.map D.fromName allVars
                            ++ List.map (D.green << D.fromName) (unboundVar :: List.map Tuple.first unboundVars)
                            ++ [ D.fromChars "=", D.fromChars "..." ]
                , D.reflow <|
                    "Why? Well, imagine one `"
                        ++ typeName
                        ++ "` where `"
                        ++ unboundVar
                        ++ "` is an Int and another where it is a Bool. When we explicitly list the type variables, the type checker can see that they are actually different types."
                ]
            )


nameClash : Code.Source -> T.CRA_Region -> T.CRA_Region -> String -> Report.Report
nameClash source r1 r2 messageThatEndsWithPunctuation =
    Report.Report "NAME CLASH" r2 [] <|
        Code.toPair source
            r1
            r2
            ( D.reflow messageThatEndsWithPunctuation
            , D.fromChars "How can I know which one you want? Rename one of them!"
            )
            ( D.reflow (messageThatEndsWithPunctuation ++ " One here:")
            , D.fromChars "And another one here:"
            , D.fromChars "How can I know which one you want? Rename one of them!"
            )


ambiguousName : Code.Source -> T.CRA_Region -> Maybe T.CDN_Name -> T.CDN_Name -> T.CEMN_Canonical -> OneOrMore.OneOrMore T.CEMN_Canonical -> String -> Report.Report
ambiguousName source region maybePrefix name h hs thing =
    let
        possibleHomes : List T.CEMN_Canonical
        possibleHomes =
            List.sortWith ModuleName.compareCanonical (h :: OneOrMore.destruct (::) hs)
    in
    Report.Report "AMBIGUOUS NAME" region [] <|
        Code.toSnippet source region Nothing <|
            case maybePrefix of
                Nothing ->
                    let
                        homeToYellowDoc : T.CEMN_Canonical -> D.Doc
                        homeToYellowDoc (T.CEMN_Canonical _ home) =
                            D.dullyellow
                                (D.fromName home
                                    |> D.a (D.fromChars ".")
                                    |> D.a (D.fromName name)
                                )
                    in
                    ( D.reflow ("This usage of `" ++ name ++ "` is ambiguous:")
                    , D.stack
                        [ D.reflow <|
                            "This name is exposed by "
                                ++ String.fromInt (List.length possibleHomes)
                                ++ " of your imports, so I am not sure which one to use:"
                        , D.indent 4 <| D.vcat (List.map homeToYellowDoc possibleHomes)
                        , D.reflow "I recommend using qualified names for imported values. I also recommend having at most one `exposing (..)` per file to make name clashes like this less common in the long run."
                        , D.link "Note" "Check out" "imports" "for more info on the import syntax."
                        ]
                    )

                Just prefix ->
                    let
                        homeToYellowDoc : T.CEMN_Canonical -> D.Doc
                        homeToYellowDoc (T.CEMN_Canonical _ home) =
                            if prefix == home then
                                D.cyan (D.fromChars "import")
                                    |> D.plus (D.fromName home)

                            else
                                D.cyan (D.fromChars "import")
                                    |> D.plus (D.fromName home)
                                    |> D.plus (D.cyan (D.fromChars "as"))
                                    |> D.plus (D.fromName prefix)

                        eitherOrAny : String
                        eitherOrAny =
                            if List.length possibleHomes == 2 then
                                "either"

                            else
                                "any"
                    in
                    ( D.reflow ("This usage of `" ++ toQualString prefix name ++ "` is ambiguous.")
                    , D.stack
                        [ D.reflow <|
                            "It could refer to a "
                                ++ thing
                                ++ " from "
                                ++ eitherOrAny
                                ++ " of these imports:"
                        , D.indent 4 <| D.vcat (List.map homeToYellowDoc possibleHomes)
                        , D.reflowLink "Read" "imports" "to learn how to clarify which one you want."
                        ]
                    )


notFound : Code.Source -> T.CRA_Region -> Maybe T.CDN_Name -> T.CDN_Name -> String -> T.CREC_PossibleNames -> Report.Report
notFound source region maybePrefix name thing { locals, quals } =
    let
        givenName : T.CDN_Name
        givenName =
            Maybe.withDefault name (Maybe.map2 toQualString maybePrefix (Just name))

        possibleNames : List String
        possibleNames =
            let
                addQuals : T.CDN_Name -> EverySet String T.CDN_Name -> List String -> List String
                addQuals prefix localSet allNames =
                    EverySet.foldr compare (\x xs -> toQualString prefix x :: xs) allNames localSet
            in
            Dict.foldr compare addQuals (EverySet.toList compare locals) quals

        nearbyNames : List String
        nearbyNames =
            List.take 4 (Suggest.sort givenName identity possibleNames)

        toDetails : String -> String -> D.Doc
        toDetails noSuggestionDetails yesSuggestionDetails =
            case nearbyNames of
                [] ->
                    D.stack
                        [ D.reflow noSuggestionDetails
                        , D.link "Hint" "Read" "imports" "to see how `import` declarations work in Elm."
                        ]

                suggestions ->
                    D.stack
                        [ D.reflow yesSuggestionDetails
                        , D.indent 4 <| D.vcat (List.map D.dullyellow (List.map D.fromChars suggestions))
                        , D.link "Hint" "Read" "imports" "to see how `import` declarations work in Elm."
                        ]
    in
    Report.Report "NAMING ERROR" region nearbyNames <|
        Code.toSnippet source
            region
            Nothing
            ( D.reflow ("I cannot find a `" ++ givenName ++ "` " ++ thing ++ ":")
            , case maybePrefix of
                Nothing ->
                    toDetails
                        "Is there an `import` or `exposing` missing up top?"
                        "These names seem close though:"

                Just prefix ->
                    case Dict.get identity prefix quals of
                        Nothing ->
                            toDetails
                                ("I cannot find a `" ++ prefix ++ "` module. Is there an `import` for it?")
                                ("I cannot find a `" ++ prefix ++ "` import. These names seem close though:")

                        Just _ ->
                            toDetails
                                ("The `" ++ prefix ++ "` module does not expose a `" ++ name ++ "` " ++ thing ++ ".")
                                ("The `" ++ prefix ++ "` module does not expose a `" ++ name ++ "` " ++ thing ++ ". These names seem close though:")
            )


toQualString : T.CDN_Name -> T.CDN_Name -> String
toQualString prefix name =
    prefix ++ "." ++ name



-- BAD ALIAS RECURSION


aliasRecursionReport : Code.Source -> T.CRA_Region -> T.CDN_Name -> List T.CDN_Name -> T.CASTS_Type -> List T.CDN_Name -> Report.Report
aliasRecursionReport source region name args tipe others =
    case others of
        [] ->
            Report.Report "ALIAS PROBLEM" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.fromChars "This type alias is recursive, forming an infinite type!"
                    , D.stack
                        [ D.reflow "When I expand a recursive type alias, it just keeps getting bigger and bigger. So dealiasing results in an infinitely large type! Try this instead:"
                        , D.indent 4 <| aliasToUnionDoc name args tipe
                        , D.link "Hint" "This is kind of a subtle distinction. I suggested the naive fix, but I recommend reading" "recursive-alias" "for ideas on how to do better."
                        ]
                    )

        _ ->
            Report.Report "ALIAS PROBLEM" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.fromChars "This type alias is part of a mutually recursive set of type aliases."
                    , D.stack
                        [ D.fromChars "It is part of this cycle of type aliases:"
                        , D.cycle 4 name others
                        , D.reflow "You need to convert at least one of these type aliases into a `type`."
                        , D.link "Note" "Read" "recursive-alias" "to learn why this `type` vs `type alias` distinction matters. It is subtle but important!"
                        ]
                    )


aliasToUnionDoc : T.CDN_Name -> List T.CDN_Name -> T.CASTS_Type -> D.Doc
aliasToUnionDoc name args tipe =
    D.vcat
        [ D.dullyellow <|
            (D.fromChars "type "
                |> D.plus (D.fromName name)
                |> D.plus (List.foldr D.plus (D.fromChars "=") (List.map D.fromName args))
            )
        , D.green <| D.indent 4 (D.fromChars name)
        , D.dullyellow <| D.indent 8 (RT.srcToDoc RT.App tipe)
        ]
