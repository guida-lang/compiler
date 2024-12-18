module Compiler.Reporting.Error.Canonicalize exposing
    ( CREC_BadArityContext(..)
    , CREC_DuplicatePatternContext(..)
    , CREC_Error(..)
    , CREC_InvalidPayload(..)
    , CREC_PortProblem(..)
    , CREC_PossibleNames
    , CREC_VarKind(..)
    , errorDecoder
    , errorEncoder
    , invalidPayloadDecoder
    , invalidPayloadEncoder
    , toReport
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Data.Index as Index
import Compiler.Data.OneOrMore as OneOrMore exposing (OneOrMore)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Json.Decode as DecodeX
import Compiler.Json.Encode as EncodeX
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Report as Report
import Compiler.Reporting.Suggest as Suggest
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T



-- CANONICALIZATION ERRORS


type CREC_Error
    = CREC_AnnotationTooShort T.CRA_Region T.CDN_Name T.CDI_ZeroBased Int
    | CREC_AmbiguousVar T.CRA_Region (Maybe T.CDN_Name) T.CDN_Name T.CEMN_Canonical (OneOrMore T.CEMN_Canonical)
    | CREC_AmbiguousType T.CRA_Region (Maybe T.CDN_Name) T.CDN_Name T.CEMN_Canonical (OneOrMore T.CEMN_Canonical)
    | CREC_AmbiguousVariant T.CRA_Region (Maybe T.CDN_Name) T.CDN_Name T.CEMN_Canonical (OneOrMore T.CEMN_Canonical)
    | CREC_AmbiguousBinop T.CRA_Region T.CDN_Name T.CEMN_Canonical (OneOrMore T.CEMN_Canonical)
    | CREC_BadArity T.CRA_Region CREC_BadArityContext T.CDN_Name Int Int
    | CREC_Binop T.CRA_Region T.CDN_Name T.CDN_Name
    | CREC_DuplicateDecl T.CDN_Name T.CRA_Region T.CRA_Region
    | CREC_DuplicateType T.CDN_Name T.CRA_Region T.CRA_Region
    | CREC_DuplicateCtor T.CDN_Name T.CRA_Region T.CRA_Region
    | CREC_DuplicateBinop T.CDN_Name T.CRA_Region T.CRA_Region
    | CREC_DuplicateField T.CDN_Name T.CRA_Region T.CRA_Region
    | CREC_DuplicateAliasArg T.CDN_Name T.CDN_Name T.CRA_Region T.CRA_Region
    | CREC_DuplicateUnionArg T.CDN_Name T.CDN_Name T.CRA_Region T.CRA_Region
    | CREC_DuplicatePattern CREC_DuplicatePatternContext T.CDN_Name T.CRA_Region T.CRA_Region
    | CREC_EffectNotFound T.CRA_Region T.CDN_Name
    | CREC_EffectFunctionNotFound T.CRA_Region T.CDN_Name
    | CREC_ExportDuplicate T.CDN_Name T.CRA_Region T.CRA_Region
    | CREC_ExportNotFound T.CRA_Region CREC_VarKind T.CDN_Name (List T.CDN_Name)
    | CREC_ExportOpenAlias T.CRA_Region T.CDN_Name
    | CREC_ImportCtorByName T.CRA_Region T.CDN_Name T.CDN_Name
    | CREC_ImportNotFound T.CRA_Region T.CDN_Name (List T.CEMN_Canonical)
    | CREC_ImportOpenAlias T.CRA_Region T.CDN_Name
    | CREC_ImportExposingNotFound T.CRA_Region T.CEMN_Canonical T.CDN_Name (List T.CDN_Name)
    | CREC_NotFoundVar T.CRA_Region (Maybe T.CDN_Name) T.CDN_Name CREC_PossibleNames
    | CREC_NotFoundType T.CRA_Region (Maybe T.CDN_Name) T.CDN_Name CREC_PossibleNames
    | CREC_NotFoundVariant T.CRA_Region (Maybe T.CDN_Name) T.CDN_Name CREC_PossibleNames
    | CREC_NotFoundBinop T.CRA_Region T.CDN_Name (EverySet String T.CDN_Name)
    | CREC_PatternHasRecordCtor T.CRA_Region T.CDN_Name
    | CREC_PortPayloadInvalid T.CRA_Region T.CDN_Name T.CASTC_Type CREC_InvalidPayload
    | CREC_PortTypeInvalid T.CRA_Region T.CDN_Name CREC_PortProblem
    | CREC_RecursiveAlias T.CRA_Region T.CDN_Name (List T.CDN_Name) T.CASTS_Type (List T.CDN_Name)
    | CREC_RecursiveDecl T.CRA_Region T.CDN_Name (List T.CDN_Name)
    | CREC_RecursiveLet (T.CRA_Located T.CDN_Name) (List T.CDN_Name)
    | CREC_Shadowing T.CDN_Name T.CRA_Region T.CRA_Region
    | CREC_TupleLargerThanThree T.CRA_Region
    | CREC_TypeVarsUnboundInUnion T.CRA_Region T.CDN_Name (List T.CDN_Name) ( T.CDN_Name, T.CRA_Region ) (List ( T.CDN_Name, T.CRA_Region ))
    | CREC_TypeVarsMessedUpInAlias T.CRA_Region T.CDN_Name (List T.CDN_Name) (List ( T.CDN_Name, T.CRA_Region )) (List ( T.CDN_Name, T.CRA_Region ))


type CREC_BadArityContext
    = CREC_TypeArity
    | CREC_PatternArity


type CREC_DuplicatePatternContext
    = CREC_DPLambdaArgs
    | CREC_DPFuncArgs T.CDN_Name
    | CREC_DPCaseBranch
    | CREC_DPLetBinding
    | CREC_DPDestruct


type CREC_InvalidPayload
    = CREC_ExtendedRecord
    | CREC_Function
    | CREC_TypeVariable T.CDN_Name
    | CREC_UnsupportedType T.CDN_Name


type CREC_PortProblem
    = CREC_CmdNoArg
    | CREC_CmdExtraArgs Int
    | CREC_CmdBadMsg
    | CREC_SubBad
    | CREC_NotCmdOrSub


type alias CREC_PossibleNames =
    { locals : EverySet String T.CDN_Name
    , quals : Dict String T.CDN_Name (EverySet String T.CDN_Name)
    }



-- KIND


type CREC_VarKind
    = CREC_BadOp
    | CREC_BadVar
    | CREC_BadPattern
    | CREC_BadType


toKindInfo : CREC_VarKind -> T.CDN_Name -> ( D.Doc, D.Doc, D.Doc )
toKindInfo kind name =
    case kind of
        CREC_BadOp ->
            ( D.fromChars "an"
            , D.fromChars "operator"
            , D.fromChars "("
                |> D.a (D.fromName name)
                |> D.a (D.fromChars ")")
            )

        CREC_BadVar ->
            ( D.fromChars "a"
            , D.fromChars "value"
            , D.fromChars "`"
                |> D.a (D.fromName name)
                |> D.a (D.fromChars "`")
            )

        CREC_BadPattern ->
            ( D.fromChars "a"
            , D.fromChars "pattern"
            , D.fromChars "`"
                |> D.a (D.fromName name)
                |> D.a (D.fromChars "`")
            )

        CREC_BadType ->
            ( D.fromChars "a"
            , D.fromChars "type"
            , D.fromChars "`"
                |> D.a (D.fromName name)
                |> D.a (D.fromChars "`")
            )



-- TO REPORT


toReport : Code.Source -> CREC_Error -> Report.Report
toReport source err =
    case err of
        CREC_AnnotationTooShort region name index leftovers ->
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

        CREC_AmbiguousVar region maybePrefix name h hs ->
            ambiguousName source region maybePrefix name h hs "variable"

        CREC_AmbiguousType region maybePrefix name h hs ->
            ambiguousName source region maybePrefix name h hs "type"

        CREC_AmbiguousVariant region maybePrefix name h hs ->
            ambiguousName source region maybePrefix name h hs "variant"

        CREC_AmbiguousBinop region name h hs ->
            ambiguousName source region Nothing name h hs "operator"

        CREC_BadArity region badArityContext name expected actual ->
            let
                thing : String
                thing =
                    case badArityContext of
                        CREC_TypeArity ->
                            "type"

                        CREC_PatternArity ->
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

        CREC_Binop region op1 op2 ->
            Report.Report "INFIX PROBLEM" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("You cannot mix (" ++ op1 ++ ") and (" ++ op2 ++ ") without parentheses.")
                    , D.reflow
                        "I do not know how to group these expressions. Add parentheses for me!"
                    )

        CREC_DuplicateDecl name r1 r2 ->
            nameClash source r1 r2 <|
                "This file has multiple `"
                    ++ name
                    ++ "` declarations."

        CREC_DuplicateType name r1 r2 ->
            nameClash source r1 r2 <|
                "This file defines multiple `"
                    ++ name
                    ++ "` types."

        CREC_DuplicateCtor name r1 r2 ->
            nameClash source r1 r2 <|
                "This file defines multiple `"
                    ++ name
                    ++ "` type constructors."

        CREC_DuplicateBinop name r1 r2 ->
            nameClash source r1 r2 <|
                "This file defines multiple ("
                    ++ name
                    ++ ") operators."

        CREC_DuplicateField name r1 r2 ->
            nameClash source r1 r2 <|
                "This record has multiple `"
                    ++ name
                    ++ "` fields."

        CREC_DuplicateAliasArg typeName name r1 r2 ->
            nameClash source r1 r2 <|
                "The `"
                    ++ typeName
                    ++ "` type alias has multiple `"
                    ++ name
                    ++ "` type variables."

        CREC_DuplicateUnionArg typeName name r1 r2 ->
            nameClash source r1 r2 <|
                "The `"
                    ++ typeName
                    ++ "` type has multiple `"
                    ++ name
                    ++ "` type variables."

        CREC_DuplicatePattern context name r1 r2 ->
            nameClash source r1 r2 <|
                case context of
                    CREC_DPLambdaArgs ->
                        "This anonymous function has multiple `" ++ name ++ "` arguments."

                    CREC_DPFuncArgs funcName ->
                        "The `" ++ funcName ++ "` function has multiple `" ++ name ++ "` arguments."

                    CREC_DPCaseBranch ->
                        "This `case` pattern has multiple `" ++ name ++ "` variables."

                    CREC_DPLetBinding ->
                        "This `let` expression defines `" ++ name ++ "` more than once!"

                    CREC_DPDestruct ->
                        "This pattern contains multiple `" ++ name ++ "` variables."

        CREC_EffectNotFound region name ->
            Report.Report "EFFECT PROBLEM" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("You have declared that `" ++ name ++ "` is an effect type:")
                    , D.reflow
                        ("But I cannot find a custom type named `" ++ name ++ "` in this file!")
                    )

        CREC_EffectFunctionNotFound region name ->
            Report.Report "EFFECT PROBLEM" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("This kind of effect module must define a `" ++ name ++ "` function.")
                    , D.reflow
                        ("But I cannot find `" ++ name ++ "` in this file!")
                    )

        CREC_ExportDuplicate name r1 r2 ->
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

        CREC_ExportNotFound region kind rawName possibleNames ->
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

        CREC_ExportOpenAlias region name ->
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

        CREC_ImportCtorByName region ctor tipe ->
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

        CREC_ImportNotFound region name _ ->
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

        CREC_ImportOpenAlias region name ->
            Report.Report "BAD IMPORT" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("The `" ++ name ++ "` type alias cannot be followed by (..) like this:")
                    , D.reflow
                        "Remove the (..) and it should work."
                    )

        CREC_ImportExposingNotFound region (T.CEMN_Canonical _ home) value possibleNames ->
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

        CREC_NotFoundVar region prefix name possibleNames ->
            notFound source region prefix name "variable" possibleNames

        CREC_NotFoundType region prefix name possibleNames ->
            notFound source region prefix name "type" possibleNames

        CREC_NotFoundVariant region prefix name possibleNames ->
            notFound source region prefix name "variant" possibleNames

        CREC_NotFoundBinop region op locals ->
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

        CREC_PatternHasRecordCtor region name ->
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

        CREC_PortPayloadInvalid region portName _ invalidPayload ->
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
                    CREC_ExtendedRecord ->
                        ( "an extended record"
                        , D.reflow
                            "But the exact shape of the record must be known at compile time. No type variables!"
                        )

                    CREC_Function ->
                        ( "a function"
                        , D.reflow
                            "But functions cannot be sent in and out ports. If we allowed functions in from JS they may perform some side-effects. If we let functions out, they could produce incorrect results because Elm optimizations assume there are no side-effects."
                        )

                    CREC_TypeVariable name ->
                        ( "an unspecified type"
                        , D.reflow
                            ("But type variables like `" ++ name ++ "` cannot flow through ports. I need to know exactly what type of data I am getting, so I can guarantee that unexpected data cannot sneak in and crash the Elm program.")
                        )

                    CREC_UnsupportedType name ->
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

        CREC_PortTypeInvalid region name portProblem ->
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
                    CREC_CmdNoArg ->
                        ( "The `" ++ name ++ "` port cannot be just a command."
                        , D.reflow
                            "It can be (() -> Cmd msg) if you just need to trigger a JavaScript function, but there is often a better way to set things up."
                        )

                    CREC_CmdExtraArgs n ->
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

                    CREC_CmdBadMsg ->
                        ( "The `" ++ name ++ "` port cannot send any messages to the `update` function."
                        , D.reflow
                            "It must produce a (Cmd msg) type. Notice the lower case `msg` type variable. The command will trigger some JS code, but it will not send anything particular back to Elm."
                        )

                    CREC_SubBad ->
                        ( "There is something off about this `" ++ name ++ "` port declaration."
                        , D.stack
                            [ D.reflow
                                "To receive messages from JavaScript, you need to define a port like this:"
                            , D.indent 4 <| D.dullyellow <| D.fromChars <| "port " ++ name ++ " : (Int -> msg) -> Sub msg"
                            , D.reflow
                                "Now every time JS sends an `Int` to this port, it is converted to a `msg`. And if you subscribe, those `msg` values will be piped into your `update` function. The only thing you can customize here is the `Int` type."
                            ]
                        )

                    CREC_NotCmdOrSub ->
                        ( "I am confused about the `" ++ name ++ "` port declaration."
                        , D.reflow
                            "Ports need to produce a command (Cmd) or a subscription (Sub) but this is neither. I do not know how to handle this."
                        )

        CREC_RecursiveAlias region name args tipe others ->
            aliasRecursionReport source region name args tipe others

        CREC_RecursiveDecl region name names ->
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

        CREC_RecursiveLet (T.CRA_At region name) names ->
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

        CREC_Shadowing name r1 r2 ->
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

        CREC_TupleLargerThanThree region ->
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

        CREC_TypeVarsUnboundInUnion unionRegion typeName allVars unbound unbounds ->
            unboundTypeVars source unionRegion [ D.fromChars "type" ] typeName allVars unbound unbounds

        CREC_TypeVarsMessedUpInAlias aliasRegion typeName allVars unusedVars unboundVars ->
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


notFound : Code.Source -> T.CRA_Region -> Maybe T.CDN_Name -> T.CDN_Name -> String -> CREC_PossibleNames -> Report.Report
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



-- ENCODERS and DECODERS


errorEncoder : CREC_Error -> Encode.Value
errorEncoder error =
    case error of
        CREC_AnnotationTooShort region name index leftovers ->
            Encode.object
                [ ( "type", Encode.string "AnnotationTooShort" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                , ( "index", Index.zeroBasedEncoder index )
                , ( "leftovers", Encode.int leftovers )
                ]

        CREC_AmbiguousVar region maybePrefix name h hs ->
            Encode.object
                [ ( "type", Encode.string "AmbiguousVar" )
                , ( "region", A.regionEncoder region )
                , ( "maybePrefix", EncodeX.maybe Encode.string maybePrefix )
                , ( "name", Encode.string name )
                , ( "h", ModuleName.canonicalEncoder h )
                , ( "hs", EncodeX.oneOrMore ModuleName.canonicalEncoder hs )
                ]

        CREC_AmbiguousType region maybePrefix name h hs ->
            Encode.object
                [ ( "type", Encode.string "AmbiguousType" )
                , ( "region", A.regionEncoder region )
                , ( "maybePrefix", EncodeX.maybe Encode.string maybePrefix )
                , ( "name", Encode.string name )
                , ( "h", ModuleName.canonicalEncoder h )
                , ( "hs", EncodeX.oneOrMore ModuleName.canonicalEncoder hs )
                ]

        CREC_AmbiguousVariant region maybePrefix name h hs ->
            Encode.object
                [ ( "type", Encode.string "AmbiguousVariant" )
                , ( "region", A.regionEncoder region )
                , ( "maybePrefix", EncodeX.maybe Encode.string maybePrefix )
                , ( "name", Encode.string name )
                , ( "h", ModuleName.canonicalEncoder h )
                , ( "hs", EncodeX.oneOrMore ModuleName.canonicalEncoder hs )
                ]

        CREC_AmbiguousBinop region name h hs ->
            Encode.object
                [ ( "type", Encode.string "AmbiguousBinop" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                , ( "h", ModuleName.canonicalEncoder h )
                , ( "hs", EncodeX.oneOrMore ModuleName.canonicalEncoder hs )
                ]

        CREC_BadArity region badArityContext name expected actual ->
            Encode.object
                [ ( "type", Encode.string "BadArity" )
                , ( "region", A.regionEncoder region )
                , ( "badArityContext", badArityContextEncoder badArityContext )
                , ( "name", Encode.string name )
                , ( "expected", Encode.int expected )
                , ( "actual", Encode.int actual )
                ]

        CREC_Binop region op1 op2 ->
            Encode.object
                [ ( "type", Encode.string "Binop" )
                , ( "region", A.regionEncoder region )
                , ( "op1", Encode.string op1 )
                , ( "op2", Encode.string op2 )
                ]

        CREC_DuplicateDecl name r1 r2 ->
            Encode.object
                [ ( "type", Encode.string "DuplicateDecl" )
                , ( "name", Encode.string name )
                , ( "r1", A.regionEncoder r1 )
                , ( "r2", A.regionEncoder r2 )
                ]

        CREC_DuplicateType name r1 r2 ->
            Encode.object
                [ ( "type", Encode.string "DuplicateType" )
                , ( "name", Encode.string name )
                , ( "r1", A.regionEncoder r1 )
                , ( "r2", A.regionEncoder r2 )
                ]

        CREC_DuplicateCtor name r1 r2 ->
            Encode.object
                [ ( "type", Encode.string "DuplicateCtor" )
                , ( "name", Encode.string name )
                , ( "r1", A.regionEncoder r1 )
                , ( "r2", A.regionEncoder r2 )
                ]

        CREC_DuplicateBinop name r1 r2 ->
            Encode.object
                [ ( "type", Encode.string "DuplicateBinop" )
                , ( "name", Encode.string name )
                , ( "r1", A.regionEncoder r1 )
                , ( "r2", A.regionEncoder r2 )
                ]

        CREC_DuplicateField name r1 r2 ->
            Encode.object
                [ ( "type", Encode.string "DuplicateField" )
                , ( "name", Encode.string name )
                , ( "r1", A.regionEncoder r1 )
                , ( "r2", A.regionEncoder r2 )
                ]

        CREC_DuplicateAliasArg typeName name r1 r2 ->
            Encode.object
                [ ( "type", Encode.string "DuplicateAliasArg" )
                , ( "typeName", Encode.string typeName )
                , ( "name", Encode.string name )
                , ( "r1", A.regionEncoder r1 )
                , ( "r2", A.regionEncoder r2 )
                ]

        CREC_DuplicateUnionArg typeName name r1 r2 ->
            Encode.object
                [ ( "type", Encode.string "DuplicateUnionArg" )
                , ( "typeName", Encode.string typeName )
                , ( "name", Encode.string name )
                , ( "r1", A.regionEncoder r1 )
                , ( "r2", A.regionEncoder r2 )
                ]

        CREC_DuplicatePattern context name r1 r2 ->
            Encode.object
                [ ( "type", Encode.string "DuplicatePattern" )
                , ( "context", duplicatePatternContextEncoder context )
                , ( "name", Encode.string name )
                , ( "r1", A.regionEncoder r1 )
                , ( "r2", A.regionEncoder r2 )
                ]

        CREC_EffectNotFound region name ->
            Encode.object
                [ ( "type", Encode.string "EffectNotFound" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                ]

        CREC_EffectFunctionNotFound region name ->
            Encode.object
                [ ( "type", Encode.string "EffectFunctionNotFound" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                ]

        CREC_ExportDuplicate name r1 r2 ->
            Encode.object
                [ ( "type", Encode.string "ExportDuplicate" )
                , ( "name", Encode.string name )
                , ( "r1", A.regionEncoder r1 )
                , ( "r2", A.regionEncoder r2 )
                ]

        CREC_ExportNotFound region kind rawName possibleNames ->
            Encode.object
                [ ( "type", Encode.string "ExportNotFound" )
                , ( "region", A.regionEncoder region )
                , ( "kind", varKindEncoder kind )
                , ( "rawName", Encode.string rawName )
                , ( "possibleNames", Encode.list Encode.string possibleNames )
                ]

        CREC_ExportOpenAlias region name ->
            Encode.object
                [ ( "type", Encode.string "ExportOpenAlias" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                ]

        CREC_ImportCtorByName region ctor tipe ->
            Encode.object
                [ ( "type", Encode.string "ImportCtorByName" )
                , ( "region", A.regionEncoder region )
                , ( "ctor", Encode.string ctor )
                , ( "tipe", Encode.string tipe )
                ]

        CREC_ImportNotFound region name suggestions ->
            Encode.object
                [ ( "type", Encode.string "ImportNotFound" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                , ( "suggestions", Encode.list ModuleName.canonicalEncoder suggestions )
                ]

        CREC_ImportOpenAlias region name ->
            Encode.object
                [ ( "type", Encode.string "ImportOpenAlias" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                ]

        CREC_ImportExposingNotFound region home value possibleNames ->
            Encode.object
                [ ( "type", Encode.string "ImportExposingNotFound" )
                , ( "region", A.regionEncoder region )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "value", Encode.string value )
                , ( "possibleNames", Encode.list Encode.string possibleNames )
                ]

        CREC_NotFoundVar region prefix name possibleNames ->
            Encode.object
                [ ( "type", Encode.string "NotFoundVar" )
                , ( "region", A.regionEncoder region )
                , ( "prefix", EncodeX.maybe Encode.string prefix )
                , ( "name", Encode.string name )
                , ( "possibleNames", possibleNamesEncoder possibleNames )
                ]

        CREC_NotFoundType region prefix name possibleNames ->
            Encode.object
                [ ( "type", Encode.string "NotFoundType" )
                , ( "region", A.regionEncoder region )
                , ( "prefix", EncodeX.maybe Encode.string prefix )
                , ( "name", Encode.string name )
                , ( "possibleNames", possibleNamesEncoder possibleNames )
                ]

        CREC_NotFoundVariant region prefix name possibleNames ->
            Encode.object
                [ ( "type", Encode.string "NotFoundVariant" )
                , ( "region", A.regionEncoder region )
                , ( "prefix", EncodeX.maybe Encode.string prefix )
                , ( "name", Encode.string name )
                , ( "possibleNames", possibleNamesEncoder possibleNames )
                ]

        CREC_NotFoundBinop region op locals ->
            Encode.object
                [ ( "type", Encode.string "NotFoundBinop" )
                , ( "region", A.regionEncoder region )
                , ( "op", Encode.string op )
                , ( "locals", EncodeX.everySet compare Encode.string locals )
                ]

        CREC_PatternHasRecordCtor region name ->
            Encode.object
                [ ( "type", Encode.string "PatternHasRecordCtor" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                ]

        CREC_PortPayloadInvalid region portName badType invalidPayload ->
            Encode.object
                [ ( "type", Encode.string "PortPayloadInvalid" )
                , ( "region", A.regionEncoder region )
                , ( "portName", Encode.string portName )
                , ( "badType", Can.typeEncoder badType )
                , ( "invalidPayload", invalidPayloadEncoder invalidPayload )
                ]

        CREC_PortTypeInvalid region name portProblem ->
            Encode.object
                [ ( "type", Encode.string "PortTypeInvalid" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                , ( "portProblem", portProblemEncoder portProblem )
                ]

        CREC_RecursiveAlias region name args tipe others ->
            Encode.object
                [ ( "type", Encode.string "RecursiveAlias" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                , ( "args", Encode.list Encode.string args )
                , ( "tipe", Src.typeEncoder tipe )
                , ( "others", Encode.list Encode.string others )
                ]

        CREC_RecursiveDecl region name names ->
            Encode.object
                [ ( "type", Encode.string "RecursiveDecl" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                , ( "names", Encode.list Encode.string names )
                ]

        CREC_RecursiveLet name names ->
            Encode.object
                [ ( "type", Encode.string "RecursiveLet" )
                , ( "name", A.locatedEncoder Encode.string name )
                , ( "names", Encode.list Encode.string names )
                ]

        CREC_Shadowing name r1 r2 ->
            Encode.object
                [ ( "type", Encode.string "Shadowing" )
                , ( "name", Encode.string name )
                , ( "r1", A.regionEncoder r1 )
                , ( "r2", A.regionEncoder r2 )
                ]

        CREC_TupleLargerThanThree region ->
            Encode.object
                [ ( "type", Encode.string "TupleLargerThanThree" )
                , ( "region", A.regionEncoder region )
                ]

        CREC_TypeVarsUnboundInUnion unionRegion typeName allVars unbound unbounds ->
            Encode.object
                [ ( "type", Encode.string "TypeVarsUnboundInUnion" )
                , ( "unionRegion", A.regionEncoder unionRegion )
                , ( "typeName", Encode.string typeName )
                , ( "allVars", Encode.list Encode.string allVars )
                , ( "unbound", EncodeX.jsonPair Encode.string A.regionEncoder unbound )
                , ( "unbounds", Encode.list (EncodeX.jsonPair Encode.string A.regionEncoder) unbounds )
                ]

        CREC_TypeVarsMessedUpInAlias aliasRegion typeName allVars unusedVars unboundVars ->
            Encode.object
                [ ( "type", Encode.string "TypeVarsMessedUpInAlias" )
                , ( "aliasRegion", A.regionEncoder aliasRegion )
                , ( "typeName", Encode.string typeName )
                , ( "allVars", Encode.list Encode.string allVars )
                , ( "unusedVars", Encode.list (EncodeX.jsonPair Encode.string A.regionEncoder) unusedVars )
                , ( "unboundVars", Encode.list (EncodeX.jsonPair Encode.string A.regionEncoder) unboundVars )
                ]


errorDecoder : Decode.Decoder CREC_Error
errorDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "AnnotationTooShort" ->
                        Decode.map4 CREC_AnnotationTooShort
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "index" Index.zeroBasedDecoder)
                            (Decode.field "leftovers" Decode.int)

                    "AmbiguousVar" ->
                        Decode.map5 CREC_AmbiguousVar
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "maybePrefix" (Decode.maybe Decode.string))
                            (Decode.field "name" Decode.string)
                            (Decode.field "h" ModuleName.canonicalDecoder)
                            (Decode.field "hs" (DecodeX.oneOrMore ModuleName.canonicalDecoder))

                    "AmbiguousType" ->
                        Decode.map5 CREC_AmbiguousType
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "maybePrefix" (Decode.maybe Decode.string))
                            (Decode.field "name" Decode.string)
                            (Decode.field "h" ModuleName.canonicalDecoder)
                            (Decode.field "hs" (DecodeX.oneOrMore ModuleName.canonicalDecoder))

                    "AmbiguousVariant" ->
                        Decode.map5 CREC_AmbiguousVariant
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "maybePrefix" (Decode.maybe Decode.string))
                            (Decode.field "name" Decode.string)
                            (Decode.field "h" ModuleName.canonicalDecoder)
                            (Decode.field "hs" (DecodeX.oneOrMore ModuleName.canonicalDecoder))

                    "AmbiguousBinop" ->
                        Decode.map4 CREC_AmbiguousBinop
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "h" ModuleName.canonicalDecoder)
                            (Decode.field "hs" (DecodeX.oneOrMore ModuleName.canonicalDecoder))

                    "BadArity" ->
                        Decode.map5 CREC_BadArity
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "badArityContext" badArityContextDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "expected" Decode.int)
                            (Decode.field "actual" Decode.int)

                    "Binop" ->
                        Decode.map3 CREC_Binop
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "op1" Decode.string)
                            (Decode.field "op2" Decode.string)

                    "DuplicateDecl" ->
                        Decode.map3 CREC_DuplicateDecl
                            (Decode.field "name" Decode.string)
                            (Decode.field "r1" A.regionDecoder)
                            (Decode.field "r2" A.regionDecoder)

                    "DuplicateType" ->
                        Decode.map3 CREC_DuplicateType
                            (Decode.field "name" Decode.string)
                            (Decode.field "r1" A.regionDecoder)
                            (Decode.field "r2" A.regionDecoder)

                    "DuplicateCtor" ->
                        Decode.map3 CREC_DuplicateCtor
                            (Decode.field "name" Decode.string)
                            (Decode.field "r1" A.regionDecoder)
                            (Decode.field "r2" A.regionDecoder)

                    "DuplicateBinop" ->
                        Decode.map3 CREC_DuplicateBinop
                            (Decode.field "name" Decode.string)
                            (Decode.field "r1" A.regionDecoder)
                            (Decode.field "r2" A.regionDecoder)

                    "DuplicateField" ->
                        Decode.map3 CREC_DuplicateField
                            (Decode.field "name" Decode.string)
                            (Decode.field "r1" A.regionDecoder)
                            (Decode.field "r2" A.regionDecoder)

                    "DuplicateAliasArg" ->
                        Decode.map4 CREC_DuplicateAliasArg
                            (Decode.field "typeName" Decode.string)
                            (Decode.field "name" Decode.string)
                            (Decode.field "r1" A.regionDecoder)
                            (Decode.field "r2" A.regionDecoder)

                    "DuplicateUnionArg" ->
                        Decode.map4 CREC_DuplicateUnionArg
                            (Decode.field "typeName" Decode.string)
                            (Decode.field "name" Decode.string)
                            (Decode.field "r1" A.regionDecoder)
                            (Decode.field "r2" A.regionDecoder)

                    "DuplicatePattern" ->
                        Decode.map4 CREC_DuplicatePattern
                            (Decode.field "context" duplicatePatternContextDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "r1" A.regionDecoder)
                            (Decode.field "r2" A.regionDecoder)

                    "EffectNotFound" ->
                        Decode.map2 CREC_EffectNotFound
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)

                    "EffectFunctionNotFound" ->
                        Decode.map2 CREC_EffectFunctionNotFound
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)

                    "ExportDuplicate" ->
                        Decode.map3 CREC_ExportDuplicate
                            (Decode.field "name" Decode.string)
                            (Decode.field "r1" A.regionDecoder)
                            (Decode.field "r2" A.regionDecoder)

                    "ExportNotFound" ->
                        Decode.map4 CREC_ExportNotFound
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "kind" varKindDecoder)
                            (Decode.field "rawName" Decode.string)
                            (Decode.field "possibleNames" (Decode.list Decode.string))

                    "ExportOpenAlias" ->
                        Decode.map2 CREC_ExportOpenAlias
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)

                    "ImportCtorByName" ->
                        Decode.map3 CREC_ImportCtorByName
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "ctor" Decode.string)
                            (Decode.field "tipe" Decode.string)

                    "ImportNotFound" ->
                        Decode.map3 CREC_ImportNotFound
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "suggestions" (Decode.list ModuleName.canonicalDecoder))

                    "ImportOpenAlias" ->
                        Decode.map2 CREC_ImportOpenAlias
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)

                    "ImportExposingNotFound" ->
                        Decode.map4 CREC_ImportExposingNotFound
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "value" Decode.string)
                            (Decode.field "possibleNames" (Decode.list Decode.string))

                    "NotFoundVar" ->
                        Decode.map4 CREC_NotFoundVar
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "prefix" (Decode.maybe Decode.string))
                            (Decode.field "name" Decode.string)
                            (Decode.field "possibleNames" possibleNamesDecoder)

                    "NotFoundType" ->
                        Decode.map4 CREC_NotFoundType
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "prefix" (Decode.maybe Decode.string))
                            (Decode.field "name" Decode.string)
                            (Decode.field "possibleNames" possibleNamesDecoder)

                    "NotFoundVariant" ->
                        Decode.map4 CREC_NotFoundVariant
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "prefix" (Decode.maybe Decode.string))
                            (Decode.field "name" Decode.string)
                            (Decode.field "possibleNames" possibleNamesDecoder)

                    "NotFoundBinop" ->
                        Decode.map3 CREC_NotFoundBinop
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "op" Decode.string)
                            (Decode.field "locals" (DecodeX.everySet identity Decode.string))

                    "PatternHasRecordCtor" ->
                        Decode.map2 CREC_PatternHasRecordCtor
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)

                    "PortPayloadInvalid" ->
                        Decode.map4 CREC_PortPayloadInvalid
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "portName" Decode.string)
                            (Decode.field "badType" Can.typeDecoder)
                            (Decode.field "invalidPayload" invalidPayloadDecoder)

                    "PortTypeInvalid" ->
                        Decode.map3 CREC_PortTypeInvalid
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "portProblem" portProblemDecoder)

                    "RecursiveAlias" ->
                        Decode.map5 CREC_RecursiveAlias
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list Decode.string))
                            (Decode.field "tipe" Src.typeDecoder)
                            (Decode.field "others" (Decode.list Decode.string))

                    "RecursiveDecl" ->
                        Decode.map3 CREC_RecursiveDecl
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "names" (Decode.list Decode.string))

                    "RecursiveLet" ->
                        Decode.map2 CREC_RecursiveLet
                            (Decode.field "name" (A.locatedDecoder Decode.string))
                            (Decode.field "names" (Decode.list Decode.string))

                    "Shadowing" ->
                        Decode.map3 CREC_Shadowing
                            (Decode.field "name" Decode.string)
                            (Decode.field "r1" A.regionDecoder)
                            (Decode.field "r2" A.regionDecoder)

                    "TupleLargerThanThree" ->
                        Decode.map CREC_TupleLargerThanThree (Decode.field "region" A.regionDecoder)

                    "TypeVarsUnboundInUnion" ->
                        Decode.map5 CREC_TypeVarsUnboundInUnion
                            (Decode.field "unionRegion" A.regionDecoder)
                            (Decode.field "typeName" Decode.string)
                            (Decode.field "allVars" (Decode.list Decode.string))
                            (Decode.field "unbound" (DecodeX.jsonPair Decode.string A.regionDecoder))
                            (Decode.field "unbounds" (Decode.list (DecodeX.jsonPair Decode.string A.regionDecoder)))

                    "TypeVarsMessedUpInAlias" ->
                        Decode.map5 CREC_TypeVarsMessedUpInAlias
                            (Decode.field "aliasRegion" A.regionDecoder)
                            (Decode.field "typeName" Decode.string)
                            (Decode.field "allVars" (Decode.list Decode.string))
                            (Decode.field "unusedVars" (Decode.list (DecodeX.jsonPair Decode.string A.regionDecoder)))
                            (Decode.field "unboundVars" (Decode.list (DecodeX.jsonPair Decode.string A.regionDecoder)))

                    _ ->
                        Decode.fail ("Failed to decode Error's type: " ++ type_)
            )


badArityContextEncoder : CREC_BadArityContext -> Encode.Value
badArityContextEncoder badArityContext =
    case badArityContext of
        CREC_TypeArity ->
            Encode.string "TypeArity"

        CREC_PatternArity ->
            Encode.string "PatternArity"


badArityContextDecoder : Decode.Decoder CREC_BadArityContext
badArityContextDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "TypeArity" ->
                        Decode.succeed CREC_TypeArity

                    "PatternArity" ->
                        Decode.succeed CREC_PatternArity

                    _ ->
                        Decode.fail ("Unknown BadArityContext: " ++ str)
            )


duplicatePatternContextEncoder : CREC_DuplicatePatternContext -> Encode.Value
duplicatePatternContextEncoder duplicatePatternContext =
    case duplicatePatternContext of
        CREC_DPLambdaArgs ->
            Encode.object
                [ ( "type", Encode.string "DPLambdaArgs" )
                ]

        CREC_DPFuncArgs funcName ->
            Encode.object
                [ ( "type", Encode.string "DPFuncArgs" )
                , ( "funcName", Encode.string funcName )
                ]

        CREC_DPCaseBranch ->
            Encode.object
                [ ( "type", Encode.string "DPCaseBranch" )
                ]

        CREC_DPLetBinding ->
            Encode.object
                [ ( "type", Encode.string "DPLetBinding" )
                ]

        CREC_DPDestruct ->
            Encode.object
                [ ( "type", Encode.string "DPDestruct" )
                ]


duplicatePatternContextDecoder : Decode.Decoder CREC_DuplicatePatternContext
duplicatePatternContextDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "DPLambdaArgs" ->
                        Decode.succeed CREC_DPLambdaArgs

                    "DPFuncArgs" ->
                        Decode.map CREC_DPFuncArgs (Decode.field "funcName" Decode.string)

                    "DPCaseBranch" ->
                        Decode.succeed CREC_DPCaseBranch

                    "DPLetBinding" ->
                        Decode.succeed CREC_DPLetBinding

                    "DPDestruct" ->
                        Decode.succeed CREC_DPDestruct

                    _ ->
                        Decode.fail ("Failed to decode DuplicatePatternContext's type: " ++ type_)
            )


varKindEncoder : CREC_VarKind -> Encode.Value
varKindEncoder varKind =
    case varKind of
        CREC_BadOp ->
            Encode.string "BadOp"

        CREC_BadVar ->
            Encode.string "BadVar"

        CREC_BadPattern ->
            Encode.string "BadPattern"

        CREC_BadType ->
            Encode.string "BadType"


varKindDecoder : Decode.Decoder CREC_VarKind
varKindDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "BadOp" ->
                        Decode.succeed CREC_BadOp

                    "BadVar" ->
                        Decode.succeed CREC_BadVar

                    "BadPattern" ->
                        Decode.succeed CREC_BadPattern

                    "BadType" ->
                        Decode.succeed CREC_BadType

                    _ ->
                        Decode.fail ("Unknown VarKind: " ++ str)
            )


possibleNamesEncoder : CREC_PossibleNames -> Encode.Value
possibleNamesEncoder possibleNames =
    Encode.object
        [ ( "type", Encode.string "PossibleNames" )
        , ( "locals", EncodeX.everySet compare Encode.string possibleNames.locals )
        , ( "quals", EncodeX.assocListDict compare Encode.string (EncodeX.everySet compare Encode.string) possibleNames.quals )
        ]


possibleNamesDecoder : Decode.Decoder CREC_PossibleNames
possibleNamesDecoder =
    Decode.map2 CREC_PossibleNames
        (Decode.field "locals" (DecodeX.everySet identity Decode.string))
        (Decode.field "quals" (DecodeX.assocListDict identity Decode.string (DecodeX.everySet identity Decode.string)))


invalidPayloadEncoder : CREC_InvalidPayload -> Encode.Value
invalidPayloadEncoder invalidPayload =
    case invalidPayload of
        CREC_ExtendedRecord ->
            Encode.object
                [ ( "type", Encode.string "ExtendedRecord" )
                ]

        CREC_Function ->
            Encode.object
                [ ( "type", Encode.string "Function" )
                ]

        CREC_TypeVariable name ->
            Encode.object
                [ ( "type", Encode.string "TypeVariable" )
                , ( "name", Encode.string name )
                ]

        CREC_UnsupportedType name ->
            Encode.object
                [ ( "type", Encode.string "UnsupportedType" )
                , ( "name", Encode.string name )
                ]


invalidPayloadDecoder : Decode.Decoder CREC_InvalidPayload
invalidPayloadDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "ExtendedRecord" ->
                        Decode.succeed CREC_ExtendedRecord

                    "Function" ->
                        Decode.succeed CREC_Function

                    "TypeVariable" ->
                        Decode.map CREC_TypeVariable (Decode.field "name" Decode.string)

                    "UnsupportedType" ->
                        Decode.map CREC_UnsupportedType (Decode.field "name" Decode.string)

                    _ ->
                        Decode.fail ("Failed to decode InvalidPayload's type: " ++ type_)
            )


portProblemEncoder : CREC_PortProblem -> Encode.Value
portProblemEncoder portProblem =
    case portProblem of
        CREC_CmdNoArg ->
            Encode.object
                [ ( "type", Encode.string "CmdNoArg" )
                ]

        CREC_CmdExtraArgs n ->
            Encode.object
                [ ( "type", Encode.string "CmdExtraArgs" )
                , ( "n", Encode.int n )
                ]

        CREC_CmdBadMsg ->
            Encode.object
                [ ( "type", Encode.string "CmdBadMsg" )
                ]

        CREC_SubBad ->
            Encode.object
                [ ( "type", Encode.string "SubBad" )
                ]

        CREC_NotCmdOrSub ->
            Encode.object
                [ ( "type", Encode.string "NotCmdOrSub" )
                ]


portProblemDecoder : Decode.Decoder CREC_PortProblem
portProblemDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "CmdNoArg" ->
                        Decode.succeed CREC_CmdNoArg

                    "CmdExtraArgs" ->
                        Decode.map CREC_CmdExtraArgs (Decode.field "n" Decode.int)

                    "CmdBadMsg" ->
                        Decode.succeed CREC_CmdBadMsg

                    "SubBad" ->
                        Decode.succeed CREC_SubBad

                    "NotCmdOrSub" ->
                        Decode.succeed CREC_NotCmdOrSub

                    _ ->
                        Decode.fail ("Failed to decode PortProblem's type: " ++ type_)
            )
