module Compiler.Reporting.Error.Canonicalize exposing
    ( BadArityContext(..)
    , DuplicatePatternContext(..)
    , Error(..)
    , InvalidPayload(..)
    , PortProblem(..)
    , PossibleNames
    , VarKind(..)
    , errorDecoder
    , errorEncoder
    , invalidPayloadDecoder
    , invalidPayloadEncoder
    , toReport
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Data.OneOrMore as OneOrMore exposing (OneOrMore)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Report as Report
import Compiler.Reporting.Suggest as Suggest
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import System.TypeCheck.IO as IO
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



-- CANONICALIZATION ERRORS


type Error
    = AnnotationTooShort A.Region Name Index.ZeroBased Int
    | AmbiguousVar A.Region (Maybe Name) Name IO.Canonical (OneOrMore IO.Canonical)
    | AmbiguousType A.Region (Maybe Name) Name IO.Canonical (OneOrMore IO.Canonical)
    | AmbiguousVariant A.Region (Maybe Name) Name IO.Canonical (OneOrMore IO.Canonical)
    | AmbiguousBinop A.Region Name IO.Canonical (OneOrMore IO.Canonical)
    | BadArity A.Region BadArityContext Name Int Int
    | Binop A.Region Name Name
    | DuplicateDecl Name A.Region A.Region
    | DuplicateType Name A.Region A.Region
    | DuplicateCtor Name A.Region A.Region
    | DuplicateBinop Name A.Region A.Region
    | DuplicateField Name A.Region A.Region
    | DuplicateAliasArg Name Name A.Region A.Region
    | DuplicateUnionArg Name Name A.Region A.Region
    | DuplicatePattern DuplicatePatternContext Name A.Region A.Region
    | EffectNotFound A.Region Name
    | EffectFunctionNotFound A.Region Name
    | ExportDuplicate Name A.Region A.Region
    | ExportNotFound A.Region VarKind Name (List Name)
    | ExportOpenAlias A.Region Name
    | ImportCtorByName A.Region Name Name
    | ImportNotFound A.Region Name (List IO.Canonical)
    | ImportOpenAlias A.Region Name
    | ImportExposingNotFound A.Region IO.Canonical Name (List Name)
    | NotFoundVar A.Region (Maybe Name) Name PossibleNames
    | NotFoundType A.Region (Maybe Name) Name PossibleNames
    | NotFoundVariant A.Region (Maybe Name) Name PossibleNames
    | NotFoundBinop A.Region Name (EverySet String Name)
    | PatternHasRecordCtor A.Region Name
    | PortPayloadInvalid A.Region Name Can.Type InvalidPayload
    | PortTypeInvalid A.Region Name PortProblem
    | RecursiveAlias A.Region Name (List Name) Src.Type (List Name)
    | RecursiveDecl A.Region Name (List Name)
    | RecursiveLet (A.Located Name) (List Name)
    | Shadowing Name A.Region A.Region
    | TupleLargerThanThree A.Region
    | TypeVarsUnboundInUnion A.Region Name (List Name) ( Name, A.Region ) (List ( Name, A.Region ))
    | TypeVarsMessedUpInAlias A.Region Name (List Name) (List ( Name, A.Region )) (List ( Name, A.Region ))


type BadArityContext
    = TypeArity
    | PatternArity


type DuplicatePatternContext
    = DPLambdaArgs
    | DPFuncArgs Name
    | DPCaseBranch
    | DPLetBinding
    | DPDestruct


type InvalidPayload
    = ExtendedRecord
    | Function
    | TypeVariable Name
    | UnsupportedType Name


type PortProblem
    = CmdNoArg
    | CmdExtraArgs Int
    | CmdBadMsg
    | SubBad
    | NotCmdOrSub


type alias PossibleNames =
    { locals : EverySet String Name
    , quals : Dict String Name (EverySet String Name)
    }



-- KIND


type VarKind
    = BadOp
    | BadVar
    | BadPattern
    | BadType


toKindInfo : VarKind -> Name -> ( D.Doc, D.Doc, D.Doc )
toKindInfo kind name =
    case kind of
        BadOp ->
            ( D.fromChars "an"
            , D.fromChars "operator"
            , D.fromChars "("
                |> D.a (D.fromName name)
                |> D.a (D.fromChars ")")
            )

        BadVar ->
            ( D.fromChars "a"
            , D.fromChars "value"
            , D.fromChars "`"
                |> D.a (D.fromName name)
                |> D.a (D.fromChars "`")
            )

        BadPattern ->
            ( D.fromChars "a"
            , D.fromChars "pattern"
            , D.fromChars "`"
                |> D.a (D.fromName name)
                |> D.a (D.fromChars "`")
            )

        BadType ->
            ( D.fromChars "a"
            , D.fromChars "type"
            , D.fromChars "`"
                |> D.a (D.fromName name)
                |> D.a (D.fromChars "`")
            )



-- TO REPORT


toReport : Code.Source -> Error -> Report.Report
toReport source err =
    case err of
        AnnotationTooShort region name index leftovers ->
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

        AmbiguousVar region maybePrefix name h hs ->
            ambiguousName source region maybePrefix name h hs "variable"

        AmbiguousType region maybePrefix name h hs ->
            ambiguousName source region maybePrefix name h hs "type"

        AmbiguousVariant region maybePrefix name h hs ->
            ambiguousName source region maybePrefix name h hs "variant"

        AmbiguousBinop region name h hs ->
            ambiguousName source region Nothing name h hs "operator"

        BadArity region badArityContext name expected actual ->
            let
                thing : String
                thing =
                    case badArityContext of
                        TypeArity ->
                            "type"

                        PatternArity ->
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

        Binop region op1 op2 ->
            Report.Report "INFIX PROBLEM" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("You cannot mix (" ++ op1 ++ ") and (" ++ op2 ++ ") without parentheses.")
                    , D.reflow
                        "I do not know how to group these expressions. Add parentheses for me!"
                    )

        DuplicateDecl name r1 r2 ->
            nameClash source r1 r2 <|
                "This file has multiple `"
                    ++ name
                    ++ "` declarations."

        DuplicateType name r1 r2 ->
            nameClash source r1 r2 <|
                "This file defines multiple `"
                    ++ name
                    ++ "` types."

        DuplicateCtor name r1 r2 ->
            nameClash source r1 r2 <|
                "This file defines multiple `"
                    ++ name
                    ++ "` type constructors."

        DuplicateBinop name r1 r2 ->
            nameClash source r1 r2 <|
                "This file defines multiple ("
                    ++ name
                    ++ ") operators."

        DuplicateField name r1 r2 ->
            nameClash source r1 r2 <|
                "This record has multiple `"
                    ++ name
                    ++ "` fields."

        DuplicateAliasArg typeName name r1 r2 ->
            nameClash source r1 r2 <|
                "The `"
                    ++ typeName
                    ++ "` type alias has multiple `"
                    ++ name
                    ++ "` type variables."

        DuplicateUnionArg typeName name r1 r2 ->
            nameClash source r1 r2 <|
                "The `"
                    ++ typeName
                    ++ "` type has multiple `"
                    ++ name
                    ++ "` type variables."

        DuplicatePattern context name r1 r2 ->
            nameClash source r1 r2 <|
                case context of
                    DPLambdaArgs ->
                        "This anonymous function has multiple `" ++ name ++ "` arguments."

                    DPFuncArgs funcName ->
                        "The `" ++ funcName ++ "` function has multiple `" ++ name ++ "` arguments."

                    DPCaseBranch ->
                        "This `case` pattern has multiple `" ++ name ++ "` variables."

                    DPLetBinding ->
                        "This `let` expression defines `" ++ name ++ "` more than once!"

                    DPDestruct ->
                        "This pattern contains multiple `" ++ name ++ "` variables."

        EffectNotFound region name ->
            Report.Report "EFFECT PROBLEM" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("You have declared that `" ++ name ++ "` is an effect type:")
                    , D.reflow
                        ("But I cannot find a custom type named `" ++ name ++ "` in this file!")
                    )

        EffectFunctionNotFound region name ->
            Report.Report "EFFECT PROBLEM" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("This kind of effect module must define a `" ++ name ++ "` function.")
                    , D.reflow
                        ("But I cannot find `" ++ name ++ "` in this file!")
                    )

        ExportDuplicate name r1 r2 ->
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

        ExportNotFound region kind rawName possibleNames ->
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

        ExportOpenAlias region name ->
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

        ImportCtorByName region ctor tipe ->
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

        ImportNotFound region name _ ->
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

        ImportOpenAlias region name ->
            Report.Report "BAD IMPORT" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow
                        ("The `" ++ name ++ "` type alias cannot be followed by (..) like this:")
                    , D.reflow
                        "Remove the (..) and it should work."
                    )

        ImportExposingNotFound region (IO.Canonical _ home) value possibleNames ->
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

        NotFoundVar region prefix name possibleNames ->
            notFound source region prefix name "variable" possibleNames

        NotFoundType region prefix name possibleNames ->
            notFound source region prefix name "type" possibleNames

        NotFoundVariant region prefix name possibleNames ->
            notFound source region prefix name "variant" possibleNames

        NotFoundBinop region op locals ->
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

        PatternHasRecordCtor region name ->
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

        PortPayloadInvalid region portName _ invalidPayload ->
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
                    ExtendedRecord ->
                        ( "an extended record"
                        , D.reflow
                            "But the exact shape of the record must be known at compile time. No type variables!"
                        )

                    Function ->
                        ( "a function"
                        , D.reflow
                            "But functions cannot be sent in and out ports. If we allowed functions in from JS they may perform some side-effects. If we let functions out, they could produce incorrect results because Elm optimizations assume there are no side-effects."
                        )

                    TypeVariable name ->
                        ( "an unspecified type"
                        , D.reflow
                            ("But type variables like `" ++ name ++ "` cannot flow through ports. I need to know exactly what type of data I am getting, so I can guarantee that unexpected data cannot sneak in and crash the Elm program.")
                        )

                    UnsupportedType name ->
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

        PortTypeInvalid region name portProblem ->
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
                    CmdNoArg ->
                        ( "The `" ++ name ++ "` port cannot be just a command."
                        , D.reflow
                            "It can be (() -> Cmd msg) if you just need to trigger a JavaScript function, but there is often a better way to set things up."
                        )

                    CmdExtraArgs n ->
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

                    CmdBadMsg ->
                        ( "The `" ++ name ++ "` port cannot send any messages to the `update` function."
                        , D.reflow
                            "It must produce a (Cmd msg) type. Notice the lower case `msg` type variable. The command will trigger some JS code, but it will not send anything particular back to Elm."
                        )

                    SubBad ->
                        ( "There is something off about this `" ++ name ++ "` port declaration."
                        , D.stack
                            [ D.reflow
                                "To receive messages from JavaScript, you need to define a port like this:"
                            , D.indent 4 <| D.dullyellow <| D.fromChars <| "port " ++ name ++ " : (Int -> msg) -> Sub msg"
                            , D.reflow
                                "Now every time JS sends an `Int` to this port, it is converted to a `msg`. And if you subscribe, those `msg` values will be piped into your `update` function. The only thing you can customize here is the `Int` type."
                            ]
                        )

                    NotCmdOrSub ->
                        ( "I am confused about the `" ++ name ++ "` port declaration."
                        , D.reflow
                            "Ports need to produce a command (Cmd) or a subscription (Sub) but this is neither. I do not know how to handle this."
                        )

        RecursiveAlias region name args tipe others ->
            aliasRecursionReport source region name args tipe others

        RecursiveDecl region name names ->
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

        RecursiveLet (A.At region name) names ->
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

        Shadowing name r1 r2 ->
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

        TupleLargerThanThree region ->
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

        TypeVarsUnboundInUnion unionRegion typeName allVars unbound unbounds ->
            unboundTypeVars source unionRegion [ D.fromChars "type" ] typeName allVars unbound unbounds

        TypeVarsMessedUpInAlias aliasRegion typeName allVars unusedVars unboundVars ->
            case ( unusedVars, unboundVars ) of
                ( unused :: unuseds, [] ) ->
                    let
                        backQuote : Name -> D.Doc
                        backQuote name =
                            D.fromChars "`"
                                |> D.a (D.fromName name)
                                |> D.a (D.fromChars "`")

                        allUnusedNames : List Name
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
                        unused : List Name
                        unused =
                            List.map Tuple.first unusedVars

                        unbound : List Name
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


unboundTypeVars : Code.Source -> A.Region -> List D.Doc -> Name.Name -> List Name.Name -> ( Name.Name, A.Region ) -> List ( Name.Name, A.Region ) -> Report.Report
unboundTypeVars source declRegion tipe typeName allVars ( unboundVar, varRegion ) unboundVars =
    let
        backQuote : Name -> D.Doc
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


nameClash : Code.Source -> A.Region -> A.Region -> String -> Report.Report
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


ambiguousName : Code.Source -> A.Region -> Maybe Name.Name -> Name.Name -> IO.Canonical -> OneOrMore.OneOrMore IO.Canonical -> String -> Report.Report
ambiguousName source region maybePrefix name h hs thing =
    let
        possibleHomes : List IO.Canonical
        possibleHomes =
            List.sortWith ModuleName.compareCanonical (h :: OneOrMore.destruct (::) hs)
    in
    Report.Report "AMBIGUOUS NAME" region [] <|
        Code.toSnippet source region Nothing <|
            case maybePrefix of
                Nothing ->
                    let
                        homeToYellowDoc : IO.Canonical -> D.Doc
                        homeToYellowDoc (IO.Canonical _ home) =
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
                        homeToYellowDoc : IO.Canonical -> D.Doc
                        homeToYellowDoc (IO.Canonical _ home) =
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


notFound : Code.Source -> A.Region -> Maybe Name.Name -> Name.Name -> String -> PossibleNames -> Report.Report
notFound source region maybePrefix name thing { locals, quals } =
    let
        givenName : Name
        givenName =
            Maybe.withDefault name (Maybe.map2 toQualString maybePrefix (Just name))

        possibleNames : List String
        possibleNames =
            let
                addQuals : Name -> EverySet String Name -> List String -> List String
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


toQualString : Name.Name -> Name.Name -> String
toQualString prefix name =
    prefix ++ "." ++ name



-- BAD ALIAS RECURSION


aliasRecursionReport : Code.Source -> A.Region -> Name -> List Name -> Src.Type -> List Name -> Report.Report
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


aliasToUnionDoc : Name -> List Name -> Src.Type -> D.Doc
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


errorEncoder : Error -> BE.Encoder
errorEncoder error =
    case error of
        AnnotationTooShort region name index leftovers ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.regionEncoder region
                , BE.string name
                , Index.zeroBasedEncoder index
                , BE.int leftovers
                ]

        AmbiguousVar region maybePrefix name h hs ->
            BE.sequence
                [ BE.unsignedInt8 1
                , A.regionEncoder region
                , BE.maybe BE.string maybePrefix
                , BE.string name
                , ModuleName.canonicalEncoder h
                , BE.oneOrMore ModuleName.canonicalEncoder hs
                ]

        AmbiguousType region maybePrefix name h hs ->
            BE.sequence
                [ BE.unsignedInt8 2
                , A.regionEncoder region
                , BE.maybe BE.string maybePrefix
                , BE.string name
                , ModuleName.canonicalEncoder h
                , BE.oneOrMore ModuleName.canonicalEncoder hs
                ]

        AmbiguousVariant region maybePrefix name h hs ->
            BE.sequence
                [ BE.unsignedInt8 3
                , A.regionEncoder region
                , BE.maybe BE.string maybePrefix
                , BE.string name
                , ModuleName.canonicalEncoder h
                , BE.oneOrMore ModuleName.canonicalEncoder hs
                ]

        AmbiguousBinop region name h hs ->
            BE.sequence
                [ BE.unsignedInt8 4
                , A.regionEncoder region
                , BE.string name
                , ModuleName.canonicalEncoder h
                , BE.oneOrMore ModuleName.canonicalEncoder hs
                ]

        BadArity region badArityContext name expected actual ->
            BE.sequence
                [ BE.unsignedInt8 5
                , A.regionEncoder region
                , badArityContextEncoder badArityContext
                , BE.string name
                , BE.int expected
                , BE.int actual
                ]

        Binop region op1 op2 ->
            BE.sequence
                [ BE.unsignedInt8 6
                , A.regionEncoder region
                , BE.string op1
                , BE.string op2
                ]

        DuplicateDecl name r1 r2 ->
            BE.sequence
                [ BE.unsignedInt8 7
                , BE.string name
                , A.regionEncoder r1
                , A.regionEncoder r2
                ]

        DuplicateType name r1 r2 ->
            BE.sequence
                [ BE.unsignedInt8 8
                , BE.string name
                , A.regionEncoder r1
                , A.regionEncoder r2
                ]

        DuplicateCtor name r1 r2 ->
            BE.sequence
                [ BE.unsignedInt8 9
                , BE.string name
                , A.regionEncoder r1
                , A.regionEncoder r2
                ]

        DuplicateBinop name r1 r2 ->
            BE.sequence
                [ BE.unsignedInt8 10
                , BE.string name
                , A.regionEncoder r1
                , A.regionEncoder r2
                ]

        DuplicateField name r1 r2 ->
            BE.sequence
                [ BE.unsignedInt8 11
                , BE.string name
                , A.regionEncoder r1
                , A.regionEncoder r2
                ]

        DuplicateAliasArg typeName name r1 r2 ->
            BE.sequence
                [ BE.unsignedInt8 12
                , BE.string typeName
                , BE.string name
                , A.regionEncoder r1
                , A.regionEncoder r2
                ]

        DuplicateUnionArg typeName name r1 r2 ->
            BE.sequence
                [ BE.unsignedInt8 13
                , BE.string typeName
                , BE.string name
                , A.regionEncoder r1
                , A.regionEncoder r2
                ]

        DuplicatePattern context name r1 r2 ->
            BE.sequence
                [ BE.unsignedInt8 14
                , duplicatePatternContextEncoder context
                , BE.string name
                , A.regionEncoder r1
                , A.regionEncoder r2
                ]

        EffectNotFound region name ->
            BE.sequence
                [ BE.unsignedInt8 15
                , A.regionEncoder region
                , BE.string name
                ]

        EffectFunctionNotFound region name ->
            BE.sequence
                [ BE.unsignedInt8 16
                , A.regionEncoder region
                , BE.string name
                ]

        ExportDuplicate name r1 r2 ->
            BE.sequence
                [ BE.unsignedInt8 17
                , BE.string name
                , A.regionEncoder r1
                , A.regionEncoder r2
                ]

        ExportNotFound region kind rawName possibleNames ->
            BE.sequence
                [ BE.unsignedInt8 18
                , A.regionEncoder region
                , varKindEncoder kind
                , BE.string rawName
                , BE.list BE.string possibleNames
                ]

        ExportOpenAlias region name ->
            BE.sequence
                [ BE.unsignedInt8 19
                , A.regionEncoder region
                , BE.string name
                ]

        ImportCtorByName region ctor tipe ->
            BE.sequence
                [ BE.unsignedInt8 20
                , A.regionEncoder region
                , BE.string ctor
                , BE.string tipe
                ]

        ImportNotFound region name suggestions ->
            BE.sequence
                [ BE.unsignedInt8 21
                , A.regionEncoder region
                , BE.string name
                , BE.list ModuleName.canonicalEncoder suggestions
                ]

        ImportOpenAlias region name ->
            BE.sequence
                [ BE.unsignedInt8 22
                , A.regionEncoder region
                , BE.string name
                ]

        ImportExposingNotFound region home value possibleNames ->
            BE.sequence
                [ BE.unsignedInt8 23
                , A.regionEncoder region
                , ModuleName.canonicalEncoder home
                , BE.string value
                , BE.list BE.string possibleNames
                ]

        NotFoundVar region prefix name possibleNames ->
            BE.sequence
                [ BE.unsignedInt8 24
                , A.regionEncoder region
                , BE.maybe BE.string prefix
                , BE.string name
                , possibleNamesEncoder possibleNames
                ]

        NotFoundType region prefix name possibleNames ->
            BE.sequence
                [ BE.unsignedInt8 25
                , A.regionEncoder region
                , BE.maybe BE.string prefix
                , BE.string name
                , possibleNamesEncoder possibleNames
                ]

        NotFoundVariant region prefix name possibleNames ->
            BE.sequence
                [ BE.unsignedInt8 26
                , A.regionEncoder region
                , BE.maybe BE.string prefix
                , BE.string name
                , possibleNamesEncoder possibleNames
                ]

        NotFoundBinop region op locals ->
            BE.sequence
                [ BE.unsignedInt8 27
                , A.regionEncoder region
                , BE.string op
                , BE.everySet compare BE.string locals
                ]

        PatternHasRecordCtor region name ->
            BE.sequence
                [ BE.unsignedInt8 28
                , A.regionEncoder region
                , BE.string name
                ]

        PortPayloadInvalid region portName badType invalidPayload ->
            BE.sequence
                [ BE.unsignedInt8 29
                , A.regionEncoder region
                , BE.string portName
                , Can.typeEncoder badType
                , invalidPayloadEncoder invalidPayload
                ]

        PortTypeInvalid region name portProblem ->
            BE.sequence
                [ BE.unsignedInt8 30
                , A.regionEncoder region
                , BE.string name
                , portProblemEncoder portProblem
                ]

        RecursiveAlias region name args tipe others ->
            BE.sequence
                [ BE.unsignedInt8 31
                , A.regionEncoder region
                , BE.string name
                , BE.list BE.string args
                , Src.typeEncoder tipe
                , BE.list BE.string others
                ]

        RecursiveDecl region name names ->
            BE.sequence
                [ BE.unsignedInt8 32
                , A.regionEncoder region
                , BE.string name
                , BE.list BE.string names
                ]

        RecursiveLet name names ->
            BE.sequence
                [ BE.unsignedInt8 33
                , A.locatedEncoder BE.string name
                , BE.list BE.string names
                ]

        Shadowing name r1 r2 ->
            BE.sequence
                [ BE.unsignedInt8 34
                , BE.string name
                , A.regionEncoder r1
                , A.regionEncoder r2
                ]

        TupleLargerThanThree region ->
            BE.sequence
                [ BE.unsignedInt8 35
                , A.regionEncoder region
                ]

        TypeVarsUnboundInUnion unionRegion typeName allVars unbound unbounds ->
            BE.sequence
                [ BE.unsignedInt8 36
                , A.regionEncoder unionRegion
                , BE.string typeName
                , BE.list BE.string allVars
                , BE.jsonPair BE.string A.regionEncoder unbound
                , BE.list (BE.jsonPair BE.string A.regionEncoder) unbounds
                ]

        TypeVarsMessedUpInAlias aliasRegion typeName allVars unusedVars unboundVars ->
            BE.sequence
                [ BE.unsignedInt8 37
                , A.regionEncoder aliasRegion
                , BE.string typeName
                , BE.list BE.string allVars
                , BE.list (BE.jsonPair BE.string A.regionEncoder) unusedVars
                , BE.list (BE.jsonPair BE.string A.regionEncoder) unboundVars
                ]


errorDecoder : BD.Decoder Error
errorDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map4 AnnotationTooShort
                            A.regionDecoder
                            BD.string
                            Index.zeroBasedDecoder
                            BD.int

                    1 ->
                        BD.map5 AmbiguousVar
                            A.regionDecoder
                            (BD.maybe BD.string)
                            BD.string
                            ModuleName.canonicalDecoder
                            (BD.oneOrMore ModuleName.canonicalDecoder)

                    2 ->
                        BD.map5 AmbiguousType
                            A.regionDecoder
                            (BD.maybe BD.string)
                            BD.string
                            ModuleName.canonicalDecoder
                            (BD.oneOrMore ModuleName.canonicalDecoder)

                    3 ->
                        BD.map5 AmbiguousVariant
                            A.regionDecoder
                            (BD.maybe BD.string)
                            BD.string
                            ModuleName.canonicalDecoder
                            (BD.oneOrMore ModuleName.canonicalDecoder)

                    4 ->
                        BD.map4 AmbiguousBinop
                            A.regionDecoder
                            BD.string
                            ModuleName.canonicalDecoder
                            (BD.oneOrMore ModuleName.canonicalDecoder)

                    5 ->
                        BD.map5 BadArity
                            A.regionDecoder
                            badArityContextDecoder
                            BD.string
                            BD.int
                            BD.int

                    6 ->
                        BD.map3 Binop
                            A.regionDecoder
                            BD.string
                            BD.string

                    7 ->
                        BD.map3 DuplicateDecl
                            BD.string
                            A.regionDecoder
                            A.regionDecoder

                    8 ->
                        BD.map3 DuplicateType
                            BD.string
                            A.regionDecoder
                            A.regionDecoder

                    9 ->
                        BD.map3 DuplicateCtor
                            BD.string
                            A.regionDecoder
                            A.regionDecoder

                    10 ->
                        BD.map3 DuplicateBinop
                            BD.string
                            A.regionDecoder
                            A.regionDecoder

                    11 ->
                        BD.map3 DuplicateField
                            BD.string
                            A.regionDecoder
                            A.regionDecoder

                    12 ->
                        BD.map4 DuplicateAliasArg
                            BD.string
                            BD.string
                            A.regionDecoder
                            A.regionDecoder

                    13 ->
                        BD.map4 DuplicateUnionArg
                            BD.string
                            BD.string
                            A.regionDecoder
                            A.regionDecoder

                    14 ->
                        BD.map4 DuplicatePattern
                            duplicatePatternContextDecoder
                            BD.string
                            A.regionDecoder
                            A.regionDecoder

                    15 ->
                        BD.map2 EffectNotFound
                            A.regionDecoder
                            BD.string

                    16 ->
                        BD.map2 EffectFunctionNotFound
                            A.regionDecoder
                            BD.string

                    17 ->
                        BD.map3 ExportDuplicate
                            BD.string
                            A.regionDecoder
                            A.regionDecoder

                    18 ->
                        BD.map4 ExportNotFound
                            A.regionDecoder
                            varKindDecoder
                            BD.string
                            (BD.list BD.string)

                    19 ->
                        BD.map2 ExportOpenAlias
                            A.regionDecoder
                            BD.string

                    20 ->
                        BD.map3 ImportCtorByName
                            A.regionDecoder
                            BD.string
                            BD.string

                    21 ->
                        BD.map3 ImportNotFound
                            A.regionDecoder
                            BD.string
                            (BD.list ModuleName.canonicalDecoder)

                    22 ->
                        BD.map2 ImportOpenAlias
                            A.regionDecoder
                            BD.string

                    23 ->
                        BD.map4 ImportExposingNotFound
                            A.regionDecoder
                            ModuleName.canonicalDecoder
                            BD.string
                            (BD.list BD.string)

                    24 ->
                        BD.map4 NotFoundVar
                            A.regionDecoder
                            (BD.maybe BD.string)
                            BD.string
                            possibleNamesDecoder

                    25 ->
                        BD.map4 NotFoundType
                            A.regionDecoder
                            (BD.maybe BD.string)
                            BD.string
                            possibleNamesDecoder

                    26 ->
                        BD.map4 NotFoundVariant
                            A.regionDecoder
                            (BD.maybe BD.string)
                            BD.string
                            possibleNamesDecoder

                    27 ->
                        BD.map3 NotFoundBinop
                            A.regionDecoder
                            BD.string
                            (BD.everySet identity BD.string)

                    28 ->
                        BD.map2 PatternHasRecordCtor
                            A.regionDecoder
                            BD.string

                    29 ->
                        BD.map4 PortPayloadInvalid
                            A.regionDecoder
                            BD.string
                            Can.typeDecoder
                            invalidPayloadDecoder

                    30 ->
                        BD.map3 PortTypeInvalid
                            A.regionDecoder
                            BD.string
                            portProblemDecoder

                    31 ->
                        BD.map5 RecursiveAlias
                            A.regionDecoder
                            BD.string
                            (BD.list BD.string)
                            Src.typeDecoder
                            (BD.list BD.string)

                    32 ->
                        BD.map3 RecursiveDecl
                            A.regionDecoder
                            BD.string
                            (BD.list BD.string)

                    33 ->
                        BD.map2 RecursiveLet
                            (A.locatedDecoder BD.string)
                            (BD.list BD.string)

                    34 ->
                        BD.map3 Shadowing
                            BD.string
                            A.regionDecoder
                            A.regionDecoder

                    35 ->
                        BD.map TupleLargerThanThree A.regionDecoder

                    36 ->
                        BD.map5 TypeVarsUnboundInUnion
                            A.regionDecoder
                            BD.string
                            (BD.list BD.string)
                            (BD.jsonPair BD.string A.regionDecoder)
                            (BD.list (BD.jsonPair BD.string A.regionDecoder))

                    37 ->
                        BD.map5 TypeVarsMessedUpInAlias
                            A.regionDecoder
                            BD.string
                            (BD.list BD.string)
                            (BD.list (BD.jsonPair BD.string A.regionDecoder))
                            (BD.list (BD.jsonPair BD.string A.regionDecoder))

                    _ ->
                        BD.fail
            )


badArityContextEncoder : BadArityContext -> BE.Encoder
badArityContextEncoder badArityContext =
    BE.unsignedInt8
        (case badArityContext of
            TypeArity ->
                0

            PatternArity ->
                1
        )


badArityContextDecoder : BD.Decoder BadArityContext
badArityContextDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed TypeArity

                    1 ->
                        BD.succeed PatternArity

                    _ ->
                        BD.fail
            )


duplicatePatternContextEncoder : DuplicatePatternContext -> BE.Encoder
duplicatePatternContextEncoder duplicatePatternContext =
    case duplicatePatternContext of
        DPLambdaArgs ->
            BE.unsignedInt8 0

        DPFuncArgs funcName ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string funcName
                ]

        DPCaseBranch ->
            BE.unsignedInt8 2

        DPLetBinding ->
            BE.unsignedInt8 3

        DPDestruct ->
            BE.unsignedInt8 4


duplicatePatternContextDecoder : BD.Decoder DuplicatePatternContext
duplicatePatternContextDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed DPLambdaArgs

                    1 ->
                        BD.map DPFuncArgs BD.string

                    2 ->
                        BD.succeed DPCaseBranch

                    3 ->
                        BD.succeed DPLetBinding

                    4 ->
                        BD.succeed DPDestruct

                    _ ->
                        BD.fail
            )


varKindEncoder : VarKind -> BE.Encoder
varKindEncoder varKind =
    BE.unsignedInt8
        (case varKind of
            BadOp ->
                0

            BadVar ->
                1

            BadPattern ->
                2

            BadType ->
                3
        )


varKindDecoder : BD.Decoder VarKind
varKindDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed BadOp

                    1 ->
                        BD.succeed BadVar

                    2 ->
                        BD.succeed BadPattern

                    3 ->
                        BD.succeed BadType

                    _ ->
                        BD.fail
            )


possibleNamesEncoder : PossibleNames -> BE.Encoder
possibleNamesEncoder possibleNames =
    BE.sequence
        [ BE.everySet compare BE.string possibleNames.locals
        , BE.assocListDict compare BE.string (BE.everySet compare BE.string) possibleNames.quals
        ]


possibleNamesDecoder : BD.Decoder PossibleNames
possibleNamesDecoder =
    BD.map2 PossibleNames
        (BD.everySet identity BD.string)
        (BD.assocListDict identity BD.string (BD.everySet identity BD.string))


invalidPayloadEncoder : InvalidPayload -> BE.Encoder
invalidPayloadEncoder invalidPayload =
    case invalidPayload of
        ExtendedRecord ->
            BE.unsignedInt8 0

        Function ->
            BE.unsignedInt8 1

        TypeVariable name ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.string name
                ]

        UnsupportedType name ->
            BE.sequence
                [ BE.unsignedInt8 3
                , BE.string name
                ]


invalidPayloadDecoder : BD.Decoder InvalidPayload
invalidPayloadDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed ExtendedRecord

                    1 ->
                        BD.succeed Function

                    2 ->
                        BD.map TypeVariable BD.string

                    3 ->
                        BD.map UnsupportedType BD.string

                    _ ->
                        BD.fail
            )


portProblemEncoder : PortProblem -> BE.Encoder
portProblemEncoder portProblem =
    case portProblem of
        CmdNoArg ->
            BE.unsignedInt8 0

        CmdExtraArgs n ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.int n
                ]

        CmdBadMsg ->
            BE.unsignedInt8 2

        SubBad ->
            BE.unsignedInt8 3

        NotCmdOrSub ->
            BE.unsignedInt8 4


portProblemDecoder : BD.Decoder PortProblem
portProblemDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed CmdNoArg

                    1 ->
                        BD.map CmdExtraArgs BD.int

                    2 ->
                        BD.succeed CmdBadMsg

                    3 ->
                        BD.succeed SubBad

                    4 ->
                        BD.succeed NotCmdOrSub

                    _ ->
                        BD.fail
            )
