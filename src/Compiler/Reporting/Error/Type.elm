module Compiler.Reporting.Error.Type exposing
    ( errorDecoder
    , errorEncoder
    , ptypeReplace
    , toReport
    , typeReplace
    )

import Compiler.AST.Canonical as Can
import Compiler.Data.Index as Index
import Compiler.Json.Decode as DecodeX
import Compiler.Json.Encode as EncodeX
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Report as Report
import Compiler.Reporting.Suggest as Suggest
import Compiler.Type.Error as T
import Data.Map as Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T



-- HELPERS


typeReplace : T.CRET_Expected a -> b -> T.CRET_Expected b
typeReplace expectation tipe =
    case expectation of
        T.CRET_NoExpectation _ ->
            T.CRET_NoExpectation tipe

        T.CRET_FromContext region context _ ->
            T.CRET_FromContext region context tipe

        T.CRET_FromAnnotation name arity context _ ->
            T.CRET_FromAnnotation name arity context tipe


ptypeReplace : T.CRET_PExpected a -> b -> T.CRET_PExpected b
ptypeReplace expectation tipe =
    case expectation of
        T.CRET_PNoExpectation _ ->
            T.CRET_PNoExpectation tipe

        T.CRET_PFromContext region context _ ->
            T.CRET_PFromContext region context tipe



-- TO REPORT


toReport : Code.Source -> T.CRRTL_Localizer -> T.CRET_Error -> Report.Report
toReport source localizer err =
    case err of
        T.CRET_BadExpr region category actualType expected ->
            toExprReport source localizer region category actualType expected

        T.CRET_BadPattern region category tipe expected ->
            toPatternReport source localizer region category tipe expected

        T.CRET_InfiniteType region name overallType ->
            toInfiniteReport source localizer region name overallType



-- TO PATTERN REPORT


toPatternReport : Code.Source -> T.CRRTL_Localizer -> T.CRA_Region -> T.CRET_PCategory -> T.CTE_Type -> T.CRET_PExpected T.CTE_Type -> Report.Report
toPatternReport source localizer patternRegion category tipe expected =
    Report.Report "TYPE MISMATCH" patternRegion [] <|
        case expected of
            T.CRET_PNoExpectation expectedType ->
                Code.toSnippet source patternRegion Nothing <|
                    ( D.fromChars "This pattern is being used in an unexpected way:"
                    , patternTypeComparison localizer
                        tipe
                        expectedType
                        (addPatternCategory "It is" category)
                        "But it needs to match:"
                        []
                    )

            T.CRET_PFromContext region context expectedType ->
                Code.toSnippet source region (Just patternRegion) <|
                    case context of
                        T.CRET_PTypedArg name index ->
                            ( D.reflow <|
                                "The "
                                    ++ D.ordinal index
                                    ++ " argument to `"
                                    ++ name
                                    ++ "` is weird."
                            , patternTypeComparison localizer
                                tipe
                                expectedType
                                (addPatternCategory "The argument is a pattern that matches" category)
                                ("But the type annotation on `"
                                    ++ name
                                    ++ "` says the "
                                    ++ D.ordinal index
                                    ++ " argument should be:"
                                )
                                []
                            )

                        T.CRET_PCaseMatch index ->
                            if index == Index.first then
                                ( D.reflow <|
                                    "The 1st pattern in this `case` causing a mismatch:"
                                , patternTypeComparison localizer
                                    tipe
                                    expectedType
                                    (addPatternCategory "The first pattern is trying to match" category)
                                    "But the expression between `case` and `of` is:"
                                    [ D.reflow <|
                                        "These can never match! Is the pattern the problem? Or is it the expression?"
                                    ]
                                )

                            else
                                ( D.reflow <|
                                    "The "
                                        ++ D.ordinal index
                                        ++ " pattern in this `case` does not match the previous ones."
                                , patternTypeComparison localizer
                                    tipe
                                    expectedType
                                    (addPatternCategory ("The " ++ D.ordinal index ++ " pattern is trying to match") category)
                                    "But all the previous patterns match:"
                                    [ D.link "Note"
                                        "A `case` expression can only handle one type of value, so you may want to use"
                                        "custom-types"
                                        "to handle “mixing” types."
                                    ]
                                )

                        T.CRET_PCtorArg name index ->
                            ( D.reflow <|
                                "The "
                                    ++ D.ordinal index
                                    ++ " argument to `"
                                    ++ name
                                    ++ "` is weird."
                            , patternTypeComparison localizer
                                tipe
                                expectedType
                                (addPatternCategory "It is trying to match" category)
                                ("But `"
                                    ++ name
                                    ++ "` needs its "
                                    ++ D.ordinal index
                                    ++ " argument to be:"
                                )
                                []
                            )

                        T.CRET_PListEntry index ->
                            ( D.reflow <|
                                "The "
                                    ++ D.ordinal index
                                    ++ " pattern in this list does not match all the previous ones:"
                            , patternTypeComparison localizer
                                tipe
                                expectedType
                                (addPatternCategory ("The " ++ D.ordinal index ++ " pattern is trying to match") category)
                                "But all the previous patterns in the list are:"
                                [ D.link "Hint"
                                    "Everything in a list must be the same type of value. This way, we never run into unexpected values partway through a List.map, List.foldl, etc. Read"
                                    "custom-types"
                                    "to learn how to “mix” types."
                                ]
                            )

                        T.CRET_PTail ->
                            ( D.reflow <|
                                "The pattern after (::) is causing issues."
                            , patternTypeComparison localizer
                                tipe
                                expectedType
                                (addPatternCategory "The pattern after (::) is trying to match" category)
                                "But it needs to match lists like this:"
                                []
                            )



-- PATTERN HELPERS


patternTypeComparison : T.CRRTL_Localizer -> T.CTE_Type -> T.CTE_Type -> String -> String -> List D.Doc -> D.Doc
patternTypeComparison localizer actual expected iAmSeeing insteadOf contextHints =
    let
        ( actualDoc, expectedDoc, problems ) =
            T.toComparison localizer actual expected
    in
    D.stack <|
        [ D.reflow iAmSeeing
        , D.indent 4 actualDoc
        , D.reflow insteadOf
        , D.indent 4 expectedDoc
        ]
            ++ problemsToHint problems
            ++ contextHints


addPatternCategory : String -> T.CRET_PCategory -> String
addPatternCategory iAmTryingToMatch category =
    iAmTryingToMatch
        ++ (case category of
                T.CRET_PRecord ->
                    " record values of type:"

                T.CRET_PUnit ->
                    " unit values:"

                T.CRET_PTuple ->
                    " tuples of type:"

                T.CRET_PList ->
                    " lists of type:"

                T.CRET_PCtor name ->
                    " `" ++ name ++ "` values of type:"

                T.CRET_PInt ->
                    " integers:"

                T.CRET_PStr ->
                    " strings:"

                T.CRET_PChr ->
                    " characters:"

                T.CRET_PBool ->
                    " booleans:"
           )



-- EXPR HELPERS


typeComparison : T.CRRTL_Localizer -> T.CTE_Type -> T.CTE_Type -> String -> String -> List D.Doc -> D.Doc
typeComparison localizer actual expected iAmSeeing insteadOf contextHints =
    let
        ( actualDoc, expectedDoc, problems ) =
            T.toComparison localizer actual expected
    in
    D.stack <|
        [ D.reflow iAmSeeing
        , D.indent 4 actualDoc
        , D.reflow insteadOf
        , D.indent 4 expectedDoc
        ]
            ++ contextHints
            ++ problemsToHint problems


loneType : T.CRRTL_Localizer -> T.CTE_Type -> T.CTE_Type -> D.Doc -> List D.Doc -> D.Doc
loneType localizer actual expected iAmSeeing furtherDetails =
    let
        ( actualDoc, _, problems ) =
            T.toComparison localizer actual expected
    in
    D.stack <|
        [ iAmSeeing
        , D.indent 4 actualDoc
        ]
            ++ furtherDetails
            ++ problemsToHint problems


addCategory : String -> T.CRET_Category -> String
addCategory thisIs category =
    case category of
        T.CRET_Local name ->
            "This `" ++ name ++ "` value is a:"

        T.CRET_Foreign name ->
            "This `" ++ name ++ "` value is a:"

        T.CRET_Access field ->
            "The value at ." ++ field ++ " is a:"

        T.CRET_Accessor field ->
            "This ." ++ field ++ " field access function has type:"

        T.CRET_If ->
            "This `if` expression produces:"

        T.CRET_Case ->
            "This `case` expression produces:"

        T.CRET_List ->
            thisIs ++ " a list of type:"

        T.CRET_Number ->
            thisIs ++ " a number of type:"

        T.CRET_Float ->
            thisIs ++ " a float of type:"

        T.CRET_String ->
            thisIs ++ " a string of type:"

        T.CRET_Char ->
            thisIs ++ " a character of type:"

        T.CRET_Lambda ->
            thisIs ++ " an anonymous function of type:"

        T.CRET_Record ->
            thisIs ++ " a record of type:"

        T.CRET_Tuple ->
            thisIs ++ " a tuple of type:"

        T.CRET_Unit ->
            thisIs ++ " a unit value:"

        T.CRET_Shader ->
            thisIs ++ " a GLSL shader of type:"

        T.CRET_Effects ->
            thisIs ++ " a thing for CORE LIBRARIES ONLY."

        T.CRET_CallResult maybeName ->
            case maybeName of
                T.CRET_NoName ->
                    thisIs ++ ":"

                T.CRET_FuncName name ->
                    "This `" ++ name ++ "` call produces:"

                T.CRET_CtorName name ->
                    "This `" ++ name ++ "` call produces:"

                T.CRET_OpName _ ->
                    thisIs ++ ":"


problemsToHint : List T.Problem -> List D.Doc
problemsToHint problems =
    case problems of
        [] ->
            []

        problem :: _ ->
            problemToHint problem


problemToHint : T.Problem -> List D.Doc
problemToHint problem =
    case problem of
        T.IntFloat ->
            [ D.fancyLink "Note"
                [ D.fromChars "Read" ]
                "implicit-casts"
                [ D.fromChars "to"
                , D.fromChars "learn"
                , D.fromChars "why"
                , D.fromChars "Elm"
                , D.fromChars "does"
                , D.fromChars "not"
                , D.fromChars "implicitly"
                , D.fromChars "convert"
                , D.fromChars "Ints"
                , D.fromChars "to"
                , D.fromChars "Floats."
                , D.fromChars "Use"
                , D.green (D.fromChars "toFloat")
                , D.fromChars "and"
                , D.green (D.fromChars "round")
                , D.fromChars "to"
                , D.fromChars "do"
                , D.fromChars "explicit"
                , D.fromChars "conversions."
                ]
            ]

        T.StringFromInt ->
            [ D.toFancyHint
                [ D.fromChars "Want"
                , D.fromChars "to"
                , D.fromChars "convert"
                , D.fromChars "an"
                , D.fromChars "Int"
                , D.fromChars "into"
                , D.fromChars "a"
                , D.fromChars "String?"
                , D.fromChars "Use"
                , D.fromChars "the"
                , D.green (D.fromChars "String.fromInt")
                , D.fromChars "function!"
                ]
            ]

        T.StringFromFloat ->
            [ D.toFancyHint
                [ D.fromChars "Want"
                , D.fromChars "to"
                , D.fromChars "convert"
                , D.fromChars "a"
                , D.fromChars "Float"
                , D.fromChars "into"
                , D.fromChars "a"
                , D.fromChars "String?"
                , D.fromChars "Use"
                , D.fromChars "the"
                , D.green (D.fromChars "String.fromFloat")
                , D.fromChars "function!"
                ]
            ]

        T.StringToInt ->
            [ D.toFancyHint
                [ D.fromChars "Want"
                , D.fromChars "to"
                , D.fromChars "convert"
                , D.fromChars "a"
                , D.fromChars "String"
                , D.fromChars "into"
                , D.fromChars "an"
                , D.fromChars "Int?"
                , D.fromChars "Use"
                , D.fromChars "the"
                , D.green (D.fromChars "String.toInt")
                , D.fromChars "function!"
                ]
            ]

        T.StringToFloat ->
            [ D.toFancyHint
                [ D.fromChars "Want"
                , D.fromChars "to"
                , D.fromChars "convert"
                , D.fromChars "a"
                , D.fromChars "String"
                , D.fromChars "into"
                , D.fromChars "a"
                , D.fromChars "Float?"
                , D.fromChars "Use"
                , D.fromChars "the"
                , D.green (D.fromChars "String.toFloat")
                , D.fromChars "function!"
                ]
            ]

        T.AnythingToBool ->
            [ D.toSimpleHint <|
                "Elm does not have “truthiness” such that ints and strings and lists are automatically converted to booleans. Do that conversion explicitly!"
            ]

        T.AnythingFromMaybe ->
            [ D.toFancyHint
                [ D.fromChars "Use"
                , D.green (D.fromChars "Maybe.withDefault")
                , D.fromChars "to"
                , D.fromChars "handle"
                , D.fromChars "possible"
                , D.fromChars "errors."
                , D.fromChars "Longer"
                , D.fromChars "term,"
                , D.fromChars "it"
                , D.fromChars "is"
                , D.fromChars "usually"
                , D.fromChars "better"
                , D.fromChars "to"
                , D.fromChars "write"
                , D.fromChars "out"
                , D.fromChars "the"
                , D.fromChars "full"
                , D.fromChars "`case`"
                , D.fromChars "though!"
                ]
            ]

        T.ArityMismatch x y ->
            [ D.toSimpleHint <|
                if x < y then
                    "It looks like it takes too few arguments. I was expecting " ++ String.fromInt (y - x) ++ " more."

                else
                    "It looks like it takes too many arguments. I see " ++ String.fromInt (x - y) ++ " extra."
            ]

        T.BadFlexSuper direction super tipe ->
            case tipe of
                T.CTE_Lambda _ _ _ ->
                    badFlexSuper direction super tipe

                T.CTE_Infinite ->
                    []

                T.CTE_Error ->
                    []

                T.CTE_FlexVar _ ->
                    []

                T.CTE_FlexSuper s _ ->
                    badFlexFlexSuper super s

                T.CTE_RigidVar y ->
                    badRigidVar y (toASuperThing super)

                T.CTE_RigidSuper s _ ->
                    badRigidSuper s (toASuperThing super)

                T.CTE_Type _ _ _ ->
                    badFlexSuper direction super tipe

                T.CTE_Record _ _ ->
                    badFlexSuper direction super tipe

                T.CTE_Unit ->
                    badFlexSuper direction super tipe

                T.CTE_Tuple _ _ _ ->
                    badFlexSuper direction super tipe

                T.CTE_Alias _ _ _ _ ->
                    badFlexSuper direction super tipe

        T.BadRigidVar x tipe ->
            case tipe of
                T.CTE_Lambda _ _ _ ->
                    badRigidVar x "a function"

                T.CTE_Infinite ->
                    []

                T.CTE_Error ->
                    []

                T.CTE_FlexVar _ ->
                    []

                T.CTE_FlexSuper s _ ->
                    badRigidVar x (toASuperThing s)

                T.CTE_RigidVar y ->
                    badDoubleRigid x y

                T.CTE_RigidSuper _ y ->
                    badDoubleRigid x y

                T.CTE_Type _ n _ ->
                    badRigidVar x ("a `" ++ n ++ "` value")

                T.CTE_Record _ _ ->
                    badRigidVar x "a record"

                T.CTE_Unit ->
                    badRigidVar x "a unit value"

                T.CTE_Tuple _ _ _ ->
                    badRigidVar x "a tuple"

                T.CTE_Alias _ n _ _ ->
                    badRigidVar x ("a `" ++ n ++ "` value")

        T.BadRigidSuper super x tipe ->
            case tipe of
                T.CTE_Lambda _ _ _ ->
                    badRigidSuper super "a function"

                T.CTE_Infinite ->
                    []

                T.CTE_Error ->
                    []

                T.CTE_FlexVar _ ->
                    []

                T.CTE_FlexSuper s _ ->
                    badRigidSuper super (toASuperThing s)

                T.CTE_RigidVar y ->
                    badDoubleRigid x y

                T.CTE_RigidSuper _ y ->
                    badDoubleRigid x y

                T.CTE_Type _ n _ ->
                    badRigidSuper super ("a `" ++ n ++ "` value")

                T.CTE_Record _ _ ->
                    badRigidSuper super "a record"

                T.CTE_Unit ->
                    badRigidSuper super "a unit value"

                T.CTE_Tuple _ _ _ ->
                    badRigidSuper super "a tuple"

                T.CTE_Alias _ n _ _ ->
                    badRigidSuper super ("a `" ++ n ++ "` value")

        T.FieldsMissing fields ->
            case List.map (D.green << D.fromName) fields of
                [] ->
                    []

                [ f1 ] ->
                    [ D.toFancyHint
                        [ D.fromChars "Looks"
                        , D.fromChars "like"
                        , D.fromChars "the"
                        , f1
                        , D.fromChars "field"
                        , D.fromChars "is"
                        , D.fromChars "missing."
                        ]
                    ]

                fieldDocs ->
                    [ D.toFancyHint <|
                        [ D.fromChars "Looks"
                        , D.fromChars "like"
                        , D.fromChars "fields"
                        ]
                            ++ D.commaSep (D.fromChars "and") identity fieldDocs
                            ++ [ D.fromChars "are", D.fromChars "missing." ]
                    ]

        T.FieldTypo typo possibilities ->
            case Suggest.sort typo identity possibilities of
                [] ->
                    []

                nearest :: _ ->
                    [ D.toFancyHint <|
                        [ D.fromChars "Seems"
                        , D.fromChars "like"
                        , D.fromChars "a"
                        , D.fromChars "record"
                        , D.fromChars "field"
                        , D.fromChars "typo."
                        , D.fromChars "Maybe"
                        , D.dullyellow (D.fromName typo)
                        , D.fromChars "should"
                        , D.fromChars "be"
                        , D.green (D.fromName nearest) |> D.a (D.fromChars "?")
                        ]
                    , D.toSimpleHint
                        "Can more type annotations be added? Type annotations always help me give more specific messages, and I think they could help a lot in this case!"
                    ]



-- BAD RIGID HINTS


badRigidVar : T.CDN_Name -> String -> List D.Doc
badRigidVar name aThing =
    [ D.toSimpleHint <|
        "Your type annotation uses type variable `"
            ++ name
            ++ "` which means ANY type of value can flow through, but your code is saying it specifically wants "
            ++ aThing
            ++ ". Maybe change your type annotation to be more specific? Maybe change the code to be more general?"
    , D.reflowLink "Read" "type-annotations" "for more advice!"
    ]


badDoubleRigid : T.CDN_Name -> T.CDN_Name -> List D.Doc
badDoubleRigid x y =
    [ D.toSimpleHint <|
        "Your type annotation uses `"
            ++ x
            ++ "` and `"
            ++ y
            ++ "` as separate type variables. Your code seems to be saying they are the same though. Maybe they should be the same in your type annotation? Maybe your code uses them in a weird way?"
    , D.reflowLink "Read" "type-annotations" "for more advice!"
    ]


toASuperThing : T.CTE_Super -> String
toASuperThing super =
    case super of
        T.CTE_Number ->
            "a `number` value"

        T.CTE_Comparable ->
            "a `comparable` value"

        T.CTE_CompAppend ->
            "a `compappend` value"

        T.CTE_Appendable ->
            "an `appendable` value"



-- BAD SUPER HINTS


badFlexSuper : T.Direction -> T.CTE_Super -> T.CTE_Type -> List D.Doc
badFlexSuper direction super tipe =
    case super of
        T.CTE_Comparable ->
            case tipe of
                T.CTE_Record _ _ ->
                    [ D.link "Hint"
                        "I do not know how to compare records. I can only compare ints, floats, chars, strings, lists of comparable values, and tuples of comparable values. Check out"
                        "comparing-records"
                        "for ideas on how to proceed."
                    ]

                T.CTE_Type _ name _ ->
                    [ D.toSimpleHint <|
                        "I do not know how to compare `"
                            ++ name
                            ++ "` values. I can only compare ints, floats, chars, strings, lists of comparable values, and tuples of comparable values."
                    , D.reflowLink
                        "Check out"
                        "comparing-custom-types"
                        "for ideas on how to proceed."
                    ]

                _ ->
                    [ D.toSimpleHint <|
                        "I only know how to compare ints, floats, chars, strings, lists of comparable values, and tuples of comparable values."
                    ]

        T.CTE_Appendable ->
            [ D.toSimpleHint "I only know how to append strings and lists."
            ]

        T.CTE_CompAppend ->
            [ D.toSimpleHint "Only strings and lists are both comparable and appendable."
            ]

        T.CTE_Number ->
            case tipe of
                T.CTE_Type home name _ ->
                    if T.isString home name then
                        case direction of
                            T.Have ->
                                [ D.toFancyHint
                                    [ D.fromChars "Try"
                                    , D.fromChars "using"
                                    , D.green (D.fromChars "String.fromInt")
                                    , D.fromChars "to"
                                    , D.fromChars "convert"
                                    , D.fromChars "it"
                                    , D.fromChars "to"
                                    , D.fromChars "a"
                                    , D.fromChars "string?"
                                    ]
                                ]

                            T.Need ->
                                [ D.toFancyHint
                                    [ D.fromChars "Try"
                                    , D.fromChars "using"
                                    , D.green (D.fromChars "String.toInt")
                                    , D.fromChars "to"
                                    , D.fromChars "convert"
                                    , D.fromChars "it"
                                    , D.fromChars "to"
                                    , D.fromChars "an"
                                    , D.fromChars "integer?"
                                    ]
                                ]

                    else
                        badFlexSuperNumber

                _ ->
                    badFlexSuperNumber


badFlexSuperNumber : List D.Doc
badFlexSuperNumber =
    [ D.toFancyHint
        [ D.fromChars "Only"
        , D.green (D.fromChars "Int")
        , D.fromChars "and"
        , D.green (D.fromChars "Float")
        , D.fromChars "values"
        , D.fromChars "work"
        , D.fromChars "as"
        , D.fromChars "numbers."
        ]
    ]


badRigidSuper : T.CTE_Super -> String -> List D.Doc
badRigidSuper super aThing =
    let
        ( superType, manyThings ) =
            case super of
                T.CTE_Number ->
                    ( "number", "ints AND floats" )

                T.CTE_Comparable ->
                    ( "comparable", "ints, floats, chars, strings, lists, and tuples" )

                T.CTE_Appendable ->
                    ( "appendable", "strings AND lists" )

                T.CTE_CompAppend ->
                    ( "compappend", "strings AND lists" )
    in
    [ D.toSimpleHint <|
        "The `"
            ++ superType
            ++ "` in your type annotation is saying that "
            ++ manyThings
            ++ " can flow through, but your code is saying it specifically wants "
            ++ aThing
            ++ ". Maybe change your type annotation to be more specific? Maybe change the code to be more general?"
    , D.reflowLink "Read" "type-annotations" "for more advice!"
    ]


badFlexFlexSuper : T.CTE_Super -> T.CTE_Super -> List D.Doc
badFlexFlexSuper s1 s2 =
    let
        likeThis : T.CTE_Super -> String
        likeThis super =
            case super of
                T.CTE_Number ->
                    "a number"

                T.CTE_Comparable ->
                    "comparable"

                T.CTE_CompAppend ->
                    "a compappend"

                T.CTE_Appendable ->
                    "appendable"
    in
    [ D.toSimpleHint <|
        "There are no values in Elm that are both "
            ++ likeThis s1
            ++ " and "
            ++ likeThis s2
            ++ "."
    ]



-- TO EXPR REPORT


toExprReport : Code.Source -> T.CRRTL_Localizer -> T.CRA_Region -> T.CRET_Category -> T.CTE_Type -> T.CRET_Expected T.CTE_Type -> Report.Report
toExprReport source localizer exprRegion category tipe expected =
    case expected of
        T.CRET_NoExpectation expectedType ->
            Report.Report "TYPE MISMATCH" exprRegion [] <|
                Code.toSnippet source
                    exprRegion
                    Nothing
                    ( D.fromChars "This expression is being used in an unexpected way:"
                    , typeComparison localizer
                        tipe
                        expectedType
                        (addCategory "It is" category)
                        "But you are trying to use it as:"
                        []
                    )

        T.CRET_FromAnnotation name _ subContext expectedType ->
            let
                thing : String
                thing =
                    case subContext of
                        T.CRET_TypedIfBranch index ->
                            D.ordinal index ++ " branch of this `if` expression:"

                        T.CRET_TypedCaseBranch index ->
                            D.ordinal index ++ " branch of this `case` expression:"

                        T.CRET_TypedBody ->
                            "body of the `" ++ name ++ "` definition:"

                itIs : String
                itIs =
                    case subContext of
                        T.CRET_TypedIfBranch index ->
                            "The " ++ D.ordinal index ++ " branch is"

                        T.CRET_TypedCaseBranch index ->
                            "The " ++ D.ordinal index ++ " branch is"

                        T.CRET_TypedBody ->
                            "The body is"
            in
            Report.Report "TYPE MISMATCH" exprRegion [] <|
                Code.toSnippet source exprRegion Nothing <|
                    ( D.reflow ("Something is off with the " ++ thing)
                    , typeComparison localizer
                        tipe
                        expectedType
                        (addCategory itIs category)
                        ("But the type annotation on `" ++ name ++ "` says it should be:")
                        []
                    )

        T.CRET_FromContext region context expectedType ->
            let
                mismatch : ( ( Maybe T.CRA_Region, String ), ( String, String, List D.Doc ) ) -> Report.Report
                mismatch ( ( maybeHighlight, problem ), ( thisIs, insteadOf, furtherDetails ) ) =
                    Report.Report "TYPE MISMATCH" exprRegion [] <|
                        Code.toSnippet source
                            region
                            maybeHighlight
                            ( D.reflow problem
                            , typeComparison localizer tipe expectedType (addCategory thisIs category) insteadOf furtherDetails
                            )

                badType : ( ( Maybe T.CRA_Region, String ), ( String, List D.Doc ) ) -> Report.Report
                badType ( ( maybeHighlight, problem ), ( thisIs, furtherDetails ) ) =
                    Report.Report "TYPE MISMATCH" exprRegion [] <|
                        Code.toSnippet source
                            region
                            maybeHighlight
                            ( D.reflow problem
                            , loneType localizer tipe expectedType (D.reflow (addCategory thisIs category)) furtherDetails
                            )

                custom : Maybe T.CRA_Region -> ( D.Doc, D.Doc ) -> Report.Report
                custom maybeHighlight docPair =
                    Report.Report "TYPE MISMATCH" exprRegion [] <|
                        Code.toSnippet source region maybeHighlight docPair
            in
            case context of
                T.CRET_ListEntry index ->
                    let
                        ith : String
                        ith =
                            D.ordinal index
                    in
                    mismatch
                        ( ( Just exprRegion
                          , "The " ++ ith ++ " element of this list does not match all the previous elements:"
                          )
                        , ( "The " ++ ith ++ " element is"
                          , "But all the previous elements in the list are:"
                          , [ D.link "Hint"
                                "Everything in a list must be the same type of value. This way, we never run into unexpected values partway through a List.map, List.foldl, etc. Read"
                                "custom-types"
                                "to learn how to “mix” types."
                            ]
                          )
                        )

                T.CRET_Negate ->
                    badType
                        ( ( Just exprRegion
                          , "I do not know how to negate this type of value:"
                          )
                        , ( "It is"
                          , [ D.fillSep
                                [ D.fromChars "But"
                                , D.fromChars "I"
                                , D.fromChars "only"
                                , D.fromChars "now"
                                , D.fromChars "how"
                                , D.fromChars "to"
                                , D.fromChars "negate"
                                , D.dullyellow (D.fromChars "Int")
                                , D.fromChars "and"
                                , D.dullyellow (D.fromChars "Float")
                                , D.fromChars "values."
                                ]
                            ]
                          )
                        )

                T.CRET_OpLeft op ->
                    custom (Just exprRegion) <|
                        opLeftToDocs localizer category op tipe expectedType

                T.CRET_OpRight op ->
                    case opRightToDocs localizer category op tipe expectedType of
                        EmphBoth details ->
                            custom Nothing details

                        EmphRight details ->
                            custom (Just exprRegion) details

                T.CRET_IfCondition ->
                    badType
                        ( ( Just exprRegion
                          , "This `if` condition does not evaluate to a boolean value, True or False."
                          )
                        , ( "It is"
                          , [ D.fillSep
                                [ D.fromChars "But"
                                , D.fromChars "I"
                                , D.fromChars "need"
                                , D.fromChars "this"
                                , D.fromChars "`if`"
                                , D.fromChars "condition"
                                , D.fromChars "to"
                                , D.fromChars "be"
                                , D.fromChars "a"
                                , D.dullyellow (D.fromChars "Bool")
                                , D.fromChars "value."
                                ]
                            ]
                          )
                        )

                T.CRET_IfBranch index ->
                    let
                        ith : String
                        ith =
                            D.ordinal index
                    in
                    mismatch
                        ( ( Just exprRegion
                          , "The " ++ ith ++ " branch of this `if` does not match all the previous branches:"
                          )
                        , ( "The " ++ ith ++ " branch is"
                          , "But all the previous branches result in:"
                          , [ D.link "Hint"
                                "All branches in an `if` must produce the same type of values. This way, no matter which branch we take, the result is always a consistent shape. Read"
                                "custom-types"
                                "to learn how to “mix” types."
                            ]
                          )
                        )

                T.CRET_CaseBranch index ->
                    let
                        ith : String
                        ith =
                            D.ordinal index
                    in
                    mismatch
                        ( ( Just exprRegion
                          , "The " ++ ith ++ " branch of this `case` does not match all the previous branches:"
                          )
                        , ( "The " ++ ith ++ " branch is"
                          , "But all the previous branches result in:"
                          , [ D.link "Hint"
                                "All branches in a `case` must produce the same type of values. This way, no matter which branch we take, the result is always a consistent shape. Read"
                                "custom-types"
                                "to learn how to “mix” types."
                            ]
                          )
                        )

                T.CRET_CallArity maybeFuncName numGivenArgs ->
                    Report.Report "TOO MANY ARGS" exprRegion [] <|
                        Code.toSnippet source region (Just exprRegion) <|
                            case countArgs tipe of
                                0 ->
                                    let
                                        thisValue : String
                                        thisValue =
                                            case maybeFuncName of
                                                T.CRET_NoName ->
                                                    "This value"

                                                T.CRET_FuncName name ->
                                                    "The `" ++ name ++ "` value"

                                                T.CRET_CtorName name ->
                                                    "The `" ++ name ++ "` value"

                                                T.CRET_OpName op ->
                                                    "The (" ++ op ++ ") operator"
                                    in
                                    ( D.reflow <| thisValue ++ " is not a function, but it was given " ++ D.args numGivenArgs ++ "."
                                    , D.reflow <| "Are there any missing commas? Or missing parentheses?"
                                    )

                                n ->
                                    let
                                        thisFunction : String
                                        thisFunction =
                                            case maybeFuncName of
                                                T.CRET_NoName ->
                                                    "This function"

                                                T.CRET_FuncName name ->
                                                    "The `" ++ name ++ "` function"

                                                T.CRET_CtorName name ->
                                                    "The `" ++ name ++ "` constructor"

                                                T.CRET_OpName op ->
                                                    "The (" ++ op ++ ") operator"
                                    in
                                    ( D.reflow <| thisFunction ++ " expects " ++ D.args n ++ ", but it got " ++ String.fromInt numGivenArgs ++ " instead."
                                    , D.reflow <| "Are there any missing commas? Or missing parentheses?"
                                    )

                T.CRET_CallArg maybeFuncName index ->
                    let
                        ith : String
                        ith =
                            D.ordinal index

                        thisFunction : String
                        thisFunction =
                            case maybeFuncName of
                                T.CRET_NoName ->
                                    "this function"

                                T.CRET_FuncName name ->
                                    "`" ++ name ++ "`"

                                T.CRET_CtorName name ->
                                    "`" ++ name ++ "`"

                                T.CRET_OpName op ->
                                    "(" ++ op ++ ")"
                    in
                    mismatch
                        ( ( Just exprRegion
                          , "The " ++ ith ++ " argument to " ++ thisFunction ++ " is not what I expect:"
                          )
                        , ( "This argument is"
                          , "But " ++ thisFunction ++ " needs the " ++ ith ++ " argument to be:"
                          , if Index.toHuman index == 1 then
                                []

                            else
                                [ D.toSimpleHint <|
                                    "I always figure out the argument types from left to right. If an argument is acceptable, I assume it is “correct” and move on. So the problem may actually be in one of the previous arguments!"
                                ]
                          )
                        )

                T.CRET_RecordAccess recordRegion maybeName fieldRegion field ->
                    case T.iteratedDealias tipe of
                        T.CTE_Record fields ext ->
                            custom (Just fieldRegion)
                                ( D.reflow <|
                                    "This "
                                        ++ Maybe.withDefault "" (Maybe.map (\n -> "`" ++ n ++ "`") maybeName)
                                        ++ " record does not have a `"
                                        ++ field
                                        ++ "` field:"
                                , case Suggest.sort field Tuple.first (Dict.toList compare fields) of
                                    [] ->
                                        D.reflow "In fact, it is a record with NO fields!"

                                    f :: fs ->
                                        D.stack
                                            [ D.reflow <|
                                                "This is usually a typo. Here are the "
                                                    ++ Maybe.withDefault "" (Maybe.map (\n -> "`" ++ n ++ "`") maybeName)
                                                    ++ " fields that are most similar:"
                                            , toNearbyRecord localizer f fs ext
                                            , D.fillSep
                                                [ D.fromChars "So"
                                                , D.fromChars "maybe"
                                                , D.dullyellow (D.fromName field)
                                                , D.fromChars "should"
                                                , D.fromChars "be"
                                                , D.green (D.fromName (Tuple.first f))
                                                    |> D.a (D.fromChars "?")
                                                ]
                                            ]
                                )

                        _ ->
                            badType
                                ( ( Just recordRegion
                                  , "This is not a record, so it has no fields to access!"
                                  )
                                , ( "It is"
                                  , [ D.fillSep
                                        [ D.fromChars "But"
                                        , D.fromChars "I"
                                        , D.fromChars "need"
                                        , D.fromChars "a"
                                        , D.fromChars "record"
                                        , D.fromChars "with"
                                        , D.fromChars "a"
                                        , D.dullyellow (D.fromName field)
                                        , D.fromChars "field!"
                                        ]
                                    ]
                                  )
                                )

                T.CRET_RecordUpdateKeys record expectedFields ->
                    case T.iteratedDealias tipe of
                        T.CTE_Record actualFields ext ->
                            case List.sortBy Tuple.first (Dict.toList compare (Dict.diff expectedFields actualFields)) of
                                [] ->
                                    mismatch
                                        ( ( Nothing
                                          , "Something is off with this record update:"
                                          )
                                        , ( "The `" ++ record ++ "` record is"
                                          , "But this update needs it to be compatable with:"
                                          , [ D.reflow
                                                "Do you mind creating an <http://sscce.org/> that produces this error message and sharing it at <https://github.com/elm/error-message-catalog/issues> so we can try to give better advice here?"
                                            ]
                                          )
                                        )

                                ( field, T.CASTC_FieldUpdate fieldRegion _ ) :: _ ->
                                    let
                                        rStr : String
                                        rStr =
                                            "`" ++ record ++ "`"

                                        fStr : String
                                        fStr =
                                            "`" ++ field ++ "`"
                                    in
                                    custom (Just fieldRegion)
                                        ( D.reflow <|
                                            "The "
                                                ++ rStr
                                                ++ " record does not have a "
                                                ++ fStr
                                                ++ " field:"
                                        , case Suggest.sort field Tuple.first (Dict.toList compare actualFields) of
                                            [] ->
                                                D.reflow <| "In fact, " ++ rStr ++ " is a record with NO fields!"

                                            f :: fs ->
                                                D.stack
                                                    [ D.reflow <|
                                                        "This is usually a typo. Here are the "
                                                            ++ rStr
                                                            ++ " fields that are most similar:"
                                                    , toNearbyRecord localizer f fs ext
                                                    , D.fillSep
                                                        [ D.fromChars "So"
                                                        , D.fromChars "maybe"
                                                        , D.dullyellow (D.fromName field)
                                                        , D.fromChars "should"
                                                        , D.fromChars "be"
                                                        , D.green (D.fromName (Tuple.first f))
                                                            |> D.a (D.fromChars "?")
                                                        ]
                                                    ]
                                        )

                        _ ->
                            badType
                                ( ( Just exprRegion
                                  , "This is not a record, so it has no fields to update!"
                                  )
                                , ( "It is"
                                  , [ D.reflow <| "But I need a record!"
                                    ]
                                  )
                                )

                T.CRET_RecordUpdateValue field ->
                    mismatch
                        ( ( Just exprRegion
                          , "I cannot update the `" ++ field ++ "` field like this:"
                          )
                        , ( "You are trying to update `" ++ field ++ "` to be"
                          , "But it should be:"
                          , [ D.toSimpleNote
                                "The record update syntax does not allow you to change the type of fields. You can achieve that with record constructors or the record literal syntax."
                            ]
                          )
                        )

                T.CRET_Destructure ->
                    mismatch
                        ( ( Nothing
                          , "This definition is causing issues:"
                          )
                        , ( "You are defining"
                          , "But then trying to destructure it as:"
                          , []
                          )
                        )



-- HELPERS


countArgs : T.CTE_Type -> Int
countArgs tipe =
    case tipe of
        T.CTE_Lambda _ _ stuff ->
            1 + List.length stuff

        _ ->
            0



-- FIELD NAME HELPERS


toNearbyRecord : T.CRRTL_Localizer -> ( T.CDN_Name, T.CTE_Type ) -> List ( T.CDN_Name, T.CTE_Type ) -> T.CTE_Extension -> D.Doc
toNearbyRecord localizer f fs ext =
    D.indent 4 <|
        if List.length fs <= 3 then
            RT.vrecord (List.map (fieldToDocs localizer) (f :: fs)) (extToDoc ext)

        else
            RT.vrecordSnippet (fieldToDocs localizer f) (List.map (fieldToDocs localizer) (List.take 3 fs))


fieldToDocs : T.CRRTL_Localizer -> ( T.CDN_Name, T.CTE_Type ) -> ( D.Doc, D.Doc )
fieldToDocs localizer ( name, tipe ) =
    ( D.fromName name
    , T.toDoc localizer RT.None tipe
    )


extToDoc : T.CTE_Extension -> Maybe D.Doc
extToDoc ext =
    case ext of
        T.CTE_Closed ->
            Nothing

        T.CTE_FlexOpen x ->
            Just (D.fromName x)

        T.CTE_RigidOpen x ->
            Just (D.fromName x)



-- OP LEFT


opLeftToDocs : T.CRRTL_Localizer -> T.CRET_Category -> T.CDN_Name -> T.CTE_Type -> T.CTE_Type -> ( D.Doc, D.Doc )
opLeftToDocs localizer category op tipe expected =
    case op of
        "+" ->
            if isString tipe then
                badStringAdd

            else if isList tipe then
                badListAdd localizer category "left" tipe expected

            else
                badMath localizer category "Addition" "left" "+" tipe expected []

        "*" ->
            if isList tipe then
                badListMul localizer category "left" tipe expected

            else
                badMath localizer category "Multiplication" "left" "*" tipe expected []

        "-" ->
            badMath localizer category "Subtraction" "left" "-" tipe expected []

        "^" ->
            badMath localizer category "Exponentiation" "left" "^" tipe expected []

        "/" ->
            badFDiv localizer (D.fromChars "left") tipe expected

        "//" ->
            badIDiv localizer (D.fromChars "left") tipe expected

        "&&" ->
            badBool localizer (D.fromChars "&&") (D.fromChars "left") tipe expected

        "||" ->
            badBool localizer (D.fromChars "||") (D.fromChars "left") tipe expected

        "<" ->
            badCompLeft localizer category "<" "left" tipe expected

        ">" ->
            badCompLeft localizer category ">" "left" tipe expected

        "<=" ->
            badCompLeft localizer category "<=" "left" tipe expected

        ">=" ->
            badCompLeft localizer category ">=" "left" tipe expected

        "++" ->
            badAppendLeft localizer category tipe expected

        "<|" ->
            ( D.fromChars "The left side of (<|) needs to be a function so I can pipe arguments to it!"
            , loneType localizer
                tipe
                expected
                (D.reflow (addCategory "I am seeing" category))
                [ D.reflow "This needs to be some kind of function though!" ]
            )

        _ ->
            ( D.reflow ("The left argument of (" ++ op ++ ") is causing problems:")
            , typeComparison localizer
                tipe
                expected
                (addCategory "The left argument is" category)
                ("But (" ++ op ++ ") needs the left argument to be:")
                []
            )



-- OP RIGHT


type RightDocs
    = EmphBoth ( D.Doc, D.Doc )
    | EmphRight ( D.Doc, D.Doc )


opRightToDocs : T.CRRTL_Localizer -> T.CRET_Category -> T.CDN_Name -> T.CTE_Type -> T.CTE_Type -> RightDocs
opRightToDocs localizer category op tipe expected =
    case op of
        "+" ->
            if isFloat expected && isInt tipe then
                badCast op FloatInt

            else if isInt expected && isFloat tipe then
                badCast op IntFloat

            else if isString tipe then
                EmphRight badStringAdd

            else if isList tipe then
                EmphRight (badListAdd localizer category "right" tipe expected)

            else
                EmphRight (badMath localizer category "Addition" "right" "+" tipe expected [])

        "*" ->
            if isFloat expected && isInt tipe then
                badCast op FloatInt

            else if isInt expected && isFloat tipe then
                badCast op IntFloat

            else if isList tipe then
                EmphRight (badListMul localizer category "right" tipe expected)

            else
                EmphRight (badMath localizer category "Multiplication" "right" "*" tipe expected [])

        "-" ->
            if isFloat expected && isInt tipe then
                badCast op FloatInt

            else if isInt expected && isFloat tipe then
                badCast op IntFloat

            else
                EmphRight (badMath localizer category "Subtraction" "right" "-" tipe expected [])

        "^" ->
            if isFloat expected && isInt tipe then
                badCast op FloatInt

            else if isInt expected && isFloat tipe then
                badCast op IntFloat

            else
                EmphRight (badMath localizer category "Exponentiation" "right" "^" tipe expected [])

        "/" ->
            EmphRight (badFDiv localizer (D.fromChars "right") tipe expected)

        "//" ->
            EmphRight (badIDiv localizer (D.fromChars "right") tipe expected)

        "&&" ->
            EmphRight (badBool localizer (D.fromChars "&&") (D.fromChars "right") tipe expected)

        "||" ->
            EmphRight (badBool localizer (D.fromChars "||") (D.fromChars "right") tipe expected)

        "<" ->
            badCompRight localizer "<" tipe expected

        ">" ->
            badCompRight localizer ">" tipe expected

        "<=" ->
            badCompRight localizer "<=" tipe expected

        ">=" ->
            badCompRight localizer ">=" tipe expected

        "==" ->
            badEquality localizer "==" tipe expected

        "/=" ->
            badEquality localizer "/=" tipe expected

        "::" ->
            badConsRight localizer category tipe expected

        "++" ->
            badAppendRight localizer category tipe expected

        "<|" ->
            EmphRight
                ( D.reflow "I cannot send this through the (<|) pipe:"
                , typeComparison localizer
                    tipe
                    expected
                    "The argument is:"
                    "But (<|) is piping it to a function that expects:"
                    []
                )

        "|>" ->
            case ( tipe, expected ) of
                ( T.CTE_Lambda expectedArgType _ _, T.CTE_Lambda argType _ _ ) ->
                    EmphRight
                        ( D.reflow "This function cannot handle the argument sent through the (|>) pipe:"
                        , typeComparison localizer
                            argType
                            expectedArgType
                            "The argument is:"
                            "But (|>) is piping it to a function that expects:"
                            []
                        )

                _ ->
                    EmphRight
                        ( D.reflow "The right side of (|>) needs to be a function so I can pipe arguments to it!"
                        , loneType localizer
                            tipe
                            expected
                            (D.reflow (addCategory "But instead of a function, I am seeing" category))
                            []
                        )

        _ ->
            badOpRightFallback localizer category op tipe expected


badOpRightFallback : T.CRRTL_Localizer -> T.CRET_Category -> T.CDN_Name -> T.CTE_Type -> T.CTE_Type -> RightDocs
badOpRightFallback localizer category op tipe expected =
    EmphRight
        ( D.reflow ("The right argument of (" ++ op ++ ") is causing problems.")
        , typeComparison localizer
            tipe
            expected
            (addCategory "The right argument is" category)
            ("But (" ++ op ++ ") needs the right argument to be:")
            [ D.toSimpleHint <|
                "With operators like ("
                    ++ op
                    ++ ") I always check the left side first. If it seems fine, I assume it is correct and check the right side. So the problem may be in how the left and right arguments interact!"
            ]
        )


isInt : T.CTE_Type -> Bool
isInt tipe =
    case tipe of
        T.CTE_Type home name [] ->
            T.isInt home name

        _ ->
            False


isFloat : T.CTE_Type -> Bool
isFloat tipe =
    case tipe of
        T.CTE_Type home name [] ->
            T.isFloat home name

        _ ->
            False


isString : T.CTE_Type -> Bool
isString tipe =
    case tipe of
        T.CTE_Type home name [] ->
            T.isString home name

        _ ->
            False


isList : T.CTE_Type -> Bool
isList tipe =
    case tipe of
        T.CTE_Type home name [ _ ] ->
            T.isList home name

        _ ->
            False



-- BAD CONS


badConsRight : T.CRRTL_Localizer -> T.CRET_Category -> T.CTE_Type -> T.CTE_Type -> RightDocs
badConsRight localizer category tipe expected =
    case tipe of
        T.CTE_Type home1 name1 [ actualElement ] ->
            if T.isList home1 name1 then
                case expected of
                    T.CTE_Type home2 name2 [ expectedElement ] ->
                        if T.isList home2 name2 then
                            EmphBoth
                                ( D.reflow "I am having trouble with this (::) operator:"
                                , typeComparison localizer
                                    expectedElement
                                    actualElement
                                    "The left side of (::) is:"
                                    "But you are trying to put that into a list filled with:"
                                    (case expectedElement of
                                        T.CTE_Type home name [ _ ] ->
                                            if T.isList home name then
                                                [ D.toSimpleHint
                                                    "Are you trying to append two lists? The (++) operator appends lists, whereas the (::) operator is only for adding ONE element to a list."
                                                ]

                                            else
                                                [ D.reflow
                                                    "Lists need ALL elements to be the same type though."
                                                ]

                                        _ ->
                                            [ D.reflow
                                                "Lists need ALL elements to be the same type though."
                                            ]
                                    )
                                )

                        else
                            badOpRightFallback localizer category "::" tipe expected

                    _ ->
                        badOpRightFallback localizer category "::" tipe expected

            else
                EmphRight
                    ( D.reflow "The (::) operator can only add elements onto lists."
                    , loneType localizer
                        tipe
                        expected
                        (D.reflow (addCategory "The right side is" category))
                        [ D.fillSep
                            [ D.fromChars "But"
                            , D.fromChars "(::)"
                            , D.fromChars "needs"
                            , D.fromChars "a"
                            , D.dullyellow (D.fromChars "List")
                            , D.fromChars "on"
                            , D.fromChars "the"
                            , D.fromChars "right."
                            ]
                        ]
                    )

        _ ->
            EmphRight
                ( D.reflow "The (::) operator can only add elements onto lists."
                , loneType localizer
                    tipe
                    expected
                    (D.reflow (addCategory "The right side is" category))
                    [ D.fillSep
                        [ D.fromChars "But"
                        , D.fromChars "(::)"
                        , D.fromChars "needs"
                        , D.fromChars "a"
                        , D.dullyellow (D.fromChars "List")
                        , D.fromChars "on"
                        , D.fromChars "the"
                        , D.fromChars "right."
                        ]
                    ]
                )



-- BAD APPEND


type AppendType
    = ANumber D.Doc D.Doc
    | AString
    | AList
    | AOther


toAppendType : T.CTE_Type -> AppendType
toAppendType tipe =
    case tipe of
        T.CTE_Type home name _ ->
            if T.isInt home name then
                ANumber (D.fromChars "Int") (D.fromChars "String.fromInt")

            else if T.isFloat home name then
                ANumber (D.fromChars "Float") (D.fromChars "String.fromFloat")

            else if T.isString home name then
                AString

            else if T.isList home name then
                AList

            else
                AOther

        T.CTE_FlexSuper T.CTE_Number _ ->
            ANumber (D.fromChars "number") (D.fromChars "String.fromInt")

        _ ->
            AOther


badAppendLeft : T.CRRTL_Localizer -> T.CRET_Category -> T.CTE_Type -> T.CTE_Type -> ( D.Doc, D.Doc )
badAppendLeft localizer category tipe expected =
    case toAppendType tipe of
        ANumber thing stringFromThing ->
            ( D.fillSep
                [ D.fromChars "The"
                , D.fromChars "(++)"
                , D.fromChars "operator"
                , D.fromChars "can"
                , D.fromChars "append"
                , D.fromChars "List"
                , D.fromChars "and"
                , D.fromChars "String"
                , D.fromChars "values,"
                , D.fromChars "but"
                , D.fromChars "not"
                , D.dullyellow thing
                , D.fromChars "values"
                , D.fromChars "like"
                , D.fromChars "this:"
                ]
            , D.fillSep
                [ D.fromChars "Try"
                , D.fromChars "using"
                , D.green stringFromThing
                , D.fromChars "to"
                , D.fromChars "turn"
                , D.fromChars "it"
                , D.fromChars "into"
                , D.fromChars "a"
                , D.fromChars "string?"
                , D.fromChars "Or"
                , D.fromChars "put"
                , D.fromChars "it"
                , D.fromChars "in"
                , D.fromChars "[]"
                , D.fromChars "to"
                , D.fromChars "make"
                , D.fromChars "it"
                , D.fromChars "a"
                , D.fromChars "list?"
                , D.fromChars "Or"
                , D.fromChars "switch"
                , D.fromChars "to"
                , D.fromChars "the"
                , D.fromChars "(::)"
                , D.fromChars "operator?"
                ]
            )

        _ ->
            ( D.reflow "The (++) operator cannot append this type of value:"
            , loneType localizer
                tipe
                expected
                (D.reflow (addCategory "I am seeing" category))
                [ D.fillSep
                    [ D.fromChars "But"
                    , D.fromChars "the"
                    , D.fromChars "(++)"
                    , D.fromChars "operator"
                    , D.fromChars "is"
                    , D.fromChars "only"
                    , D.fromChars "for"
                    , D.fromChars "appending"
                    , D.dullyellow (D.fromChars "List")
                    , D.fromChars "and"
                    , D.dullyellow (D.fromChars "String")
                    , D.fromChars "values."
                    , D.fromChars "Maybe"
                    , D.fromChars "put"
                    , D.fromChars "this"
                    , D.fromChars "value"
                    , D.fromChars "in"
                    , D.fromChars "[]"
                    , D.fromChars "to"
                    , D.fromChars "make"
                    , D.fromChars "it"
                    , D.fromChars "a"
                    , D.fromChars "list?"
                    ]
                ]
            )


badAppendRight : T.CRRTL_Localizer -> T.CRET_Category -> T.CTE_Type -> T.CTE_Type -> RightDocs
badAppendRight localizer category tipe expected =
    case ( toAppendType expected, toAppendType tipe ) of
        ( AString, ANumber thing stringFromThing ) ->
            EmphRight
                ( D.fillSep
                    [ D.fromChars "I"
                    , D.fromChars "thought"
                    , D.fromChars "I"
                    , D.fromChars "was"
                    , D.fromChars "appending"
                    , D.dullyellow (D.fromChars "String")
                    , D.fromChars "values"
                    , D.fromChars "here,"
                    , D.fromChars "not"
                    , D.dullyellow thing
                    , D.fromChars "values"
                    , D.fromChars "like"
                    , D.fromChars "this:"
                    ]
                , D.fillSep
                    [ D.fromChars "Try"
                    , D.fromChars "using"
                    , D.green stringFromThing
                    , D.fromChars "to"
                    , D.fromChars "turn"
                    , D.fromChars "it"
                    , D.fromChars "into"
                    , D.fromChars "a"
                    , D.fromChars "string?"
                    ]
                )

        ( AList, ANumber thing _ ) ->
            EmphRight
                ( D.fillSep
                    [ D.fromChars "I"
                    , D.fromChars "thought"
                    , D.fromChars "I"
                    , D.fromChars "was"
                    , D.fromChars "appending"
                    , D.dullyellow (D.fromChars "List")
                    , D.fromChars "values"
                    , D.fromChars "here,"
                    , D.fromChars "not"
                    , D.dullyellow thing
                    , D.fromChars "values"
                    , D.fromChars "like"
                    , D.fromChars "this:"
                    ]
                , D.reflow "Try putting it in [] to make it a list?"
                )

        ( AString, AList ) ->
            EmphBoth
                ( D.reflow "The (++) operator needs the same type of value on both sides:"
                , D.fillSep
                    [ D.fromChars "I"
                    , D.fromChars "see"
                    , D.fromChars "a"
                    , D.dullyellow (D.fromChars "String")
                    , D.fromChars "on"
                    , D.fromChars "the"
                    , D.fromChars "left"
                    , D.fromChars "and"
                    , D.fromChars "a"
                    , D.dullyellow (D.fromChars "List")
                    , D.fromChars "on"
                    , D.fromChars "the"
                    , D.fromChars "right."
                    , D.fromChars "Which"
                    , D.fromChars "should"
                    , D.fromChars "it"
                    , D.fromChars "be?"
                    , D.fromChars "Does"
                    , D.fromChars "the"
                    , D.fromChars "string"
                    , D.fromChars "need"
                    , D.fromChars "[]"
                    , D.fromChars "around"
                    , D.fromChars "it"
                    , D.fromChars "to"
                    , D.fromChars "become"
                    , D.fromChars "a"
                    , D.fromChars "list?"
                    ]
                )

        ( AList, AString ) ->
            EmphBoth
                ( D.reflow "The (++) operator needs the same type of value on both sides:"
                , D.fillSep
                    [ D.fromChars "I"
                    , D.fromChars "see"
                    , D.fromChars "a"
                    , D.dullyellow (D.fromChars "List")
                    , D.fromChars "on"
                    , D.fromChars "the"
                    , D.fromChars "left"
                    , D.fromChars "and"
                    , D.fromChars "a"
                    , D.dullyellow (D.fromChars "String")
                    , D.fromChars "on"
                    , D.fromChars "the"
                    , D.fromChars "right."
                    , D.fromChars "Which"
                    , D.fromChars "should"
                    , D.fromChars "it"
                    , D.fromChars "be?"
                    , D.fromChars "Does"
                    , D.fromChars "the"
                    , D.fromChars "string"
                    , D.fromChars "need"
                    , D.fromChars "[]"
                    , D.fromChars "around"
                    , D.fromChars "it"
                    , D.fromChars "to"
                    , D.fromChars "become"
                    , D.fromChars "a"
                    , D.fromChars "list?"
                    ]
                )

        _ ->
            EmphBoth
                ( D.reflow "The (++) operator cannot append these two values:"
                , typeComparison localizer
                    expected
                    tipe
                    "I already figured out that the left side of (++) is:"
                    (addCategory "But this clashes with the right side, which is" category)
                    []
                )



-- BAD MATH


type ThisThenThat
    = FloatInt
    | IntFloat


badCast : T.CDN_Name -> ThisThenThat -> RightDocs
badCast op thisThenThat =
    EmphBoth
        ( D.reflow <|
            "I need both sides of ("
                ++ op
                ++ ") to be the exact same type. Both Int or both Float."
        , let
            anInt : List D.Doc
            anInt =
                [ D.fromChars "an", D.dullyellow (D.fromChars "Int") ]

            aFloat : List D.Doc
            aFloat =
                [ D.fromChars "a", D.dullyellow (D.fromChars "Float") ]

            toFloat : D.Doc
            toFloat =
                D.green (D.fromChars "toFloat")

            round : D.Doc
            round =
                D.green (D.fromChars "round")
          in
          case thisThenThat of
            FloatInt ->
                badCastHelp aFloat anInt round toFloat

            IntFloat ->
                badCastHelp anInt aFloat toFloat round
        )


badCastHelp : List D.Doc -> List D.Doc -> D.Doc -> D.Doc -> D.Doc
badCastHelp anInt aFloat toFloat round =
    D.stack
        [ D.fillSep <|
            [ D.fromChars "But"
            , D.fromChars "I"
            , D.fromChars "see"
            ]
                ++ anInt
                ++ [ D.fromChars "on"
                   , D.fromChars "the"
                   , D.fromChars "left"
                   , D.fromChars "and"
                   ]
                ++ aFloat
                ++ [ D.fromChars "on"
                   , D.fromChars "the"
                   , D.fromChars "right."
                   ]
        , D.fillSep
            [ D.fromChars "Use"
            , toFloat
            , D.fromChars "on"
            , D.fromChars "the"
            , D.fromChars "left"
            , D.fromChars "(or"
            , round
            , D.fromChars "on"
            , D.fromChars "the"
            , D.fromChars "right)"
            , D.fromChars "to"
            , D.fromChars "make"
            , D.fromChars "both"
            , D.fromChars "sides"
            , D.fromChars "match!"
            ]
        , D.link "Note" "Read" "implicit-casts" "to learn why Elm does not implicitly convert Ints to Floats."
        ]


badStringAdd : ( D.Doc, D.Doc )
badStringAdd =
    ( D.fillSep
        [ D.fromChars "I"
        , D.fromChars "cannot"
        , D.fromChars "do"
        , D.fromChars "addition"
        , D.fromChars "with"
        , D.dullyellow (D.fromChars "String")
        , D.fromChars "values"
        , D.fromChars "like"
        , D.fromChars "this"
        , D.fromChars "one:"
        ]
    , D.stack
        [ D.fillSep
            [ D.fromChars "The"
            , D.fromChars "(+)"
            , D.fromChars "operator"
            , D.fromChars "only"
            , D.fromChars "works"
            , D.fromChars "with"
            , D.dullyellow (D.fromChars "Int")
            , D.fromChars "and"
            , D.dullyellow (D.fromChars "Float")
            , D.fromChars "values."
            ]
        , D.toFancyHint
            [ D.fromChars "Switch"
            , D.fromChars "to"
            , D.fromChars "the"
            , D.green (D.fromChars "(++)")
            , D.fromChars "operator"
            , D.fromChars "to"
            , D.fromChars "append"
            , D.fromChars "strings!"
            ]
        ]
    )


badListAdd : T.CRRTL_Localizer -> T.CRET_Category -> String -> T.CTE_Type -> T.CTE_Type -> ( D.Doc, D.Doc )
badListAdd localizer category direction tipe expected =
    ( D.fromChars "I cannot do addition with lists:"
    , loneType localizer
        tipe
        expected
        (D.reflow (addCategory ("The " ++ direction ++ " side of (+) is") category))
        [ D.fillSep
            [ D.fromChars "But"
            , D.fromChars "(+)"
            , D.fromChars "only"
            , D.fromChars "works"
            , D.fromChars "with"
            , D.dullyellow (D.fromChars "Int")
            , D.fromChars "and"
            , D.dullyellow (D.fromChars "Float")
            , D.fromChars "values."
            ]
        , D.toFancyHint
            [ D.fromChars "Switch"
            , D.fromChars "to"
            , D.fromChars "the"
            , D.green (D.fromChars "(++)")
            , D.fromChars "operator"
            , D.fromChars "to"
            , D.fromChars "append"
            , D.fromChars "lists!"
            ]
        ]
    )


badListMul : T.CRRTL_Localizer -> T.CRET_Category -> String -> T.CTE_Type -> T.CTE_Type -> ( D.Doc, D.Doc )
badListMul localizer category direction tipe expected =
    badMath localizer category "Multiplication" direction "*" tipe expected <|
        [ D.toFancyHint
            [ D.fromChars "Maybe"
            , D.fromChars "you"
            , D.fromChars "want"
            , D.green (D.fromChars "List.repeat")
            , D.fromChars "to"
            , D.fromChars "build"
            , D.fromChars "a"
            , D.fromChars "list"
            , D.fromChars "of"
            , D.fromChars "repeated"
            , D.fromChars "values?"
            ]
        ]


badMath : T.CRRTL_Localizer -> T.CRET_Category -> String -> String -> String -> T.CTE_Type -> T.CTE_Type -> List D.Doc -> ( D.Doc, D.Doc )
badMath localizer category operation direction op tipe expected otherHints =
    ( D.reflow <|
        operation
            ++ " does not work with this value:"
    , loneType localizer
        tipe
        expected
        (D.reflow (addCategory ("The " ++ direction ++ " side of (" ++ op ++ ") is") category))
        (D.fillSep
            [ D.fromChars "But"
            , D.fromChars ("(" ++ op ++ ")")
            , D.fromChars "only"
            , D.fromChars "works"
            , D.fromChars "with"
            , D.dullyellow (D.fromChars "Int")
            , D.fromChars "and"
            , D.dullyellow (D.fromChars "Float")
            , D.fromChars "values."
            ]
            :: otherHints
        )
    )


badFDiv : T.CRRTL_Localizer -> D.Doc -> T.CTE_Type -> T.CTE_Type -> ( D.Doc, D.Doc )
badFDiv localizer direction tipe expected =
    ( D.reflow "The (/) operator is specifically for floating-point division:"
    , if isInt tipe then
        D.stack
            [ D.fillSep
                [ D.fromChars "The"
                , direction
                , D.fromChars "side"
                , D.fromChars "of"
                , D.fromChars "(/)"
                , D.fromChars "must"
                , D.fromChars "be"
                , D.fromChars "a"
                , D.dullyellow (D.fromChars "Float") |> D.a (D.fromChars ",")
                , D.fromChars "but"
                , D.fromChars "I"
                , D.fromChars "am"
                , D.fromChars "seeing"
                , D.fromChars "an"
                , D.dullyellow (D.fromChars "Int") |> D.a (D.fromChars ".")
                , D.fromChars "I"
                , D.fromChars "recommend:"
                ]
            , D.vcat
                [ D.green (D.fromChars "toFloat")
                    |> D.a (D.fromChars " for explicit conversions     ")
                    |> D.a (D.black (D.fromChars "(toFloat 5 / 2) == 2.5"))
                , D.green (D.fromChars "(//)   ")
                    |> D.a (D.fromChars " for integer division         ")
                    |> D.a (D.black (D.fromChars "(5 // 2)        == 2"))
                ]
            , D.link "Note" "Read" "implicit-casts" "to learn why Elm does not implicitly convert Ints to Floats."
            ]

      else
        loneType localizer
            tipe
            expected
            (D.fillSep
                [ D.fromChars "The"
                , direction
                , D.fromChars "side"
                , D.fromChars "of"
                , D.fromChars "(/)"
                , D.fromChars "must"
                , D.fromChars "be"
                , D.fromChars "a"
                , D.dullyellow (D.fromChars "Float") |> D.a (D.fromChars ",")
                , D.fromChars "but"
                , D.fromChars "instead"
                , D.fromChars "I"
                , D.fromChars "am"
                , D.fromChars "seeing:"
                ]
            )
            []
    )


badIDiv : T.CRRTL_Localizer -> D.Doc -> T.CTE_Type -> T.CTE_Type -> ( D.Doc, D.Doc )
badIDiv localizer direction tipe expected =
    ( D.reflow "The (//) operator is specifically for integer division:"
    , if isFloat tipe then
        D.stack
            [ D.fillSep
                [ D.fromChars "The"
                , direction
                , D.fromChars "side"
                , D.fromChars "of"
                , D.fromChars "(//)"
                , D.fromChars "must"
                , D.fromChars "be"
                , D.fromChars "an"
                , D.dullyellow (D.fromChars "Int") |> D.a (D.fromChars ",")
                , D.fromChars "but"
                , D.fromChars "I"
                , D.fromChars "am"
                , D.fromChars "seeing"
                , D.fromChars "a"
                , D.dullyellow (D.fromChars "Float") |> D.a (D.fromChars ".")
                , D.fromChars "I"
                , D.fromChars "recommend"
                , D.fromChars "doing"
                , D.fromChars "the"
                , D.fromChars "conversion"
                , D.fromChars "explicitly"
                , D.fromChars "with"
                , D.fromChars "one"
                , D.fromChars "of"
                , D.fromChars "these"
                , D.fromChars "functions:"
                ]
            , D.vcat
                [ D.green (D.fromChars "round") |> D.a (D.fromChars " 3.5     == 4")
                , D.green (D.fromChars "floor") |> D.a (D.fromChars " 3.5     == 3")
                , D.green (D.fromChars "ceiling") |> D.a (D.fromChars " 3.5   == 4")
                , D.green (D.fromChars "truncate") |> D.a (D.fromChars " 3.5  == 3")
                ]
            , D.link "Note" "Read" "implicit-casts" "to learn why Elm does not implicitly convert Ints to Floats."
            ]

      else
        loneType localizer
            tipe
            expected
            (D.fillSep
                [ D.fromChars "The"
                , direction
                , D.fromChars "side"
                , D.fromChars "of"
                , D.fromChars "(//)"
                , D.fromChars "must"
                , D.fromChars "be"
                , D.fromChars "an"
                , D.dullyellow (D.fromChars "Int") |> D.a (D.fromChars ",")
                , D.fromChars "but"
                , D.fromChars "instead"
                , D.fromChars "I"
                , D.fromChars "am"
                , D.fromChars "seeing:"
                ]
            )
            []
    )



-- BAD BOOLS


badBool : T.CRRTL_Localizer -> D.Doc -> D.Doc -> T.CTE_Type -> T.CTE_Type -> ( D.Doc, D.Doc )
badBool localizer op direction tipe expected =
    ( D.reflow "I am struggling with this boolean operation:"
    , loneType localizer
        tipe
        expected
        (D.fillSep
            [ D.fromChars "Both"
            , D.fromChars "sides"
            , D.fromChars "of"
            , D.fromChars "(" |> D.a op |> D.a (D.fromChars ")")
            , D.fromChars "must"
            , D.fromChars "be"
            , D.dullyellow (D.fromChars "Bool")
            , D.fromChars "values,"
            , D.fromChars "but"
            , D.fromChars "the"
            , direction
            , D.fromChars "side"
            , D.fromChars "is:"
            ]
        )
        []
    )



-- BAD COMPARISON


badCompLeft : T.CRRTL_Localizer -> T.CRET_Category -> String -> String -> T.CTE_Type -> T.CTE_Type -> ( D.Doc, D.Doc )
badCompLeft localizer category op direction tipe expected =
    ( D.reflow "I cannot do a comparison with this value:"
    , loneType localizer
        tipe
        expected
        (D.reflow (addCategory ("The " ++ direction ++ " side of (" ++ op ++ ") is") category))
        [ D.fillSep
            [ D.fromChars "But"
            , D.fromChars ("(" ++ op ++ ")")
            , D.fromChars "only"
            , D.fromChars "works"
            , D.fromChars "on"
            , D.dullyellow (D.fromChars "Int") |> D.a (D.fromChars ",")
            , D.dullyellow (D.fromChars "Float") |> D.a (D.fromChars ",")
            , D.dullyellow (D.fromChars "Char") |> D.a (D.fromChars ",")
            , D.fromChars "and"
            , D.dullyellow (D.fromChars "String")
            , D.fromChars "values."
            , D.fromChars "It"
            , D.fromChars "can"
            , D.fromChars "work"
            , D.fromChars "on"
            , D.fromChars "lists"
            , D.fromChars "and"
            , D.fromChars "tuples"
            , D.fromChars "of"
            , D.fromChars "comparable"
            , D.fromChars "values"
            , D.fromChars "as"
            , D.fromChars "well,"
            , D.fromChars "but"
            , D.fromChars "it"
            , D.fromChars "is"
            , D.fromChars "usually"
            , D.fromChars "better"
            , D.fromChars "to"
            , D.fromChars "find"
            , D.fromChars "a"
            , D.fromChars "different"
            , D.fromChars "path."
            ]
        ]
    )


badCompRight : T.CRRTL_Localizer -> String -> T.CTE_Type -> T.CTE_Type -> RightDocs
badCompRight localizer op tipe expected =
    EmphBoth
        ( D.reflow <|
            ("I need both sides of (" ++ op ++ ") to be the same type:")
        , typeComparison localizer
            expected
            tipe
            ("The left side of (" ++ op ++ ") is:")
            "But the right side is:"
            [ D.reflow <|
                ("I cannot compare different types though! Which side of (" ++ op ++ ") is the problem?")
            ]
        )



-- BAD EQUALITY


badEquality : T.CRRTL_Localizer -> String -> T.CTE_Type -> T.CTE_Type -> RightDocs
badEquality localizer op tipe expected =
    EmphBoth
        ( D.reflow <|
            ("I need both sides of (" ++ op ++ ") to be the same type:")
        , typeComparison localizer
            expected
            tipe
            ("The left side of (" ++ op ++ ") is:")
            "But the right side is:"
            [ if isFloat tipe || isFloat expected then
                D.toSimpleNote <|
                    "Equality on floats is not 100% reliable due to the design of IEEE 754. I recommend a check like (abs (x - y) < 0.0001) instead."

              else
                D.reflow "Different types can never be equal though! Which side is messed up?"
            ]
        )



-- INFINITE TYPES


toInfiniteReport : Code.Source -> T.CRRTL_Localizer -> T.CRA_Region -> T.CDN_Name -> T.CTE_Type -> Report.Report
toInfiniteReport source localizer region name overallType =
    Report.Report "INFINITE TYPE" region [] <|
        Code.toSnippet source region Nothing <|
            ( D.reflow <|
                ("I am inferring a weird self-referential type for " ++ name ++ ":")
            , D.stack
                [ D.reflow <|
                    "Here is my best effort at writing down the type. You will see ∞ for parts of the type that repeat something already printed out infinitely."
                , D.indent 4 (D.dullyellow (T.toDoc localizer RT.None overallType))
                , D.reflowLink
                    "Staring at this type is usually not so helpful, so I recommend reading the hints at"
                    "infinite-type"
                    "to get unstuck!"
                ]
            )



-- ENCODERS and DECODERS


errorEncoder : T.CRET_Error -> Encode.Value
errorEncoder error =
    case error of
        T.CRET_BadExpr region category actualType expected ->
            Encode.object
                [ ( "type", Encode.string "BadExpr" )
                , ( "region", A.regionEncoder region )
                , ( "category", categoryEncoder category )
                , ( "actualType", T.typeEncoder actualType )
                , ( "expected", expectedEncoder T.typeEncoder expected )
                ]

        T.CRET_BadPattern region category tipe expected ->
            Encode.object
                [ ( "type", Encode.string "BadPattern" )
                , ( "region", A.regionEncoder region )
                , ( "category", pCategoryEncoder category )
                , ( "tipe", T.typeEncoder tipe )
                , ( "expected", pExpectedEncoder T.typeEncoder expected )
                ]

        T.CRET_InfiniteType region name overallType ->
            Encode.object
                [ ( "type", Encode.string "InfiniteType" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                , ( "overallType", T.typeEncoder overallType )
                ]


errorDecoder : Decode.Decoder T.CRET_Error
errorDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "BadExpr" ->
                        Decode.map4 T.CRET_BadExpr
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "category" categoryDecoder)
                            (Decode.field "actualType" T.typeDecoder)
                            (Decode.field "expected" (expectedDecoder T.typeDecoder))

                    "BadPattern" ->
                        Decode.map4 T.CRET_BadPattern
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "category" pCategoryDecoder)
                            (Decode.field "tipe" T.typeDecoder)
                            (Decode.field "expected" (pExpectedDecoder T.typeDecoder))

                    "InfiniteType" ->
                        Decode.map3 T.CRET_InfiniteType
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "overallType" T.typeDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Error's type: " ++ type_)
            )


categoryEncoder : T.CRET_Category -> Encode.Value
categoryEncoder category =
    case category of
        T.CRET_List ->
            Encode.object
                [ ( "type", Encode.string "List" )
                ]

        T.CRET_Number ->
            Encode.object
                [ ( "type", Encode.string "Number" )
                ]

        T.CRET_Float ->
            Encode.object
                [ ( "type", Encode.string "Float" )
                ]

        T.CRET_String ->
            Encode.object
                [ ( "type", Encode.string "String" )
                ]

        T.CRET_Char ->
            Encode.object
                [ ( "type", Encode.string "Char" )
                ]

        T.CRET_If ->
            Encode.object
                [ ( "type", Encode.string "If" )
                ]

        T.CRET_Case ->
            Encode.object
                [ ( "type", Encode.string "Case" )
                ]

        T.CRET_CallResult maybeName ->
            Encode.object
                [ ( "type", Encode.string "CallResult" )
                , ( "maybeName", maybeNameEncoder maybeName )
                ]

        T.CRET_Lambda ->
            Encode.object
                [ ( "type", Encode.string "Lambda" )
                ]

        T.CRET_Accessor field ->
            Encode.object
                [ ( "type", Encode.string "Accessor" )
                , ( "field", Encode.string field )
                ]

        T.CRET_Access field ->
            Encode.object
                [ ( "type", Encode.string "Access" )
                , ( "field", Encode.string field )
                ]

        T.CRET_Record ->
            Encode.object
                [ ( "type", Encode.string "Record" )
                ]

        T.CRET_Tuple ->
            Encode.object
                [ ( "type", Encode.string "Tuple" )
                ]

        T.CRET_Unit ->
            Encode.object
                [ ( "type", Encode.string "Unit" )
                ]

        T.CRET_Shader ->
            Encode.object
                [ ( "type", Encode.string "Shader" )
                ]

        T.CRET_Effects ->
            Encode.object
                [ ( "type", Encode.string "Effects" )
                ]

        T.CRET_Local name ->
            Encode.object
                [ ( "type", Encode.string "Local" )
                , ( "name", Encode.string name )
                ]

        T.CRET_Foreign name ->
            Encode.object
                [ ( "type", Encode.string "Foreign" )
                , ( "name", Encode.string name )
                ]


categoryDecoder : Decode.Decoder T.CRET_Category
categoryDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "List" ->
                        Decode.succeed T.CRET_List

                    "Number" ->
                        Decode.succeed T.CRET_Number

                    "Float" ->
                        Decode.succeed T.CRET_Float

                    "String" ->
                        Decode.succeed T.CRET_String

                    "Char" ->
                        Decode.succeed T.CRET_Char

                    "If" ->
                        Decode.succeed T.CRET_If

                    "Case" ->
                        Decode.succeed T.CRET_Case

                    "CallResult" ->
                        Decode.map T.CRET_CallResult (Decode.field "maybeName" maybeNameDecoder)

                    "Lambda" ->
                        Decode.succeed T.CRET_Lambda

                    "Accessor" ->
                        Decode.map T.CRET_Accessor (Decode.field "field" Decode.string)

                    "Access" ->
                        Decode.map T.CRET_Access (Decode.field "field" Decode.string)

                    "Record" ->
                        Decode.succeed T.CRET_Record

                    "Tuple" ->
                        Decode.succeed T.CRET_Tuple

                    "Unit" ->
                        Decode.succeed T.CRET_Unit

                    "Shader" ->
                        Decode.succeed T.CRET_Shader

                    "Effects" ->
                        Decode.succeed T.CRET_Effects

                    "Local" ->
                        Decode.map T.CRET_Local (Decode.field "name" Decode.string)

                    "Foreign" ->
                        Decode.map T.CRET_Foreign (Decode.field "name" Decode.string)

                    _ ->
                        Decode.fail ("Failed to decode Category's type: " ++ type_)
            )


expectedEncoder : (a -> Encode.Value) -> T.CRET_Expected a -> Encode.Value
expectedEncoder encoder expected =
    case expected of
        T.CRET_NoExpectation expectedType ->
            Encode.object
                [ ( "type", Encode.string "NoExpectation" )
                , ( "expectedType", encoder expectedType )
                ]

        T.CRET_FromContext region context expectedType ->
            Encode.object
                [ ( "type", Encode.string "FromContext" )
                , ( "region", A.regionEncoder region )
                , ( "context", contextEncoder context )
                , ( "expectedType", encoder expectedType )
                ]

        T.CRET_FromAnnotation name arity subContext expectedType ->
            Encode.object
                [ ( "type", Encode.string "FromAnnotation" )
                , ( "name", Encode.string name )
                , ( "arity", Encode.int arity )
                , ( "subContext", subContextEncoder subContext )
                , ( "expectedType", encoder expectedType )
                ]


expectedDecoder : Decode.Decoder a -> Decode.Decoder (T.CRET_Expected a)
expectedDecoder decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "NoExpectation" ->
                        Decode.map T.CRET_NoExpectation
                            (Decode.field "expectedType" decoder)

                    "FromContext" ->
                        Decode.map3 T.CRET_FromContext
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "context" contextDecoder)
                            (Decode.field "expectedType" decoder)

                    "FromAnnotation" ->
                        Decode.map4 T.CRET_FromAnnotation
                            (Decode.field "name" Decode.string)
                            (Decode.field "arity" Decode.int)
                            (Decode.field "subContext" subContextDecoder)
                            (Decode.field "expectedType" decoder)

                    _ ->
                        Decode.fail ("Unknown Expected's type: " ++ type_)
            )


contextDecoder : Decode.Decoder T.CRET_Context
contextDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "ListEntry" ->
                        Decode.map T.CRET_ListEntry (Decode.field "index" Index.zeroBasedDecoder)

                    "Negate" ->
                        Decode.succeed T.CRET_Negate

                    "OpLeft" ->
                        Decode.map T.CRET_OpLeft (Decode.field "op" Decode.string)

                    "OpRight" ->
                        Decode.map T.CRET_OpRight (Decode.field "op" Decode.string)

                    "IfCondition" ->
                        Decode.succeed T.CRET_IfCondition

                    "IfBranch" ->
                        Decode.map T.CRET_IfBranch (Decode.field "index" Index.zeroBasedDecoder)

                    "CaseBranch" ->
                        Decode.map T.CRET_CaseBranch (Decode.field "index" Index.zeroBasedDecoder)

                    "CallArity" ->
                        Decode.map2 T.CRET_CallArity
                            (Decode.field "maybeFuncName" maybeNameDecoder)
                            (Decode.field "numGivenArgs" Decode.int)

                    "CallArg" ->
                        Decode.map2 T.CRET_CallArg
                            (Decode.field "maybeFuncName" maybeNameDecoder)
                            (Decode.field "index" Index.zeroBasedDecoder)

                    "RecordAccess" ->
                        Decode.map4 T.CRET_RecordAccess
                            (Decode.field "recordRegion" A.regionDecoder)
                            (Decode.field "maybeName" (Decode.nullable Decode.string))
                            (Decode.field "fieldRegion" A.regionDecoder)
                            (Decode.field "field" Decode.string)

                    "RecordUpdateKeys" ->
                        Decode.map2 T.CRET_RecordUpdateKeys
                            (Decode.field "record" Decode.string)
                            (Decode.field "expectedFields" (DecodeX.assocListDict identity Decode.string Can.fieldUpdateDecoder))

                    "RecordUpdateValue" ->
                        Decode.map T.CRET_RecordUpdateValue (Decode.field "field" Decode.string)

                    "Destructure" ->
                        Decode.succeed T.CRET_Destructure

                    _ ->
                        Decode.fail ("Unknown Context's type: " ++ type_)
            )


contextEncoder : T.CRET_Context -> Encode.Value
contextEncoder context =
    case context of
        T.CRET_ListEntry index ->
            Encode.object
                [ ( "type", Encode.string "ListEntry" )
                , ( "index", Index.zeroBasedEncoder index )
                ]

        T.CRET_Negate ->
            Encode.object
                [ ( "type", Encode.string "Negate" )
                ]

        T.CRET_OpLeft op ->
            Encode.object
                [ ( "type", Encode.string "OpLeft" )
                , ( "op", Encode.string op )
                ]

        T.CRET_OpRight op ->
            Encode.object
                [ ( "type", Encode.string "OpRight" )
                , ( "op", Encode.string op )
                ]

        T.CRET_IfCondition ->
            Encode.object
                [ ( "type", Encode.string "IfCondition" )
                ]

        T.CRET_IfBranch index ->
            Encode.object
                [ ( "type", Encode.string "IfBranch" )
                , ( "index", Index.zeroBasedEncoder index )
                ]

        T.CRET_CaseBranch index ->
            Encode.object
                [ ( "type", Encode.string "CaseBranch" )
                , ( "index", Index.zeroBasedEncoder index )
                ]

        T.CRET_CallArity maybeFuncName numGivenArgs ->
            Encode.object
                [ ( "type", Encode.string "CallArity" )
                , ( "maybeFuncName", maybeNameEncoder maybeFuncName )
                , ( "numGivenArgs", Encode.int numGivenArgs )
                ]

        T.CRET_CallArg maybeFuncName index ->
            Encode.object
                [ ( "type", Encode.string "CallArg" )
                , ( "maybeFuncName", maybeNameEncoder maybeFuncName )
                , ( "index", Index.zeroBasedEncoder index )
                ]

        T.CRET_RecordAccess recordRegion maybeName fieldRegion field ->
            Encode.object
                [ ( "type", Encode.string "RecordAccess" )
                , ( "recordRegion", A.regionEncoder recordRegion )
                , ( "maybeName", EncodeX.maybe Encode.string maybeName )
                , ( "fieldRegion", A.regionEncoder fieldRegion )
                , ( "field", Encode.string field )
                ]

        T.CRET_RecordUpdateKeys record expectedFields ->
            Encode.object
                [ ( "type", Encode.string "RecordUpdateKeys" )
                , ( "record", Encode.string record )
                , ( "expectedFields", EncodeX.assocListDict compare Encode.string Can.fieldUpdateEncoder expectedFields )
                ]

        T.CRET_RecordUpdateValue field ->
            Encode.object
                [ ( "type", Encode.string "RecordUpdateValue" )
                , ( "field", Encode.string field )
                ]

        T.CRET_Destructure ->
            Encode.object
                [ ( "type", Encode.string "Destructure" )
                ]


subContextDecoder : Decode.Decoder T.CRET_SubContext
subContextDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "TypedIfBranch" ->
                        Decode.map T.CRET_TypedIfBranch
                            (Decode.field "index" Index.zeroBasedDecoder)

                    "TypedCaseBranch" ->
                        Decode.map T.CRET_TypedCaseBranch
                            (Decode.field "index" Index.zeroBasedDecoder)

                    "TypedBody" ->
                        Decode.succeed T.CRET_TypedBody

                    _ ->
                        Decode.fail ("Unknown SubContext's type: " ++ type_)
            )


subContextEncoder : T.CRET_SubContext -> Encode.Value
subContextEncoder subContext =
    case subContext of
        T.CRET_TypedIfBranch index ->
            Encode.object
                [ ( "type", Encode.string "TypedIfBranch" )
                , ( "index", Index.zeroBasedEncoder index )
                ]

        T.CRET_TypedCaseBranch index ->
            Encode.object
                [ ( "type", Encode.string "TypedCaseBranch" )
                , ( "index", Index.zeroBasedEncoder index )
                ]

        T.CRET_TypedBody ->
            Encode.object
                [ ( "type", Encode.string "TypedBody" )
                ]


pCategoryEncoder : T.CRET_PCategory -> Encode.Value
pCategoryEncoder pCategory =
    case pCategory of
        T.CRET_PRecord ->
            Encode.object
                [ ( "type", Encode.string "PRecord" )
                ]

        T.CRET_PUnit ->
            Encode.object
                [ ( "type", Encode.string "PUnit" )
                ]

        T.CRET_PTuple ->
            Encode.object
                [ ( "type", Encode.string "PTuple" )
                ]

        T.CRET_PList ->
            Encode.object
                [ ( "type", Encode.string "PList" )
                ]

        T.CRET_PCtor name ->
            Encode.object
                [ ( "type", Encode.string "PCtor" )
                , ( "name", Encode.string name )
                ]

        T.CRET_PInt ->
            Encode.object
                [ ( "type", Encode.string "PInt" )
                ]

        T.CRET_PStr ->
            Encode.object
                [ ( "type", Encode.string "PStr" )
                ]

        T.CRET_PChr ->
            Encode.object
                [ ( "type", Encode.string "PChr" )
                ]

        T.CRET_PBool ->
            Encode.object
                [ ( "type", Encode.string "PBool" )
                ]


pCategoryDecoder : Decode.Decoder T.CRET_PCategory
pCategoryDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "PRecord" ->
                        Decode.succeed T.CRET_PRecord

                    "PUnit" ->
                        Decode.succeed T.CRET_PUnit

                    "PTuple" ->
                        Decode.succeed T.CRET_PTuple

                    "PList" ->
                        Decode.succeed T.CRET_PList

                    "PCtor" ->
                        Decode.map T.CRET_PCtor (Decode.field "name" Decode.string)

                    "PInt" ->
                        Decode.succeed T.CRET_PInt

                    "PStr" ->
                        Decode.succeed T.CRET_PStr

                    "PChr" ->
                        Decode.succeed T.CRET_PChr

                    "PBool" ->
                        Decode.succeed T.CRET_PBool

                    _ ->
                        Decode.fail ("Unknown PCategory's type: " ++ type_)
            )


pExpectedEncoder : (a -> Encode.Value) -> T.CRET_PExpected a -> Encode.Value
pExpectedEncoder encoder pExpected =
    case pExpected of
        T.CRET_PNoExpectation expectedType ->
            Encode.object
                [ ( "type", Encode.string "PNoExpectation" )
                , ( "expectedType", encoder expectedType )
                ]

        T.CRET_PFromContext region context expectedType ->
            Encode.object
                [ ( "type", Encode.string "PFromContext" )
                , ( "region", A.regionEncoder region )
                , ( "context", pContextEncoder context )
                , ( "expectedType", encoder expectedType )
                ]


pExpectedDecoder : Decode.Decoder a -> Decode.Decoder (T.CRET_PExpected a)
pExpectedDecoder decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "PNoExpectation" ->
                        Decode.map T.CRET_PNoExpectation (Decode.field "expectedType" decoder)

                    --     | PFromContext T.CRA_Region PContext tipe
                    "PFromContext" ->
                        Decode.map3 T.CRET_PFromContext
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "context" pContextDecoder)
                            (Decode.field "expectedType" decoder)

                    _ ->
                        Decode.fail ("Failed to decode PExpected's type: " ++ type_)
            )


maybeNameEncoder : T.CRET_MaybeName -> Encode.Value
maybeNameEncoder maybeName =
    case maybeName of
        T.CRET_FuncName name ->
            Encode.object
                [ ( "type", Encode.string "FuncName" )
                , ( "name", Encode.string name )
                ]

        T.CRET_CtorName name ->
            Encode.object
                [ ( "type", Encode.string "CtorName" )
                , ( "name", Encode.string name )
                ]

        T.CRET_OpName op ->
            Encode.object
                [ ( "type", Encode.string "OpName" )
                , ( "op", Encode.string op )
                ]

        T.CRET_NoName ->
            Encode.object
                [ ( "type", Encode.string "NoName" )
                ]


maybeNameDecoder : Decode.Decoder T.CRET_MaybeName
maybeNameDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "FuncName" ->
                        Decode.map T.CRET_FuncName (Decode.field "name" Decode.string)

                    "CtorName" ->
                        Decode.map T.CRET_CtorName (Decode.field "name" Decode.string)

                    "OpName" ->
                        Decode.map T.CRET_OpName (Decode.field "op" Decode.string)

                    "NoName" ->
                        Decode.succeed T.CRET_NoName

                    _ ->
                        Decode.fail ("Failed to decode MaybeName's type: " ++ type_)
            )


pContextEncoder : T.CRET_PContext -> Encode.Value
pContextEncoder pContext =
    case pContext of
        T.CRET_PTypedArg name index ->
            Encode.object
                [ ( "type", Encode.string "PTypedArg" )
                , ( "name", Encode.string name )
                , ( "index", Index.zeroBasedEncoder index )
                ]

        T.CRET_PCaseMatch index ->
            Encode.object
                [ ( "type", Encode.string "PCaseMatch" )
                , ( "index", Index.zeroBasedEncoder index )
                ]

        T.CRET_PCtorArg name index ->
            Encode.object
                [ ( "type", Encode.string "PCtorArg" )
                , ( "name", Encode.string name )
                , ( "index", Index.zeroBasedEncoder index )
                ]

        T.CRET_PListEntry index ->
            Encode.object
                [ ( "type", Encode.string "PListEntry" )
                , ( "index", Index.zeroBasedEncoder index )
                ]

        T.CRET_PTail ->
            Encode.object
                [ ( "type", Encode.string "PTail" )
                ]


pContextDecoder : Decode.Decoder T.CRET_PContext
pContextDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "PTypedArg" ->
                        Decode.map2 T.CRET_PTypedArg
                            (Decode.field "name" Decode.string)
                            (Decode.field "index" Index.zeroBasedDecoder)

                    "PCaseMatch" ->
                        Decode.map T.CRET_PCaseMatch (Decode.field "index" Index.zeroBasedDecoder)

                    "PCtorArg" ->
                        Decode.map2 T.CRET_PCtorArg
                            (Decode.field "name" Decode.string)
                            (Decode.field "index" Index.zeroBasedDecoder)

                    "PListEntry" ->
                        Decode.map T.CRET_PListEntry (Decode.field "index" Index.zeroBasedDecoder)

                    "PTail" ->
                        Decode.succeed T.CRET_PTail

                    _ ->
                        Decode.fail ("Failed to decode PContext's type: " ++ type_)
            )
