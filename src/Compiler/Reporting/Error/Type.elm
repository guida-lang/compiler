module Compiler.Reporting.Error.Type exposing
    ( Category(..)
    , Context(..)
    , Error(..)
    , Expected(..)
    , MaybeName(..)
    , PCategory(..)
    , PContext(..)
    , PExpected(..)
    , SubContext(..)
    , errorDecoder
    , errorEncoder
    , ptypeReplace
    , toReport
    , typeReplace
    )

import Compiler.AST.Canonical as Can
import Compiler.Data.Index as Index
import Compiler.Data.Name exposing (Name)
import Compiler.Generate.Target as Target exposing (Target)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Compiler.Reporting.Report as Report
import Compiler.Reporting.Suggest as Suggest
import Compiler.Type.Error as T
import Data.Map as Dict exposing (Dict)
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



-- ERRORS


type Error
    = BadExpr A.Region Category T.Type (Expected T.Type)
    | BadPattern Target A.Region PCategory T.Type (PExpected T.Type)
    | InfiniteType Target A.Region Name T.Type



-- EXPRESSION EXPECTATIONS


type Expected tipe
    = NoExpectation Target tipe
    | FromContext Target A.Region Context tipe
    | FromAnnotation Target Name Int SubContext tipe


type Context
    = ListEntry Index.ZeroBased
    | Negate
    | OpLeft Name
    | OpRight Name
    | IfCondition
    | IfBranch Index.ZeroBased
    | CaseBranch Index.ZeroBased
    | CallArity MaybeName Int
    | CallArg MaybeName Index.ZeroBased
    | RecordAccess A.Region (Maybe Name) A.Region Name
    | RecordUpdateKeys (Dict String Name Can.FieldUpdate)
    | RecordUpdateValue Name
    | Destructure


type SubContext
    = TypedIfBranch Index.ZeroBased
    | TypedCaseBranch Index.ZeroBased
    | TypedBody


type MaybeName
    = FuncName Name
    | CtorName Name
    | OpName Name
    | NoName


type Category
    = List
    | Number
    | Float
    | String
    | Char
    | If
    | Case
    | CallResult MaybeName
    | Lambda
    | Accessor Name
    | Access Name
    | Record
    | Tuple
    | Unit
    | Shader
    | Effects
    | Local Name
    | Foreign Name



-- PATTERN EXPECTATIONS


type PExpected tipe
    = PNoExpectation tipe
    | PFromContext A.Region PContext tipe


type PContext
    = PTypedArg Name Index.ZeroBased
    | PCaseMatch Index.ZeroBased
    | PCtorArg Name Index.ZeroBased
    | PListEntry Index.ZeroBased
    | PTail


type PCategory
    = PRecord
    | PUnit
    | PTuple
    | PList
    | PCtor Name
    | PInt
    | PStr
    | PChr
    | PBool



-- HELPERS


typeReplace : Expected a -> b -> Expected b
typeReplace expectation tipe =
    case expectation of
        NoExpectation target _ ->
            NoExpectation target tipe

        FromContext target region context _ ->
            FromContext target region context tipe

        FromAnnotation target name arity context _ ->
            FromAnnotation target name arity context tipe


ptypeReplace : PExpected a -> b -> PExpected b
ptypeReplace expectation tipe =
    case expectation of
        PNoExpectation _ ->
            PNoExpectation tipe

        PFromContext region context _ ->
            PFromContext region context tipe



-- TO REPORT


toReport : Code.Source -> L.Localizer -> Error -> Report.Report
toReport source localizer err =
    case err of
        BadExpr region category actualType expected ->
            toExprReport source localizer region category actualType expected

        BadPattern target region category tipe expected ->
            toPatternReport target source localizer region category tipe expected

        InfiniteType target region name overallType ->
            toInfiniteReport target source localizer region name overallType



-- TO PATTERN REPORT


toPatternReport : Target -> Code.Source -> L.Localizer -> A.Region -> PCategory -> T.Type -> PExpected T.Type -> Report.Report
toPatternReport target source localizer patternRegion category tipe expected =
    Report.Report "TYPE MISMATCH" patternRegion [] <|
        case expected of
            PNoExpectation expectedType ->
                Code.toSnippet source patternRegion Nothing <|
                    ( D.fromChars "This pattern is being used in an unexpected way:"
                    , patternTypeComparison target
                        localizer
                        tipe
                        expectedType
                        (addPatternCategory "It is" category)
                        "But it needs to match:"
                        []
                    )

            PFromContext region context expectedType ->
                Code.toSnippet source region (Just patternRegion) <|
                    case context of
                        PTypedArg name index ->
                            ( D.reflow <|
                                "The "
                                    ++ D.ordinal index
                                    ++ " argument to `"
                                    ++ name
                                    ++ "` is weird."
                            , patternTypeComparison target
                                localizer
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

                        PCaseMatch index ->
                            if index == Index.first then
                                ( D.reflow <|
                                    "The 1st pattern in this `case` causing a mismatch:"
                                , patternTypeComparison target
                                    localizer
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
                                , patternTypeComparison target
                                    localizer
                                    tipe
                                    expectedType
                                    (addPatternCategory ("The " ++ D.ordinal index ++ " pattern is trying to match") category)
                                    "But all the previous patterns match:"
                                    [ D.link target
                                        "Note"
                                        "A `case` expression can only handle one type of value, so you may want to use"
                                        "custom-types"
                                        "to handle “mixing” types."
                                    ]
                                )

                        PCtorArg name index ->
                            ( D.reflow <|
                                "The "
                                    ++ D.ordinal index
                                    ++ " argument to `"
                                    ++ name
                                    ++ "` is weird."
                            , patternTypeComparison target
                                localizer
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

                        PListEntry index ->
                            ( D.reflow <|
                                "The "
                                    ++ D.ordinal index
                                    ++ " pattern in this list does not match all the previous ones:"
                            , patternTypeComparison target
                                localizer
                                tipe
                                expectedType
                                (addPatternCategory ("The " ++ D.ordinal index ++ " pattern is trying to match") category)
                                "But all the previous patterns in the list are:"
                                [ D.link target
                                    "Hint"
                                    "Everything in a list must be the same type of value. This way, we never run into unexpected values partway through a List.map, List.foldl, etc. Read"
                                    "custom-types"
                                    "to learn how to “mix” types."
                                ]
                            )

                        PTail ->
                            ( D.reflow <|
                                "The pattern after (::) is causing issues."
                            , patternTypeComparison target
                                localizer
                                tipe
                                expectedType
                                (addPatternCategory "The pattern after (::) is trying to match" category)
                                "But it needs to match lists like this:"
                                []
                            )



-- PATTERN HELPERS


patternTypeComparison : Target -> L.Localizer -> T.Type -> T.Type -> String -> String -> List D.Doc -> D.Doc
patternTypeComparison target localizer actual expected iAmSeeing insteadOf contextHints =
    let
        ( actualDoc, expectedDoc, problems ) =
            T.toComparison target localizer actual expected
    in
    D.stack <|
        [ D.reflow iAmSeeing
        , D.indent 4 actualDoc
        , D.reflow insteadOf
        , D.indent 4 expectedDoc
        ]
            ++ problemsToHint problems
            ++ contextHints


addPatternCategory : String -> PCategory -> String
addPatternCategory iAmTryingToMatch category =
    iAmTryingToMatch
        ++ (case category of
                PRecord ->
                    " record values of type:"

                PUnit ->
                    " unit values:"

                PTuple ->
                    " tuples of type:"

                PList ->
                    " lists of type:"

                PCtor name ->
                    " `" ++ name ++ "` values of type:"

                PInt ->
                    " integers:"

                PStr ->
                    " strings:"

                PChr ->
                    " characters:"

                PBool ->
                    " booleans:"
           )



-- EXPR HELPERS


typeComparison : Target -> L.Localizer -> T.Type -> T.Type -> String -> String -> List D.Doc -> D.Doc
typeComparison target localizer actual expected iAmSeeing insteadOf contextHints =
    let
        ( actualDoc, expectedDoc, problems ) =
            T.toComparison target localizer actual expected
    in
    D.stack <|
        [ D.reflow iAmSeeing
        , D.indent 4 actualDoc
        , D.reflow insteadOf
        , D.indent 4 expectedDoc
        ]
            ++ contextHints
            ++ problemsToHint problems


loneType : Target -> L.Localizer -> T.Type -> T.Type -> D.Doc -> List D.Doc -> D.Doc
loneType target localizer actual expected iAmSeeing furtherDetails =
    let
        ( actualDoc, _, problems ) =
            T.toComparison target localizer actual expected
    in
    D.stack <|
        [ iAmSeeing
        , D.indent 4 actualDoc
        ]
            ++ furtherDetails
            ++ problemsToHint problems


addCategory : String -> Category -> String
addCategory thisIs category =
    case category of
        Local name ->
            "This `" ++ name ++ "` value is a:"

        Foreign name ->
            "This `" ++ name ++ "` value is a:"

        Access field ->
            "The value at ." ++ field ++ " is a:"

        Accessor field ->
            "This ." ++ field ++ " field access function has type:"

        If ->
            "This `if` expression produces:"

        Case ->
            "This `case` expression produces:"

        List ->
            thisIs ++ " a list of type:"

        Number ->
            thisIs ++ " a number of type:"

        Float ->
            thisIs ++ " a float of type:"

        String ->
            thisIs ++ " a string of type:"

        Char ->
            thisIs ++ " a character of type:"

        Lambda ->
            thisIs ++ " an anonymous function of type:"

        Record ->
            thisIs ++ " a record of type:"

        Tuple ->
            thisIs ++ " a tuple of type:"

        Unit ->
            thisIs ++ " a unit value:"

        Shader ->
            thisIs ++ " a GLSL shader of type:"

        Effects ->
            thisIs ++ " a thing for CORE LIBRARIES ONLY."

        CallResult maybeName ->
            case maybeName of
                NoName ->
                    thisIs ++ ":"

                FuncName name ->
                    "This `" ++ name ++ "` call produces:"

                CtorName name ->
                    "This `" ++ name ++ "` call produces:"

                OpName _ ->
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
        T.IntFloat target ->
            [ D.fancyLink target
                "Note"
                [ D.fromChars "Read" ]
                "implicit-casts"
                [ D.fromChars "to"
                , D.fromChars "learn"
                , D.fromChars "why"
                , D.fromChars "Guida"
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
                "Guida does not have “truthiness” such that ints and strings and lists are automatically converted to booleans. Do that conversion explicitly!"
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

        T.BadFlexSuper target direction super tipe ->
            case tipe of
                T.Lambda _ _ _ ->
                    badFlexSuper target direction super tipe

                T.Infinite ->
                    []

                T.Error ->
                    []

                T.FlexVar _ ->
                    []

                T.FlexSuper s _ ->
                    badFlexFlexSuper super s

                T.RigidVar y ->
                    badRigidVar target y (toASuperThing super)

                T.RigidSuper s _ ->
                    badRigidSuper target s (toASuperThing super)

                T.Type _ _ _ ->
                    badFlexSuper target direction super tipe

                T.Record _ _ ->
                    badFlexSuper target direction super tipe

                T.Unit ->
                    badFlexSuper target direction super tipe

                T.Tuple _ _ _ ->
                    badFlexSuper target direction super tipe

                T.Alias _ _ _ _ ->
                    badFlexSuper target direction super tipe

        T.BadRigidVar target x tipe ->
            case tipe of
                T.Lambda _ _ _ ->
                    badRigidVar target x "a function"

                T.Infinite ->
                    []

                T.Error ->
                    []

                T.FlexVar _ ->
                    []

                T.FlexSuper s _ ->
                    badRigidVar target x (toASuperThing s)

                T.RigidVar y ->
                    badDoubleRigid target x y

                T.RigidSuper _ y ->
                    badDoubleRigid target x y

                T.Type _ n _ ->
                    badRigidVar target x ("a `" ++ n ++ "` value")

                T.Record _ _ ->
                    badRigidVar target x "a record"

                T.Unit ->
                    badRigidVar target x "a unit value"

                T.Tuple _ _ _ ->
                    badRigidVar target x "a tuple"

                T.Alias _ n _ _ ->
                    badRigidVar target x ("a `" ++ n ++ "` value")

        T.BadRigidSuper target super x tipe ->
            case tipe of
                T.Lambda _ _ _ ->
                    badRigidSuper target super "a function"

                T.Infinite ->
                    []

                T.Error ->
                    []

                T.FlexVar _ ->
                    []

                T.FlexSuper s _ ->
                    badRigidSuper target super (toASuperThing s)

                T.RigidVar y ->
                    badDoubleRigid target x y

                T.RigidSuper _ y ->
                    badDoubleRigid target x y

                T.Type _ n _ ->
                    badRigidSuper target super ("a `" ++ n ++ "` value")

                T.Record _ _ ->
                    badRigidSuper target super "a record"

                T.Unit ->
                    badRigidSuper target super "a unit value"

                T.Tuple _ _ _ ->
                    badRigidSuper target super "a tuple"

                T.Alias _ n _ _ ->
                    badRigidSuper target super ("a `" ++ n ++ "` value")

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


badRigidVar : Target -> Name -> String -> List D.Doc
badRigidVar target name aThing =
    [ D.toSimpleHint <|
        "Your type annotation uses type variable `"
            ++ name
            ++ "` which means ANY type of value can flow through, but your code is saying it specifically wants "
            ++ aThing
            ++ ". Maybe change your type annotation to be more specific? Maybe change the code to be more general?"
    , D.reflowLink target "Read" "type-annotations" "for more advice!"
    ]


badDoubleRigid : Target -> Name -> Name -> List D.Doc
badDoubleRigid target x y =
    [ D.toSimpleHint <|
        "Your type annotation uses `"
            ++ x
            ++ "` and `"
            ++ y
            ++ "` as separate type variables. Your code seems to be saying they are the same though. Maybe they should be the same in your type annotation? Maybe your code uses them in a weird way?"
    , D.reflowLink target "Read" "type-annotations" "for more advice!"
    ]


toASuperThing : T.Super -> String
toASuperThing super =
    case super of
        T.Number ->
            "a `number` value"

        T.Comparable ->
            "a `comparable` value"

        T.CompAppend ->
            "a `compappend` value"

        T.Appendable ->
            "an `appendable` value"



-- BAD SUPER HINTS


badFlexSuper : Target -> T.Direction -> T.Super -> T.Type -> List D.Doc
badFlexSuper target direction super tipe =
    case super of
        T.Comparable ->
            case tipe of
                T.Record _ _ ->
                    [ D.link target
                        "Hint"
                        "I do not know how to compare records. I can only compare ints, floats, chars, strings, lists of comparable values, and tuples of comparable values. Check out"
                        "comparing-records"
                        "for ideas on how to proceed."
                    ]

                T.Type _ name _ ->
                    [ D.toSimpleHint <|
                        "I do not know how to compare `"
                            ++ name
                            ++ "` values. I can only compare ints, floats, chars, strings, lists of comparable values, and tuples of comparable values."
                    , D.reflowLink target
                        "Check out"
                        "comparing-custom-types"
                        "for ideas on how to proceed."
                    ]

                _ ->
                    [ D.toSimpleHint <|
                        "I only know how to compare ints, floats, chars, strings, lists of comparable values, and tuples of comparable values."
                    ]

        T.Appendable ->
            [ D.toSimpleHint "I only know how to append strings and lists."
            ]

        T.CompAppend ->
            [ D.toSimpleHint "Only strings and lists are both comparable and appendable."
            ]

        T.Number ->
            case tipe of
                T.Type home name _ ->
                    if T.isString target home name then
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


badRigidSuper : Target -> T.Super -> String -> List D.Doc
badRigidSuper target super aThing =
    let
        ( superType, manyThings ) =
            case super of
                T.Number ->
                    ( "number", "ints AND floats" )

                T.Comparable ->
                    ( "comparable", "ints, floats, chars, strings, lists, and tuples" )

                T.Appendable ->
                    ( "appendable", "strings AND lists" )

                T.CompAppend ->
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
    , D.reflowLink target "Read" "type-annotations" "for more advice!"
    ]


badFlexFlexSuper : T.Super -> T.Super -> List D.Doc
badFlexFlexSuper s1 s2 =
    let
        likeThis : T.Super -> String
        likeThis super =
            case super of
                T.Number ->
                    "a number"

                T.Comparable ->
                    "comparable"

                T.CompAppend ->
                    "a compappend"

                T.Appendable ->
                    "appendable"
    in
    [ D.toSimpleHint <|
        "There are no values in Guida that are both "
            ++ likeThis s1
            ++ " and "
            ++ likeThis s2
            ++ "."
    ]



-- TO EXPR REPORT


toExprReport : Code.Source -> L.Localizer -> A.Region -> Category -> T.Type -> Expected T.Type -> Report.Report
toExprReport source localizer exprRegion category tipe expected =
    case expected of
        NoExpectation target expectedType ->
            Report.Report "TYPE MISMATCH" exprRegion [] <|
                Code.toSnippet source
                    exprRegion
                    Nothing
                    ( D.fromChars "This expression is being used in an unexpected way:"
                    , typeComparison target
                        localizer
                        tipe
                        expectedType
                        (addCategory "It is" category)
                        "But you are trying to use it as:"
                        []
                    )

        FromAnnotation target name _ subContext expectedType ->
            let
                thing : String
                thing =
                    case subContext of
                        TypedIfBranch index ->
                            D.ordinal index ++ " branch of this `if` expression:"

                        TypedCaseBranch index ->
                            D.ordinal index ++ " branch of this `case` expression:"

                        TypedBody ->
                            "body of the `" ++ name ++ "` definition:"

                itIs : String
                itIs =
                    case subContext of
                        TypedIfBranch index ->
                            "The " ++ D.ordinal index ++ " branch is"

                        TypedCaseBranch index ->
                            "The " ++ D.ordinal index ++ " branch is"

                        TypedBody ->
                            "The body is"
            in
            Report.Report "TYPE MISMATCH" exprRegion [] <|
                Code.toSnippet source exprRegion Nothing <|
                    ( D.reflow ("Something is off with the " ++ thing)
                    , typeComparison target
                        localizer
                        tipe
                        expectedType
                        (addCategory itIs category)
                        ("But the type annotation on `" ++ name ++ "` says it should be:")
                        []
                    )

        FromContext target region context expectedType ->
            let
                mismatch : ( ( Maybe A.Region, String ), ( String, String, List D.Doc ) ) -> Report.Report
                mismatch ( ( maybeHighlight, problem ), ( thisIs, insteadOf, furtherDetails ) ) =
                    Report.Report "TYPE MISMATCH" exprRegion [] <|
                        Code.toSnippet source
                            region
                            maybeHighlight
                            ( D.reflow problem
                            , typeComparison target localizer tipe expectedType (addCategory thisIs category) insteadOf furtherDetails
                            )

                badType : ( ( Maybe A.Region, String ), ( String, List D.Doc ) ) -> Report.Report
                badType ( ( maybeHighlight, problem ), ( thisIs, furtherDetails ) ) =
                    Report.Report "TYPE MISMATCH" exprRegion [] <|
                        Code.toSnippet source
                            region
                            maybeHighlight
                            ( D.reflow problem
                            , loneType target localizer tipe expectedType (D.reflow (addCategory thisIs category)) furtherDetails
                            )

                custom : Maybe A.Region -> ( D.Doc, D.Doc ) -> Report.Report
                custom maybeHighlight docPair =
                    Report.Report "TYPE MISMATCH" exprRegion [] <|
                        Code.toSnippet source region maybeHighlight docPair
            in
            case context of
                ListEntry index ->
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
                          , [ D.link target
                                "Hint"
                                "Everything in a list must be the same type of value. This way, we never run into unexpected values partway through a List.map, List.foldl, etc. Read"
                                "custom-types"
                                "to learn how to “mix” types."
                            ]
                          )
                        )

                Negate ->
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

                OpLeft op ->
                    custom (Just exprRegion) <|
                        opLeftToDocs target localizer category op tipe expectedType

                OpRight op ->
                    case opRightToDocs target localizer category op tipe expectedType of
                        EmphBoth details ->
                            custom Nothing details

                        EmphRight details ->
                            custom (Just exprRegion) details

                IfCondition ->
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

                IfBranch index ->
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
                          , [ D.link target
                                "Hint"
                                "All branches in an `if` must produce the same type of values. This way, no matter which branch we take, the result is always a consistent shape. Read"
                                "custom-types"
                                "to learn how to “mix” types."
                            ]
                          )
                        )

                CaseBranch index ->
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
                          , [ D.link target
                                "Hint"
                                "All branches in a `case` must produce the same type of values. This way, no matter which branch we take, the result is always a consistent shape. Read"
                                "custom-types"
                                "to learn how to “mix” types."
                            ]
                          )
                        )

                CallArity maybeFuncName numGivenArgs ->
                    Report.Report "TOO MANY ARGS" exprRegion [] <|
                        Code.toSnippet source region (Just exprRegion) <|
                            case countArgs tipe of
                                0 ->
                                    let
                                        thisValue : String
                                        thisValue =
                                            case maybeFuncName of
                                                NoName ->
                                                    "This value"

                                                FuncName name ->
                                                    "The `" ++ name ++ "` value"

                                                CtorName name ->
                                                    "The `" ++ name ++ "` value"

                                                OpName op ->
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
                                                NoName ->
                                                    "This function"

                                                FuncName name ->
                                                    "The `" ++ name ++ "` function"

                                                CtorName name ->
                                                    "The `" ++ name ++ "` constructor"

                                                OpName op ->
                                                    "The (" ++ op ++ ") operator"
                                    in
                                    ( D.reflow <| thisFunction ++ " expects " ++ D.args n ++ ", but it got " ++ String.fromInt numGivenArgs ++ " instead."
                                    , D.reflow <| "Are there any missing commas? Or missing parentheses?"
                                    )

                CallArg maybeFuncName index ->
                    let
                        ith : String
                        ith =
                            D.ordinal index

                        thisFunction : String
                        thisFunction =
                            case maybeFuncName of
                                NoName ->
                                    "this function"

                                FuncName name ->
                                    "`" ++ name ++ "`"

                                CtorName name ->
                                    "`" ++ name ++ "`"

                                OpName op ->
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

                RecordAccess recordRegion maybeName fieldRegion field ->
                    case T.iteratedDealias tipe of
                        T.Record fields ext ->
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
                                            , toNearbyRecord target localizer f fs ext
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

                RecordUpdateKeys expectedFields ->
                    case T.iteratedDealias tipe of
                        T.Record actualFields ext ->
                            case List.sortBy Tuple.first (Dict.toList compare (Dict.diff expectedFields actualFields)) of
                                [] ->
                                    mismatch
                                        ( ( Nothing
                                          , "Something is off with this record update:"
                                          )
                                        , ( "The record is"
                                          , "But this update needs it to be compatable with:"
                                          , [ D.reflow
                                                "Do you mind creating an <http://sscce.org/> that produces this error message and sharing it at <https://github.com/guida-lang/compiler/issues> so we can try to give better advice here?"
                                            ]
                                          )
                                        )

                                ( field, Can.FieldUpdate fieldRegion _ ) :: _ ->
                                    let
                                        fStr : String
                                        fStr =
                                            "`" ++ field ++ "`"
                                    in
                                    custom (Just fieldRegion)
                                        ( D.reflow <|
                                            "The record does not have a "
                                                ++ fStr
                                                ++ " field:"
                                        , case Suggest.sort field Tuple.first (Dict.toList compare actualFields) of
                                            [] ->
                                                D.reflow <| "In fact, it is a record with NO fields!"

                                            f :: fs ->
                                                D.stack
                                                    [ D.reflow <|
                                                        "This is usually a typo. Here are the record fields that are most similar:"
                                                    , toNearbyRecord target localizer f fs ext
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

                RecordUpdateValue field ->
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

                Destructure ->
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


countArgs : T.Type -> Int
countArgs tipe =
    case tipe of
        T.Lambda _ _ stuff ->
            1 + List.length stuff

        _ ->
            0



-- FIELD NAME HELPERS


toNearbyRecord : Target -> L.Localizer -> ( Name, T.Type ) -> List ( Name, T.Type ) -> T.Extension -> D.Doc
toNearbyRecord target localizer f fs ext =
    D.indent 4 <|
        if List.length fs <= 3 then
            RT.vrecord (List.map (fieldToDocs target localizer) (f :: fs)) (extToDoc ext)

        else
            RT.vrecordSnippet (fieldToDocs target localizer f) (List.map (fieldToDocs target localizer) (List.take 3 fs))


fieldToDocs : Target -> L.Localizer -> ( Name, T.Type ) -> ( D.Doc, D.Doc )
fieldToDocs target localizer ( name, tipe ) =
    ( D.fromName name
    , T.toDoc target localizer RT.None tipe
    )


extToDoc : T.Extension -> Maybe D.Doc
extToDoc ext =
    case ext of
        T.Closed ->
            Nothing

        T.FlexOpen x ->
            Just (D.fromName x)

        T.RigidOpen x ->
            Just (D.fromName x)



-- OP LEFT


opLeftToDocs : Target -> L.Localizer -> Category -> Name -> T.Type -> T.Type -> ( D.Doc, D.Doc )
opLeftToDocs target localizer category op tipe expected =
    case op of
        "+" ->
            if isString target tipe then
                badStringAdd

            else if isList target tipe then
                badListAdd target localizer category "left" tipe expected

            else
                badMath target localizer category "Addition" "left" "+" tipe expected []

        "*" ->
            if isList target tipe then
                badListMul target localizer category "left" tipe expected

            else
                badMath target localizer category "Multiplication" "left" "*" tipe expected []

        "-" ->
            badMath target localizer category "Subtraction" "left" "-" tipe expected []

        "^" ->
            badMath target localizer category "Exponentiation" "left" "^" tipe expected []

        "/" ->
            badFDiv target localizer (D.fromChars "left") tipe expected

        "//" ->
            badIDiv target localizer (D.fromChars "left") tipe expected

        "&&" ->
            badBool target localizer (D.fromChars "&&") (D.fromChars "left") tipe expected

        "||" ->
            badBool target localizer (D.fromChars "||") (D.fromChars "left") tipe expected

        "<" ->
            badCompLeft target localizer category "<" "left" tipe expected

        ">" ->
            badCompLeft target localizer category ">" "left" tipe expected

        "<=" ->
            badCompLeft target localizer category "<=" "left" tipe expected

        ">=" ->
            badCompLeft target localizer category ">=" "left" tipe expected

        "++" ->
            badAppendLeft target localizer category tipe expected

        "<|" ->
            ( D.fromChars "The left side of (<|) needs to be a function so I can pipe arguments to it!"
            , loneType target
                localizer
                tipe
                expected
                (D.reflow (addCategory "I am seeing" category))
                [ D.reflow "This needs to be some kind of function though!" ]
            )

        _ ->
            ( D.reflow ("The left argument of (" ++ op ++ ") is causing problems:")
            , typeComparison target
                localizer
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


opRightToDocs : Target -> L.Localizer -> Category -> Name -> T.Type -> T.Type -> RightDocs
opRightToDocs target localizer category op tipe expected =
    case op of
        "+" ->
            if isFloat target expected && isInt target tipe then
                badCast target op FloatInt

            else if isInt target expected && isFloat target tipe then
                badCast target op IntFloat

            else if isString target tipe then
                EmphRight badStringAdd

            else if isList target tipe then
                EmphRight (badListAdd target localizer category "right" tipe expected)

            else
                EmphRight (badMath target localizer category "Addition" "right" "+" tipe expected [])

        "*" ->
            if isFloat target expected && isInt target tipe then
                badCast target op FloatInt

            else if isInt target expected && isFloat target tipe then
                badCast target op IntFloat

            else if isList target tipe then
                EmphRight (badListMul target localizer category "right" tipe expected)

            else
                EmphRight (badMath target localizer category "Multiplication" "right" "*" tipe expected [])

        "-" ->
            if isFloat target expected && isInt target tipe then
                badCast target op FloatInt

            else if isInt target expected && isFloat target tipe then
                badCast target op IntFloat

            else
                EmphRight (badMath target localizer category "Subtraction" "right" "-" tipe expected [])

        "^" ->
            if isFloat target expected && isInt target tipe then
                badCast target op FloatInt

            else if isInt target expected && isFloat target tipe then
                badCast target op IntFloat

            else
                EmphRight (badMath target localizer category "Exponentiation" "right" "^" tipe expected [])

        "/" ->
            EmphRight (badFDiv target localizer (D.fromChars "right") tipe expected)

        "//" ->
            EmphRight (badIDiv target localizer (D.fromChars "right") tipe expected)

        "&&" ->
            EmphRight (badBool target localizer (D.fromChars "&&") (D.fromChars "right") tipe expected)

        "||" ->
            EmphRight (badBool target localizer (D.fromChars "||") (D.fromChars "right") tipe expected)

        "<" ->
            badCompRight target localizer "<" tipe expected

        ">" ->
            badCompRight target localizer ">" tipe expected

        "<=" ->
            badCompRight target localizer "<=" tipe expected

        ">=" ->
            badCompRight target localizer ">=" tipe expected

        "==" ->
            badEquality target localizer "==" tipe expected

        "/=" ->
            badEquality target localizer "/=" tipe expected

        "::" ->
            badConsRight target localizer category tipe expected

        "++" ->
            badAppendRight target localizer category tipe expected

        "<|" ->
            EmphRight
                ( D.reflow "I cannot send this through the (<|) pipe:"
                , typeComparison target
                    localizer
                    tipe
                    expected
                    "The argument is:"
                    "But (<|) is piping it to a function that expects:"
                    []
                )

        "|>" ->
            case ( tipe, expected ) of
                ( T.Lambda expectedArgType _ _, T.Lambda argType _ _ ) ->
                    EmphRight
                        ( D.reflow "This function cannot handle the argument sent through the (|>) pipe:"
                        , typeComparison target
                            localizer
                            argType
                            expectedArgType
                            "The argument is:"
                            "But (|>) is piping it to a function that expects:"
                            []
                        )

                _ ->
                    EmphRight
                        ( D.reflow "The right side of (|>) needs to be a function so I can pipe arguments to it!"
                        , loneType target
                            localizer
                            tipe
                            expected
                            (D.reflow (addCategory "But instead of a function, I am seeing" category))
                            []
                        )

        _ ->
            badOpRightFallback target localizer category op tipe expected


badOpRightFallback : Target -> L.Localizer -> Category -> Name -> T.Type -> T.Type -> RightDocs
badOpRightFallback target localizer category op tipe expected =
    EmphRight
        ( D.reflow ("The right argument of (" ++ op ++ ") is causing problems.")
        , typeComparison target
            localizer
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


isInt : Target -> T.Type -> Bool
isInt target tipe =
    case tipe of
        T.Type home name [] ->
            T.isInt target home name

        _ ->
            False


isFloat : Target -> T.Type -> Bool
isFloat target tipe =
    case tipe of
        T.Type home name [] ->
            T.isFloat target home name

        _ ->
            False


isString : Target -> T.Type -> Bool
isString target tipe =
    case tipe of
        T.Type home name [] ->
            T.isString target home name

        _ ->
            False


isList : Target -> T.Type -> Bool
isList target tipe =
    case tipe of
        T.Type home name [ _ ] ->
            T.isList target home name

        _ ->
            False



-- BAD CONS


badConsRight : Target -> L.Localizer -> Category -> T.Type -> T.Type -> RightDocs
badConsRight target localizer category tipe expected =
    case tipe of
        T.Type home1 name1 [ actualElement ] ->
            if T.isList target home1 name1 then
                case expected of
                    T.Type home2 name2 [ expectedElement ] ->
                        if T.isList target home2 name2 then
                            EmphBoth
                                ( D.reflow "I am having trouble with this (::) operator:"
                                , typeComparison target
                                    localizer
                                    expectedElement
                                    actualElement
                                    "The left side of (::) is:"
                                    "But you are trying to put that into a list filled with:"
                                    (case expectedElement of
                                        T.Type home name [ _ ] ->
                                            if T.isList target home name then
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
                            badOpRightFallback target localizer category "::" tipe expected

                    _ ->
                        badOpRightFallback target localizer category "::" tipe expected

            else
                EmphRight
                    ( D.reflow "The (::) operator can only add elements onto lists."
                    , loneType target
                        localizer
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
                , loneType target
                    localizer
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


toAppendType : Target -> T.Type -> AppendType
toAppendType target tipe =
    case tipe of
        T.Type home name _ ->
            if T.isInt target home name then
                ANumber (D.fromChars "Int") (D.fromChars "String.fromInt")

            else if T.isFloat target home name then
                ANumber (D.fromChars "Float") (D.fromChars "String.fromFloat")

            else if T.isString target home name then
                AString

            else if T.isList target home name then
                AList

            else
                AOther

        T.FlexSuper T.Number _ ->
            ANumber (D.fromChars "number") (D.fromChars "String.fromInt")

        _ ->
            AOther


badAppendLeft : Target -> L.Localizer -> Category -> T.Type -> T.Type -> ( D.Doc, D.Doc )
badAppendLeft target localizer category tipe expected =
    case toAppendType target tipe of
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
            , loneType target
                localizer
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


badAppendRight : Target -> L.Localizer -> Category -> T.Type -> T.Type -> RightDocs
badAppendRight target localizer category tipe expected =
    case ( toAppendType target expected, toAppendType target tipe ) of
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
                , typeComparison target
                    localizer
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


badCast : Target -> Name -> ThisThenThat -> RightDocs
badCast target op thisThenThat =
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
                badCastHelp target aFloat anInt round toFloat

            IntFloat ->
                badCastHelp target anInt aFloat toFloat round
        )


badCastHelp : Target -> List D.Doc -> List D.Doc -> D.Doc -> D.Doc -> D.Doc
badCastHelp target anInt aFloat toFloat round =
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
        , D.link target "Note" "Read" "implicit-casts" "to learn why Guida does not implicitly convert Ints to Floats."
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


badListAdd : Target -> L.Localizer -> Category -> String -> T.Type -> T.Type -> ( D.Doc, D.Doc )
badListAdd target localizer category direction tipe expected =
    ( D.fromChars "I cannot do addition with lists:"
    , loneType target
        localizer
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


badListMul : Target -> L.Localizer -> Category -> String -> T.Type -> T.Type -> ( D.Doc, D.Doc )
badListMul target localizer category direction tipe expected =
    badMath target localizer category "Multiplication" direction "*" tipe expected <|
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


badMath : Target -> L.Localizer -> Category -> String -> String -> String -> T.Type -> T.Type -> List D.Doc -> ( D.Doc, D.Doc )
badMath target localizer category operation direction op tipe expected otherHints =
    ( D.reflow <|
        operation
            ++ " does not work with this value:"
    , loneType target
        localizer
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


badFDiv : Target -> L.Localizer -> D.Doc -> T.Type -> T.Type -> ( D.Doc, D.Doc )
badFDiv target localizer direction tipe expected =
    ( D.reflow "The (/) operator is specifically for floating-point division:"
    , if isInt target tipe then
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
            , D.link target "Note" "Read" "implicit-casts" "to learn why Guida does not implicitly convert Ints to Floats."
            ]

      else
        loneType target
            localizer
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


badIDiv : Target -> L.Localizer -> D.Doc -> T.Type -> T.Type -> ( D.Doc, D.Doc )
badIDiv target localizer direction tipe expected =
    ( D.reflow "The (//) operator is specifically for integer division:"
    , if isFloat target tipe then
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
            , D.link target "Note" "Read" "implicit-casts" "to learn why Guida does not implicitly convert Ints to Floats."
            ]

      else
        loneType target
            localizer
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


badBool : Target -> L.Localizer -> D.Doc -> D.Doc -> T.Type -> T.Type -> ( D.Doc, D.Doc )
badBool target localizer op direction tipe expected =
    ( D.reflow "I am struggling with this boolean operation:"
    , loneType target
        localizer
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


badCompLeft : Target -> L.Localizer -> Category -> String -> String -> T.Type -> T.Type -> ( D.Doc, D.Doc )
badCompLeft target localizer category op direction tipe expected =
    ( D.reflow "I cannot do a comparison with this value:"
    , loneType target
        localizer
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


badCompRight : Target -> L.Localizer -> String -> T.Type -> T.Type -> RightDocs
badCompRight target localizer op tipe expected =
    EmphBoth
        ( D.reflow <|
            ("I need both sides of (" ++ op ++ ") to be the same type:")
        , typeComparison target
            localizer
            expected
            tipe
            ("The left side of (" ++ op ++ ") is:")
            "But the right side is:"
            [ D.reflow <|
                ("I cannot compare different types though! Which side of (" ++ op ++ ") is the problem?")
            ]
        )



-- BAD EQUALITY


badEquality : Target -> L.Localizer -> String -> T.Type -> T.Type -> RightDocs
badEquality target localizer op tipe expected =
    EmphBoth
        ( D.reflow <|
            ("I need both sides of (" ++ op ++ ") to be the same type:")
        , typeComparison target
            localizer
            expected
            tipe
            ("The left side of (" ++ op ++ ") is:")
            "But the right side is:"
            [ if isFloat target tipe || isFloat target expected then
                D.toSimpleNote <|
                    "Equality on floats is not 100% reliable due to the design of IEEE 754. I recommend a check like (abs (x - y) < 0.0001) instead."

              else
                D.reflow "Different types can never be equal though! Which side is messed up?"
            ]
        )



-- INFINITE TYPES


toInfiniteReport : Target -> Code.Source -> L.Localizer -> A.Region -> Name -> T.Type -> Report.Report
toInfiniteReport target source localizer region name overallType =
    Report.Report "INFINITE TYPE" region [] <|
        Code.toSnippet source region Nothing <|
            ( D.reflow <|
                ("I am inferring a weird self-referential type for " ++ name ++ ":")
            , D.stack
                [ D.reflow <|
                    "Here is my best effort at writing down the type. You will see ∞ for parts of the type that repeat something already printed out infinitely."
                , D.indent 4 (D.dullyellow (T.toDoc target localizer RT.None overallType))
                , D.reflowLink target
                    "Staring at this type is usually not so helpful, so I recommend reading the hints at"
                    "infinite-type"
                    "to get unstuck!"
                ]
            )



-- ENCODERS and DECODERS


errorEncoder : Error -> BE.Encoder
errorEncoder error =
    case error of
        BadExpr region category actualType expected ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.regionEncoder region
                , categoryEncoder category
                , T.typeEncoder actualType
                , expectedEncoder T.typeEncoder expected
                ]

        BadPattern target region category tipe expected ->
            BE.sequence
                [ BE.unsignedInt8 1
                , Target.encoder target
                , A.regionEncoder region
                , pCategoryEncoder category
                , T.typeEncoder tipe
                , pExpectedEncoder T.typeEncoder expected
                ]

        InfiniteType target region name overallType ->
            BE.sequence
                [ BE.unsignedInt8 2
                , Target.encoder target
                , A.regionEncoder region
                , BE.string name
                , T.typeEncoder overallType
                ]


errorDecoder : BD.Decoder Error
errorDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map4 BadExpr
                            A.regionDecoder
                            categoryDecoder
                            T.typeDecoder
                            (expectedDecoder T.typeDecoder)

                    1 ->
                        BD.map5 BadPattern
                            Target.decoder
                            A.regionDecoder
                            pCategoryDecoder
                            T.typeDecoder
                            (pExpectedDecoder T.typeDecoder)

                    2 ->
                        BD.map4 InfiniteType
                            Target.decoder
                            A.regionDecoder
                            BD.string
                            T.typeDecoder

                    _ ->
                        BD.fail
            )


categoryEncoder : Category -> BE.Encoder
categoryEncoder category =
    case category of
        List ->
            BE.unsignedInt8 0

        Number ->
            BE.unsignedInt8 1

        Float ->
            BE.unsignedInt8 2

        String ->
            BE.unsignedInt8 3

        Char ->
            BE.unsignedInt8 4

        If ->
            BE.unsignedInt8 5

        Case ->
            BE.unsignedInt8 6

        CallResult maybeName ->
            BE.sequence
                [ BE.unsignedInt8 7
                , maybeNameEncoder maybeName
                ]

        Lambda ->
            BE.unsignedInt8 8

        Accessor field ->
            BE.sequence
                [ BE.unsignedInt8 9
                , BE.string field
                ]

        Access field ->
            BE.sequence
                [ BE.unsignedInt8 10
                , BE.string field
                ]

        Record ->
            BE.unsignedInt8 11

        Tuple ->
            BE.unsignedInt8 12

        Unit ->
            BE.unsignedInt8 13

        Shader ->
            BE.unsignedInt8 14

        Effects ->
            BE.unsignedInt8 15

        Local name ->
            BE.sequence
                [ BE.unsignedInt8 16
                , BE.string name
                ]

        Foreign name ->
            BE.sequence
                [ BE.unsignedInt8 17
                , BE.string name
                ]


categoryDecoder : BD.Decoder Category
categoryDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed List

                    1 ->
                        BD.succeed Number

                    2 ->
                        BD.succeed Float

                    3 ->
                        BD.succeed String

                    4 ->
                        BD.succeed Char

                    5 ->
                        BD.succeed If

                    6 ->
                        BD.succeed Case

                    7 ->
                        BD.map CallResult maybeNameDecoder

                    8 ->
                        BD.succeed Lambda

                    9 ->
                        BD.map Accessor BD.string

                    10 ->
                        BD.map Access BD.string

                    11 ->
                        BD.succeed Record

                    12 ->
                        BD.succeed Tuple

                    13 ->
                        BD.succeed Unit

                    14 ->
                        BD.succeed Shader

                    15 ->
                        BD.succeed Effects

                    16 ->
                        BD.map Local BD.string

                    17 ->
                        BD.map Foreign BD.string

                    _ ->
                        BD.fail
            )


expectedEncoder : (a -> BE.Encoder) -> Expected a -> BE.Encoder
expectedEncoder encoder expected =
    case expected of
        NoExpectation target expectedType ->
            BE.sequence
                [ BE.unsignedInt8 0
                , Target.encoder target
                , encoder expectedType
                ]

        FromContext target region context expectedType ->
            BE.sequence
                [ BE.unsignedInt8 1
                , Target.encoder target
                , A.regionEncoder region
                , contextEncoder context
                , encoder expectedType
                ]

        FromAnnotation target name arity subContext expectedType ->
            BE.sequence
                [ BE.unsignedInt8 2
                , Target.encoder target
                , BE.string name
                , BE.int arity
                , subContextEncoder subContext
                , encoder expectedType
                ]


expectedDecoder : BD.Decoder a -> BD.Decoder (Expected a)
expectedDecoder decoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map2 NoExpectation
                            Target.decoder
                            decoder

                    1 ->
                        BD.map4 FromContext
                            Target.decoder
                            A.regionDecoder
                            contextDecoder
                            decoder

                    2 ->
                        BD.map5 FromAnnotation
                            Target.decoder
                            BD.string
                            BD.int
                            subContextDecoder
                            decoder

                    _ ->
                        BD.fail
            )


contextEncoder : Context -> BE.Encoder
contextEncoder context =
    case context of
        ListEntry index ->
            BE.sequence
                [ BE.unsignedInt8 0
                , Index.zeroBasedEncoder index
                ]

        Negate ->
            BE.unsignedInt8 1

        OpLeft op ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.string op
                ]

        OpRight op ->
            BE.sequence
                [ BE.unsignedInt8 3
                , BE.string op
                ]

        IfCondition ->
            BE.unsignedInt8 4

        IfBranch index ->
            BE.sequence
                [ BE.unsignedInt8 5
                , Index.zeroBasedEncoder index
                ]

        CaseBranch index ->
            BE.sequence
                [ BE.unsignedInt8 6
                , Index.zeroBasedEncoder index
                ]

        CallArity maybeFuncName numGivenArgs ->
            BE.sequence
                [ BE.unsignedInt8 7
                , maybeNameEncoder maybeFuncName
                , BE.int numGivenArgs
                ]

        CallArg maybeFuncName index ->
            BE.sequence
                [ BE.unsignedInt8 8
                , maybeNameEncoder maybeFuncName
                , Index.zeroBasedEncoder index
                ]

        RecordAccess recordRegion maybeName fieldRegion field ->
            BE.sequence
                [ BE.unsignedInt8 9
                , A.regionEncoder recordRegion
                , BE.maybe BE.string maybeName
                , A.regionEncoder fieldRegion
                , BE.string field
                ]

        RecordUpdateKeys expectedFields ->
            BE.sequence
                [ BE.unsignedInt8 10
                , BE.assocListDict compare BE.string Can.fieldUpdateEncoder expectedFields
                ]

        RecordUpdateValue field ->
            BE.sequence
                [ BE.unsignedInt8 11
                , BE.string field
                ]

        Destructure ->
            BE.unsignedInt8 12


contextDecoder : BD.Decoder Context
contextDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map ListEntry Index.zeroBasedDecoder

                    1 ->
                        BD.succeed Negate

                    2 ->
                        BD.map OpLeft BD.string

                    3 ->
                        BD.map OpRight BD.string

                    4 ->
                        BD.succeed IfCondition

                    5 ->
                        BD.map IfBranch Index.zeroBasedDecoder

                    6 ->
                        BD.map CaseBranch Index.zeroBasedDecoder

                    7 ->
                        BD.map2 CallArity
                            maybeNameDecoder
                            BD.int

                    8 ->
                        BD.map2 CallArg
                            maybeNameDecoder
                            Index.zeroBasedDecoder

                    9 ->
                        BD.map4 RecordAccess
                            A.regionDecoder
                            (BD.maybe BD.string)
                            A.regionDecoder
                            BD.string

                    10 ->
                        BD.map RecordUpdateKeys
                            (BD.assocListDict identity BD.string Can.fieldUpdateDecoder)

                    11 ->
                        BD.map RecordUpdateValue BD.string

                    12 ->
                        BD.succeed Destructure

                    _ ->
                        BD.fail
            )


subContextEncoder : SubContext -> BE.Encoder
subContextEncoder subContext =
    case subContext of
        TypedIfBranch index ->
            BE.sequence
                [ BE.unsignedInt8 0
                , Index.zeroBasedEncoder index
                ]

        TypedCaseBranch index ->
            BE.sequence
                [ BE.unsignedInt8 1
                , Index.zeroBasedEncoder index
                ]

        TypedBody ->
            BE.unsignedInt8 2


subContextDecoder : BD.Decoder SubContext
subContextDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map TypedIfBranch Index.zeroBasedDecoder

                    1 ->
                        BD.map TypedCaseBranch Index.zeroBasedDecoder

                    2 ->
                        BD.succeed TypedBody

                    _ ->
                        BD.fail
            )


pCategoryEncoder : PCategory -> BE.Encoder
pCategoryEncoder pCategory =
    case pCategory of
        PRecord ->
            BE.unsignedInt8 0

        PUnit ->
            BE.unsignedInt8 1

        PTuple ->
            BE.unsignedInt8 2

        PList ->
            BE.unsignedInt8 3

        PCtor name ->
            BE.sequence
                [ BE.unsignedInt8 4
                , BE.string name
                ]

        PInt ->
            BE.unsignedInt8 5

        PStr ->
            BE.unsignedInt8 6

        PChr ->
            BE.unsignedInt8 7

        PBool ->
            BE.unsignedInt8 8


pCategoryDecoder : BD.Decoder PCategory
pCategoryDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed PRecord

                    1 ->
                        BD.succeed PUnit

                    2 ->
                        BD.succeed PTuple

                    3 ->
                        BD.succeed PList

                    4 ->
                        BD.map PCtor BD.string

                    5 ->
                        BD.succeed PInt

                    6 ->
                        BD.succeed PStr

                    7 ->
                        BD.succeed PChr

                    8 ->
                        BD.succeed PBool

                    _ ->
                        BD.fail
            )


pExpectedEncoder : (a -> BE.Encoder) -> PExpected a -> BE.Encoder
pExpectedEncoder encoder pExpected =
    case pExpected of
        PNoExpectation expectedType ->
            BE.sequence
                [ BE.unsignedInt8 0
                , encoder expectedType
                ]

        PFromContext region context expectedType ->
            BE.sequence
                [ BE.unsignedInt8 1
                , A.regionEncoder region
                , pContextEncoder context
                , encoder expectedType
                ]


pExpectedDecoder : BD.Decoder a -> BD.Decoder (PExpected a)
pExpectedDecoder decoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map PNoExpectation decoder

                    1 ->
                        BD.map3 PFromContext
                            A.regionDecoder
                            pContextDecoder
                            decoder

                    _ ->
                        BD.fail
            )


maybeNameEncoder : MaybeName -> BE.Encoder
maybeNameEncoder maybeName =
    case maybeName of
        FuncName name ->
            BE.sequence
                [ BE.unsignedInt8 0
                , BE.string name
                ]

        CtorName name ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string name
                ]

        OpName op ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.string op
                ]

        NoName ->
            BE.unsignedInt8 3


maybeNameDecoder : BD.Decoder MaybeName
maybeNameDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map FuncName BD.string

                    1 ->
                        BD.map CtorName BD.string

                    2 ->
                        BD.map OpName BD.string

                    3 ->
                        BD.succeed NoName

                    _ ->
                        BD.fail
            )


pContextEncoder : PContext -> BE.Encoder
pContextEncoder pContext =
    case pContext of
        PTypedArg name index ->
            BE.sequence
                [ BE.unsignedInt8 0
                , BE.string name
                , Index.zeroBasedEncoder index
                ]

        PCaseMatch index ->
            BE.sequence
                [ BE.unsignedInt8 1
                , Index.zeroBasedEncoder index
                ]

        PCtorArg name index ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.string name
                , Index.zeroBasedEncoder index
                ]

        PListEntry index ->
            BE.sequence
                [ BE.unsignedInt8 3
                , Index.zeroBasedEncoder index
                ]

        PTail ->
            BE.unsignedInt8 4


pContextDecoder : BD.Decoder PContext
pContextDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map2 PTypedArg
                            BD.string
                            Index.zeroBasedDecoder

                    1 ->
                        BD.map PCaseMatch Index.zeroBasedDecoder

                    2 ->
                        BD.map2 PCtorArg
                            BD.string
                            Index.zeroBasedDecoder

                    3 ->
                        BD.map PListEntry Index.zeroBasedDecoder

                    4 ->
                        BD.succeed PTail

                    _ ->
                        BD.fail
            )
