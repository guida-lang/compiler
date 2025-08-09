module Common.Format.Render.ElmStructure exposing
    ( FunctionApplicationMultiline(..)
    , Multiline(..)
    , application
    , definition
    , equalsPair
    , extensionGroup
    , extensionGroup_
    , forceableRowOrStack
    , forceableSpaceSepOrIndented
    , forceableSpaceSepOrStack
    , forceableSpaceSepOrStack1
    , group
    , group_
    , prefixOrIndented
    , spaceSepOrIndented
    , spaceSepOrPrefix
    , spaceSepOrStack
    )

import Common.Format.Box as Box exposing (Box)
import Utils.Crash exposing (crash)


{-| Same as `forceableSpaceSepOrStack False`
-}
spaceSepOrStack : Box -> List Box -> Box
spaceSepOrStack =
    forceableSpaceSepOrStack False


{-| Formats as:

    first rest0 rest1

    first

    rest0

    rest1

-}
forceableSpaceSepOrStack : Bool -> Box -> List Box -> Box
forceableSpaceSepOrStack forceMultiline first rest =
    case
        ( forceMultiline, first, Box.allSingles rest )
    of
        ( False, Box.SingleLine first_, Ok rest_ ) ->
            Box.line <| Box.row <| List.intersperse Box.space (first_ :: rest_)

        _ ->
            Box.stack1 (first :: rest)


forceableRowOrStack : Bool -> Box -> List Box -> Box
forceableRowOrStack forceMultiline first rest =
    case
        ( forceMultiline, first, Box.allSingles rest )
    of
        ( False, Box.SingleLine first_, Ok rest_ ) ->
            Box.line <| Box.row (first_ :: rest_)

        _ ->
            Box.stack1 (first :: rest)


{-| Same as `forceableSpaceSepOrStack`
-}
forceableSpaceSepOrStack1 : Bool -> List Box -> Box
forceableSpaceSepOrStack1 forceMultiline boxes =
    case boxes of
        first :: rest ->
            forceableSpaceSepOrStack forceMultiline first rest

        _ ->
            crash "forceableSpaceSepOrStack1 with empty list"


{-| Formats as:

    first rest0 rest1 rest2

    first
        rest0
        rest1
        rest2

-}
spaceSepOrIndented : Box -> List Box -> Box
spaceSepOrIndented =
    forceableSpaceSepOrIndented False


forceableSpaceSepOrIndented : Bool -> Box -> List Box -> Box
forceableSpaceSepOrIndented forceMultiline first rest =
    case
        ( forceMultiline, first, Box.allSingles rest )
    of
        ( False, Box.SingleLine first_, Ok rest_ ) ->
            Box.line <| Box.row <| List.intersperse Box.space (first_ :: rest_)

        _ ->
            Box.stack1
                (first :: List.map Box.indent rest)


{-| Formats as:

    op rest

    op rest1
        rest2

    opLong
        rest

-}
spaceSepOrPrefix : Box -> Box -> Box
spaceSepOrPrefix op rest =
    case ( op, rest ) of
        ( Box.SingleLine op_, Box.SingleLine rest_ ) ->
            Box.line <| Box.row [ op_, Box.space, rest_ ]

        ( Box.SingleLine op_, _ ) ->
            if Box.lineLength 0 op_ < 4 then
                Box.prefix (Box.row [ op_, Box.space ]) rest

            else
                Box.stack1 [ op, Box.indent rest ]

        _ ->
            Box.stack1 [ op, Box.indent rest ]


prefixOrIndented : Box -> Box -> Box
prefixOrIndented a b =
    case ( a, b ) of
        ( Box.SingleLine a_, Box.SingleLine b_ ) ->
            Box.line <| Box.row [ a_, Box.space, b_ ]

        ( Box.SingleLine a_, Box.MustBreak b_ ) ->
            Box.mustBreak <| Box.row [ a_, Box.space, b_ ]

        _ ->
            Box.stack1 [ a, Box.indent b ]


{-| Formats as:

    left =
        right
    left =
        right
    left =
        right

-}
equalsPair : String -> Bool -> Box -> Box -> Box
equalsPair symbol forceMultiline left right =
    case ( forceMultiline, left, right ) of
        ( False, Box.SingleLine left_, Box.SingleLine right_ ) ->
            Box.line <|
                Box.row
                    [ left_
                    , Box.space
                    , Box.punc symbol
                    , Box.space
                    , right_
                    ]

        ( _, Box.SingleLine left_, Box.MustBreak right_ ) ->
            Box.mustBreak <|
                Box.row
                    [ left_
                    , Box.space
                    , Box.punc symbol
                    , Box.space
                    , right_
                    ]

        ( _, Box.SingleLine left_, right_ ) ->
            Box.stack1
                [ Box.line <| Box.row [ left_, Box.space, Box.punc symbol ]
                , Box.indent right_
                ]

        ( _, left_, right_ ) ->
            Box.stack1
                [ left_
                , Box.indent <| Box.line <| Box.punc symbol
                , Box.indent right_
                ]


{-| An equalsPair where the left side is an application
-}
definition : String -> Bool -> Box -> List Box -> Box -> Box
definition symbol forceMultiline first rest =
    equalsPair symbol
        forceMultiline
        (application (FAJoinFirst JoinAll) first rest)


{-| Formats as:

    first rest0 rest1 rest2

    first rest0
        rest1
        rest2

    first
        rest0
        rest1
        rest2

-}
application : FunctionApplicationMultiline -> Box -> List Box -> Box
application forceMultiline first args =
    case args of
        [] ->
            first

        arg0 :: rest ->
            case
                ( ( forceMultiline
                  , first
                  )
                , ( arg0
                  , Box.allSingles rest
                  )
                )
            of
                ( ( FAJoinFirst JoinAll, Box.SingleLine first_ ), ( Box.SingleLine arg0_, Ok rest_ ) ) ->
                    (first_ :: arg0_ :: rest_)
                        |> List.intersperse Box.space
                        |> Box.row
                        |> Box.line

                ( ( FAJoinFirst _, Box.SingleLine first_ ), ( Box.SingleLine arg0_, _ ) ) ->
                    Box.stack1 <|
                        Box.line (Box.row [ first_, Box.space, arg0_ ])
                            :: List.map Box.indent rest

                _ ->
                    Box.stack1 <|
                        first
                            :: List.map Box.indent (arg0 :: rest)


{-| `group True '<' ';' '>'` formats as:

    <>

    < child0 >

    < child0; child1; child2 >

    < child0
    ; child1
    ; child2
    >

-}
group : Bool -> String -> String -> String -> Bool -> List Box -> Box
group innerSpaces left sep right forceMultiline children =
    group_ innerSpaces left sep [] right forceMultiline children


group_ : Bool -> String -> String -> List Box -> String -> Bool -> List Box -> Box
group_ innerSpaces left sep extraFooter right forceMultiline children =
    case ( forceMultiline, Box.allSingles children, Box.allSingles extraFooter ) of
        ( _, Ok [], Ok efs ) ->
            Box.line <| Box.row <| List.concat [ [ Box.punc left ], efs, [ Box.punc right ] ]

        ( False, Ok ls, Ok efs ) ->
            Box.line <|
                Box.row <|
                    List.concat
                        [ if innerSpaces then
                            [ Box.punc left, Box.space ]

                          else
                            [ Box.punc left ]
                        , List.intersperse (Box.row [ Box.punc sep, Box.space ]) (ls ++ efs)
                        , if innerSpaces then
                            [ Box.space, Box.punc right ]

                          else
                            [ Box.punc right ]
                        ]

        _ ->
            case children of
                [] ->
                    -- TODO: might lose extraFooter in this case, but can that ever happen?
                    Box.line <| Box.row [ Box.punc left, Box.punc right ]

                first :: rest ->
                    Box.stack1 <|
                        Box.prefix (Box.row [ Box.punc left, Box.space ]) first
                            :: List.map (Box.prefix <| Box.row [ Box.punc sep, Box.space ]) rest
                            ++ extraFooter
                            ++ [ Box.line <| Box.punc right ]


{-| Formats as:

    { base | first }

    { base | first, rest0, rest1 }

    { base
      | first
      , rest0
      , rest1
    }

-}
extensionGroup : Bool -> Box -> Box -> List Box -> Box
extensionGroup multiline base first rest =
    case
        ( multiline
        , Box.isLine base
        , Box.allSingles (first :: rest)
        )
    of
        ( False, Ok base_, Ok fields_ ) ->
            Box.line <|
                Box.row
                    [ Box.punc "{"
                    , Box.space
                    , base_
                    , Box.space
                    , Box.punc "|"
                    , Box.space
                    , Box.row (List.intersperse (Box.row [ Box.punc ",", Box.space ]) fields_)
                    , Box.space
                    , Box.punc "}"
                    ]

        _ ->
            Box.stack1
                [ Box.prefix (Box.row [ Box.punc "{", Box.space ]) base
                , Box.stack1
                    (Box.prefix (Box.row [ Box.punc "|", Box.space ]) first
                        :: List.map (Box.prefix (Box.row [ Box.punc ",", Box.space ])) rest
                    )
                    |> Box.indent
                , Box.line <| Box.punc "}"
                ]


extensionGroup_ : Bool -> Box -> Box -> Box
extensionGroup_ multiline base fields =
    case
        ( multiline
        , base
        , fields
        )
    of
        ( False, Box.SingleLine base_, Box.SingleLine fields_ ) ->
            Box.line <|
                Box.row <|
                    List.intersperse Box.space
                        [ Box.punc "{"
                        , base_
                        , fields_
                        , Box.punc "}"
                        ]

        _ ->
            Box.stack1
                [ Box.prefix (Box.row [ Box.punc "{", Box.space ]) base
                , Box.indent fields
                , Box.line <| Box.punc "}"
                ]



-- FROM `AST.V0_16`


type Multiline
    = JoinAll
    | SplitAll


type FunctionApplicationMultiline
    = FASplitFirst
    | FAJoinFirst Multiline
