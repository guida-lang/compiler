module Common.Format.Box exposing
    ( Line, identifier, keyword, punc, literal, row, space
    , Box(..), blankLine, line, mustBreak, stack_, stack1, andThen
    , isLine, allSingles, lineLength
    , indent, prefix, addSuffix
    , render
    )

{-| Ref.: `elm-format-lib/src/Box.hs`

@docs Line, identifier, keyword, punc, literal, row, space
@docs Box, blankLine, line, mustBreak, stack_, stack1, andThen
@docs isLine, allSingles, lineLength
@docs indent, prefix, addSuffix
@docs render

-}

import Basics.Extra exposing (flip)
import Prelude
import Result.Extra as Result
import Utils.Crash exposing (crash)
import Utils.Main as Utils


{-| A line is ALWAYS just one line.

Space is self-explanatory,
Tab aligns to the nearest multiple of 4 spaces,
Text brings any string into the data structure,
Row joins more of these elements onto one line.

-}
type Line
    = Text String
    | Row (List Line)
    | Space
    | Tab


identifier : String -> Line
identifier =
    Text


keyword : String -> Line
keyword =
    Text


punc : String -> Line
punc =
    Text


literal : String -> Line
literal =
    Text


{-| join more Line elements into one
-}
row : List Line -> Line
row =
    Row


space : Line
space =
    Space


{-| Box contains Lines (at least one - can't be empty).
Box either:

  - can appear in the middle of a line
    (Stack someLine [], thus can be joined without problems), or
  - has to appear on its own
    (Stack someLine moreLines OR MustBreak someLine).

MustBreak is only used for `--` comments.

Stack contains two or more lines.

Sometimes (see `prefix`) the first line of Stack
gets different treatment than the other lines.

-}
type Box
    = SingleLine Line
    | Stack Line Line (List Line)
    | MustBreak Line


{-| -}
blankLine : Box
blankLine =
    line (literal "")


{-| -}
line : Line -> Box
line l =
    SingleLine l


{-| -}
mustBreak : Line -> Box
mustBreak l =
    MustBreak l


{-| -}
stack_ : Box -> Box -> Box
stack_ b1 b2 =
    let
        ( line1first, line1rest ) =
            destructure b1

        ( line2first, line2rest ) =
            destructure b2
    in
    case line1rest ++ line2first :: line2rest of
        [] ->
            crash "the list will contain at least line2first"

        first :: rest ->
            Stack line1first first rest


{-| -}
andThen : List Box -> Box -> Box
andThen rest first =
    List.foldl (flip stack_) first rest


{-| -}
stack1 : List Box -> Box
stack1 children =
    case children of
        [] ->
            crash "stack1: empty structure"

        [ first ] ->
            first

        boxes ->
            Utils.foldr1 stack_ boxes


mapLines : (Line -> Line) -> Box -> Box
mapLines fn =
    mapFirstLine fn fn


mapFirstLine : (Line -> Line) -> (Line -> Line) -> Box -> Box
mapFirstLine firstFn restFn b =
    case b of
        SingleLine l1 ->
            SingleLine (firstFn l1)

        Stack l1 l2 ls ->
            Stack (firstFn l1) (restFn l2) (List.map restFn ls)

        MustBreak l1 ->
            MustBreak (firstFn l1)


indent : Box -> Box
indent =
    mapLines (\l -> row [ Tab, l ])


isLine : Box -> Result Box Line
isLine b =
    case b of
        SingleLine l ->
            Ok l

        _ ->
            Err b


destructure : Box -> ( Line, List Line )
destructure b =
    case b of
        SingleLine l1 ->
            ( l1, [] )

        Stack l1 l2 rest ->
            ( l1, l2 :: rest )

        MustBreak l1 ->
            ( l1, [] )


allSingles : List Box -> Result (List Box) (List Line)
allSingles boxes =
    case Result.combine (List.map isLine boxes) of
        Ok lines_ ->
            Ok lines_

        _ ->
            Err boxes


{-| Add the prefix to the first line,
pad the other lines with spaces of the same length

EXAMPLE:
abcde
xyz
----->
myPrefix abcde
xyz

-}
prefix : Line -> Box -> Box
prefix pref =
    let
        prefixLength : Int
        prefixLength =
            lineLength 0 pref

        paddingSpaces : List Line
        paddingSpaces =
            List.repeat prefixLength space

        padLineWithSpaces : Line -> Line
        padLineWithSpaces l =
            row [ row paddingSpaces, l ]

        addPrefixToLine : Line -> Line
        addPrefixToLine l =
            row [ pref, l ]
    in
    mapFirstLine addPrefixToLine padLineWithSpaces


addSuffix : Line -> Box -> Box
addSuffix suffix b =
    case destructure b of
        ( l, [] ) ->
            line (row [ l, suffix ])

        ( l1, ls ) ->
            line l1
                |> andThen (List.map line (Prelude.init ls))
                |> andThen [ line (row [ Prelude.last ls, suffix ]) ]


renderLine : Int -> Line -> String
renderLine startColumn line_ =
    case line_ of
        Text text ->
            text

        Space ->
            " "

        Tab ->
            String.fromList (List.repeat (tabLength startColumn) ' ')

        Row lines_ ->
            renderRow startColumn lines_


render : Box -> String
render box =
    case box of
        SingleLine line_ ->
            String.trimRight (renderLine 0 line_) ++ "\n"

        Stack l1 l2 rest ->
            String.join "\n" (List.map (String.trimRight << renderLine 0) (l1 :: l2 :: rest))

        MustBreak line_ ->
            String.trimRight (renderLine 0 line_) ++ "\n"


lineLength : Int -> Line -> Int
lineLength startColumn line_ =
    startColumn
        + (case line_ of
            Text string ->
                String.length string

            Space ->
                1

            Tab ->
                tabLength startColumn

            Row lines_ ->
                rowLength startColumn lines_
          )


initRow : Int -> ( String, Int )
initRow startColumn =
    ( "", startColumn )


spacesInTab : Int
spacesInTab =
    4


spacesToNextTab : Int -> Int
spacesToNextTab startColumn =
    modBy spacesInTab startColumn


tabLength : Int -> Int
tabLength startColumn =
    spacesInTab - spacesToNextTab startColumn


{-| What happens here is we take a row and start building its contents
along with the resulting length of the string. We need to have that
because of Tabs, which need to be passed the current column in arguments
in order to determine how many Spaces are they going to span.
(See `tabLength`.)

So for example if we have a Box [Space, Tab, Text "abc", Tab, Text "x"],
it goes like this:

string | column | todo
"" | 0 | [Space, Tab, Text "abc", Tab, Text "x"]
" " | 1 | [Tab, Text "abc", Tab, Text "x"]
" " | 4 | [Text "abc", Tab, Text "x"]
" abc" | 7 | [Tab, Text "x"]
" abc " | 8 | [Text "x"]
" abc x" | 9 | []

Thus we get the result string with correctly rendered Tabs.

The (String, Int) type here means the (string, column) from the table above.

Then we just need to do one final modification to get from endColumn to resultLength,
which is what we are after in the function `rowLength`.

-}
renderRow_ : Int -> List Line -> ( String, Int )
renderRow_ startColumn lines_ =
    let
        ( result, endColumn ) =
            List.foldl addLine (initRow startColumn) lines_

        resultLength : Int
        resultLength =
            endColumn - startColumn
    in
    ( result, resultLength )


{-| A step function for renderRow\_.

    addLine Tab ( " ", 1 ) == ( " ", 4 )

-}
addLine : Line -> ( String, Int ) -> ( String, Int )
addLine line_ ( string, startColumn_ ) =
    let
        newString : String
        newString =
            string ++ renderLine startColumn_ line_

        newStartColumn : Int
        newStartColumn =
            lineLength startColumn_ line_
    in
    ( newString, newStartColumn )


{-| Extract the final string from renderRow\_
-}
renderRow : Int -> List Line -> String
renderRow startColumn lines_ =
    Tuple.first (renderRow_ startColumn lines_)


{-| Extract the final length from renderRow\_
-}
rowLength : Int -> List Line -> Int
rowLength startColumn lines_ =
    Tuple.second (renderRow_ startColumn lines_)
