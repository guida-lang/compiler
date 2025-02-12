module Compiler.Parse.Space exposing
    ( Parser
    , checkAligned
    , checkFreshLine
    , checkIndent
    , chomp
    , chompAndCheckIndent
    , docComment
    )

import Compiler.AST.Source as Src
import Compiler.Parse.Primitives as P exposing (Col, Row)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E



-- SPACE PARSING


type alias Parser x a =
    P.Parser x ( a, A.Position )



-- CHOMP


chomp : (E.Space -> Row -> Col -> x) -> P.Parser x ()
chomp toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                ( ( status, newPos ), ( newRow, newCol ) ) =
                    eat Spaces src pos end row col
            in
            case status of
                Good ->
                    let
                        newState : P.State
                        newState =
                            P.State src newPos end indent newRow newCol
                    in
                    P.Cok () newState

                HasTab ->
                    P.Cerr newRow newCol (toError E.HasTab)

                EndlessMultiComment ->
                    P.Cerr newRow newCol (toError E.EndlessMultiComment)



-- CHECKS -- to be called right after a `chomp`


checkIndent : A.Position -> (Int -> Int -> x) -> P.Parser x ()
checkIndent (A.Position endRow endCol) toError =
    P.Parser <|
        \((P.State _ _ _ indent _ col) as state) ->
            if col > indent && col > 1 then
                P.Eok () state

            else
                P.Eerr endRow endCol toError


checkAligned : (Int -> Int -> Int -> x) -> P.Parser x ()
checkAligned toError =
    P.Parser <|
        \((P.State _ _ _ indent row col) as state) ->
            if col == indent then
                P.Eok () state

            else
                P.Eerr row col (toError indent)


checkFreshLine : (Row -> Col -> x) -> P.Parser x ()
checkFreshLine toError =
    P.Parser <|
        \((P.State _ _ _ _ row col) as state) ->
            if col == 1 then
                P.Eok () state

            else
                P.Eerr row col toError



-- CHOMP AND CHECK


chompAndCheckIndent : (E.Space -> Row -> Col -> x) -> (Row -> Col -> x) -> P.Parser x ()
chompAndCheckIndent toSpaceError toIndentError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                ( ( status, newPos ), ( newRow, newCol ) ) =
                    eat Spaces src pos end row col
            in
            case status of
                Good ->
                    if newCol > indent && newCol > 1 then
                        let
                            newState : P.State
                            newState =
                                P.State src newPos end indent newRow newCol
                        in
                        P.Cok () newState

                    else
                        P.Cerr row col toIndentError

                HasTab ->
                    P.Cerr newRow newCol (toSpaceError E.HasTab)

                EndlessMultiComment ->
                    P.Cerr newRow newCol (toSpaceError E.EndlessMultiComment)



{- EAT SPACES, LINE COMMENTS AND MULTI COMMENTS

   This function combines the functionality of the original `eatSpaces`, `eatLineComment`,
   and `eatMultiComment` methods. The merge resolves a "RangeError: Maximum call stack size exceeded"
   issue reported in guida-lang/compiler#53.
-}


type EatType
    = Spaces
    | LineComment
    | MultiComment


type Status
    = Good
    | HasTab
    | EndlessMultiComment


eat : EatType -> String -> Int -> Int -> Row -> Col -> ( ( Status, Int ), ( Row, Col ) )
eat eatType src pos end row col =
    case eatType of
        Spaces ->
            if pos >= end then
                ( ( Good, pos ), ( row, col ) )

            else
                case P.unsafeIndex src pos of
                    ' ' ->
                        eat Spaces src (pos + 1) end row (col + 1)

                    '\n' ->
                        eat Spaces src (pos + 1) end (row + 1) 1

                    '{' ->
                        eat MultiComment src pos end row col

                    '-' ->
                        let
                            pos1 : Int
                            pos1 =
                                pos + 1
                        in
                        if pos1 < end && P.unsafeIndex src pos1 == '-' then
                            eat LineComment src (pos + 2) end row (col + 2)

                        else
                            ( ( Good, pos ), ( row, col ) )

                    '\u{000D}' ->
                        eat Spaces src (pos + 1) end row col

                    '\t' ->
                        ( ( HasTab, pos ), ( row, col ) )

                    _ ->
                        ( ( Good, pos ), ( row, col ) )

        LineComment ->
            if pos >= end then
                ( ( Good, pos ), ( row, col ) )

            else
                let
                    word : Char
                    word =
                        P.unsafeIndex src pos
                in
                if word == '\n' then
                    eat Spaces src (pos + 1) end (row + 1) 1

                else
                    let
                        newPos : Int
                        newPos =
                            pos + P.getCharWidth word
                    in
                    eat LineComment src newPos end row (col + 1)

        MultiComment ->
            let
                pos2 : Int
                pos2 =
                    pos + 2
            in
            if pos2 >= end then
                ( ( Good, pos ), ( row, col ) )

            else
                let
                    pos1 : Int
                    pos1 =
                        pos + 1
                in
                if P.unsafeIndex src pos1 == '-' then
                    if P.unsafeIndex src pos2 == '|' then
                        ( ( Good, pos ), ( row, col ) )

                    else
                        let
                            ( ( status, newPos ), ( newRow, newCol ) ) =
                                eatMultiCommentHelp src pos2 end row (col + 2) 1
                        in
                        case status of
                            MultiGood ->
                                eat Spaces src newPos end newRow newCol

                            MultiTab ->
                                ( ( HasTab, newPos ), ( newRow, newCol ) )

                            MultiEndless ->
                                ( ( EndlessMultiComment, pos ), ( row, col ) )

                else
                    ( ( Good, pos ), ( row, col ) )


type MultiStatus
    = MultiGood
    | MultiTab
    | MultiEndless


eatMultiCommentHelp : String -> Int -> Int -> Row -> Col -> Int -> ( ( MultiStatus, Int ), ( Row, Col ) )
eatMultiCommentHelp src pos end row col openComments =
    if pos >= end then
        ( ( MultiEndless, pos ), ( row, col ) )

    else
        let
            word : Char
            word =
                P.unsafeIndex src pos
        in
        if word == '\n' then
            eatMultiCommentHelp src (pos + 1) end (row + 1) 1 openComments

        else if word == '\t' then
            ( ( MultiTab, pos ), ( row, col ) )

        else if word == '-' && P.isWord src (pos + 1) end '}' then
            if openComments == 1 then
                ( ( MultiGood, pos + 2 ), ( row, col + 2 ) )

            else
                eatMultiCommentHelp src (pos + 2) end row (col + 2) (openComments - 1)

        else if word == '{' && P.isWord src (pos + 1) end '-' then
            eatMultiCommentHelp src (pos + 2) end row (col + 2) (openComments + 1)

        else
            let
                newPos : Int
                newPos =
                    pos + P.getCharWidth word
            in
            eatMultiCommentHelp src newPos end row (col + 1) openComments



-- DOCUMENTATION COMMENT


docComment : (Int -> Int -> x) -> (E.Space -> Int -> Int -> x) -> P.Parser x Src.Comment
docComment toExpectation toSpaceError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                pos3 : Int
                pos3 =
                    pos + 3
            in
            if
                (pos3 <= end)
                    && (P.unsafeIndex src pos == '{')
                    && (P.unsafeIndex src (pos + 1) == '-')
                    && (P.unsafeIndex src (pos + 2) == '|')
            then
                let
                    col3 : Col
                    col3 =
                        col + 3

                    ( ( status, newPos ), ( newRow, newCol ) ) =
                        eatMultiCommentHelp src pos3 end row col3 1
                in
                case status of
                    MultiGood ->
                        let
                            off : Int
                            off =
                                pos3

                            len : Int
                            len =
                                newPos - pos3 - 2

                            snippet : P.Snippet
                            snippet =
                                P.Snippet
                                    { fptr = src
                                    , offset = off
                                    , length = len
                                    , offRow = row
                                    , offCol = col3
                                    }

                            comment : Src.Comment
                            comment =
                                Src.Comment snippet

                            newState : P.State
                            newState =
                                P.State src newPos end indent newRow newCol
                        in
                        P.Cok comment newState

                    MultiTab ->
                        P.Cerr newRow newCol (toSpaceError E.HasTab)

                    MultiEndless ->
                        P.Cerr row col (toSpaceError E.EndlessMultiComment)

            else
                P.Eerr row col toExpectation
