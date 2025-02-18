module Compiler.Parse.Variable exposing
    ( Upper(..)
    , chompInnerChars
    , chompLower
    , chompUpper
    , foreignAlpha
    , foreignUpper
    , getInnerWidth
    , getInnerWidthHelp
    , getUpperWidth
    , isDot
    , isReservedWord
    , lower
    , moduleName
    , upper
    )

import Bitwise
import Compiler.AST.Source as Src
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Parse.Primitives as P exposing (Col, Row)
import Data.Set as EverySet exposing (EverySet)



-- LOCAL UPPER


upper : (Row -> Col -> x) -> P.Parser x Name
upper toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                ( newPos, newCol ) =
                    chompUpper src pos end col
            in
            if newPos == pos then
                P.Eerr row col toError

            else
                let
                    name : Name
                    name =
                        Name.fromPtr src pos newPos
                in
                P.Cok name (P.State src newPos end indent row newCol)



-- LOCAL LOWER


lower : (Row -> Col -> x) -> P.Parser x Name
lower toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                ( newPos, newCol ) =
                    chompLower src pos end col
            in
            if newPos == pos then
                P.Eerr row col toError

            else
                let
                    name : Name
                    name =
                        Name.fromPtr src pos newPos
                in
                if isReservedWord name then
                    P.Eerr row col toError

                else
                    let
                        newState : P.State
                        newState =
                            P.State src newPos end indent row newCol
                    in
                    P.Cok name newState


isReservedWord : Name.Name -> Bool
isReservedWord name =
    EverySet.member identity name reservedWords


reservedWords : EverySet String Name
reservedWords =
    EverySet.fromList identity
        [ "if"
        , "then"
        , "else"
        , "case"
        , "of"
        , "let"
        , "in"
        , "type"
        , "module"
        , "where"
        , "import"
        , "exposing"
        , "as"
        , "port"
        ]



-- MODULE NAME


moduleName : (Row -> Col -> x) -> P.Parser x Name
moduleName toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                ( pos1, col1 ) =
                    chompUpper src pos end col
            in
            if pos == pos1 then
                P.Eerr row col toError

            else
                let
                    ( status, newPos, newCol ) =
                        moduleNameHelp src pos1 end col1
                in
                case status of
                    Good ->
                        let
                            name : Name
                            name =
                                Name.fromPtr src pos newPos

                            newState : P.State
                            newState =
                                P.State src newPos end indent row newCol
                        in
                        P.Cok name newState

                    Bad ->
                        P.Cerr row newCol toError


type ModuleNameStatus
    = Good
    | Bad


moduleNameHelp : String -> Int -> Int -> Col -> ( ModuleNameStatus, Int, Col )
moduleNameHelp src pos end col =
    if isDot src pos end then
        let
            pos1 : Int
            pos1 =
                pos + 1

            ( newPos, newCol ) =
                chompUpper src pos1 end (col + 1)
        in
        if pos1 == newPos then
            ( Bad, newPos, newCol )

        else
            moduleNameHelp src newPos end newCol

    else
        ( Good, pos, col )



-- FOREIGN UPPER


type Upper
    = Unqualified Name
    | Qualified Name Name


foreignUpper : (Row -> Col -> x) -> P.Parser x Upper
foreignUpper toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                ( upperStart, upperEnd, newCol ) =
                    foreignUpperHelp src pos end col
            in
            if upperStart == upperEnd then
                P.Eerr row newCol toError

            else
                let
                    newState : P.State
                    newState =
                        P.State src upperEnd end indent row newCol

                    name : Name
                    name =
                        Name.fromPtr src upperStart upperEnd

                    upperName : Upper
                    upperName =
                        if upperStart == pos then
                            Unqualified name

                        else
                            let
                                home : Name
                                home =
                                    Name.fromPtr src pos (upperStart + -1)
                            in
                            Qualified home name
                in
                P.Cok upperName newState


foreignUpperHelp : String -> Int -> Int -> Col -> ( Int, Int, Col )
foreignUpperHelp src pos end col =
    let
        ( newPos, newCol ) =
            chompUpper src pos end col
    in
    if pos == newPos then
        ( pos, pos, col )

    else if isDot src newPos end then
        foreignUpperHelp src (newPos + 1) end (newCol + 1)

    else
        ( pos, newPos, newCol )



-- FOREIGN ALPHA


foreignAlpha : (Row -> Col -> x) -> P.Parser x Src.Expr_
foreignAlpha toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                ( ( alphaStart, alphaEnd ), ( newCol, varType ) ) =
                    foreignAlphaHelp src pos end col
            in
            if alphaStart == alphaEnd then
                P.Eerr row newCol toError

            else
                let
                    name : Name
                    name =
                        Name.fromPtr src alphaStart alphaEnd

                    newState : P.State
                    newState =
                        P.State src alphaEnd end indent row newCol
                in
                if alphaStart == pos then
                    if isReservedWord name then
                        P.Eerr row col toError

                    else
                        P.Cok (Src.Var varType name) newState

                else
                    let
                        home : Name
                        home =
                            Name.fromPtr src pos (alphaStart + -1)
                    in
                    P.Cok (Src.VarQual varType home name) newState


foreignAlphaHelp : String -> Int -> Int -> Col -> ( ( Int, Int ), ( Col, Src.VarType ) )
foreignAlphaHelp src pos end col =
    let
        ( lowerPos, lowerCol ) =
            chompLower src pos end col
    in
    if pos < lowerPos then
        ( ( pos, lowerPos ), ( lowerCol, Src.LowVar ) )

    else
        let
            ( upperPos, upperCol ) =
                chompUpper src pos end col
        in
        if pos == upperPos then
            ( ( pos, pos ), ( col, Src.CapVar ) )

        else if isDot src upperPos end then
            foreignAlphaHelp src (upperPos + 1) end (upperCol + 1)

        else
            ( ( pos, upperPos ), ( upperCol, Src.CapVar ) )



---- CHAR CHOMPERS ----
-- DOTS


isDot : String -> Int -> Int -> Bool
isDot src pos end =
    pos < end && P.unsafeIndex src pos == '.'



-- UPPER CHARS


chompUpper : String -> Int -> Int -> Col -> ( Int, Col )
chompUpper src pos end col =
    let
        width : Int
        width =
            getUpperWidth src pos end
    in
    if width == 0 then
        ( pos, col )

    else
        chompInnerChars src (pos + width) end (col + 1)


getUpperWidth : String -> Int -> Int -> Int
getUpperWidth src pos end =
    if pos < end then
        getUpperWidthHelp src pos end (P.unsafeIndex src pos)

    else
        0


getUpperWidthHelp : String -> Int -> Int -> Char -> Int
getUpperWidthHelp src pos _ word =
    let
        code : Int
        code =
            Char.toCode word
    in
    if code >= 0x41 {- A -} && code <= 0x5A {- Z -} then
        1

    else if code < 0xC0 then
        0

    else if code < 0xE0 then
        if Char.isUpper (chr2 src pos word) then
            2

        else
            0

    else if code < 0xF0 then
        if Char.isUpper (chr3 src pos word) then
            3

        else
            0

    else if code < 0xF8 then
        if Char.isUpper (chr4 src pos word) then
            4

        else
            0

    else
        0



-- LOWER CHARS


chompLower : String -> Int -> Int -> Col -> ( Int, Col )
chompLower src pos end col =
    let
        width : Int
        width =
            getLowerWidth src pos end
    in
    if width == 0 then
        ( pos, col )

    else
        chompInnerChars src (pos + width) end (col + 1)


getLowerWidth : String -> Int -> Int -> Int
getLowerWidth src pos end =
    if pos < end then
        getLowerWidthHelp src pos end (P.unsafeIndex src pos)

    else
        0


getLowerWidthHelp : String -> Int -> Int -> Char -> Int
getLowerWidthHelp src pos _ word =
    let
        code : Int
        code =
            Char.toCode word
    in
    if code >= 0x61 {- a -} && code <= 0x7A {- z -} then
        1

    else if code < 0xC0 then
        0

    else if code < 0xE0 then
        if Char.isLower (chr2 src pos word) then
            2

        else
            0

    else if code < 0xF0 then
        if Char.isLower (chr3 src pos word) then
            3

        else
            0

    else if code < 0xF8 then
        if Char.isLower (chr4 src pos word) then
            4

        else
            0

    else
        0



-- INNER CHARS


chompInnerChars : String -> Int -> Int -> Col -> ( Int, Col )
chompInnerChars src pos end col =
    let
        width : Int
        width =
            getInnerWidth src pos end
    in
    if width == 0 then
        ( pos, col )

    else
        chompInnerChars src (pos + width) end (col + 1)


getInnerWidth : String -> Int -> Int -> Int
getInnerWidth src pos end =
    if pos < end then
        getInnerWidthHelp src pos end (P.unsafeIndex src pos)

    else
        0


getInnerWidthHelp : String -> Int -> Int -> Char -> Int
getInnerWidthHelp src pos _ word =
    let
        code : Int
        code =
            Char.toCode word
    in
    if code >= 0x61 {- a -} && code <= 0x7A {- z -} then
        1

    else if code >= 0x41 {- A -} && code <= 0x5A {- Z -} then
        1

    else if code >= 0x30 {- 0 -} && code <= 0x39 {- 9 -} then
        1

    else if code == 0x5F {- _ -} then
        1

    else if code < 0xC0 then
        0

    else if code < 0xE0 then
        if Char.isAlpha (chr2 src pos word) then
            2

        else
            0

    else if code < 0xF0 then
        if Char.isAlpha (chr3 src pos word) then
            3

        else
            0

    else if code < 0xF8 then
        if Char.isAlpha (chr4 src pos word) then
            4

        else
            0

    else
        0



-- EXTRACT CHARACTERS


chr2 : String -> Int -> Char -> Char
chr2 src pos firstWord =
    let
        i1 : Int
        i1 =
            unpack firstWord

        i2 : Int
        i2 =
            unpack (P.unsafeIndex src (pos + 1))

        c1 : Int
        c1 =
            Bitwise.shiftLeftBy 6 (i1 - 0xC0)

        c2 : Int
        c2 =
            i2 - 0x80
    in
    Char.fromCode (c1 + c2)


chr3 : String -> Int -> Char -> Char
chr3 src pos firstWord =
    let
        i1 : Int
        i1 =
            unpack firstWord

        i2 : Int
        i2 =
            unpack (P.unsafeIndex src (pos + 1))

        i3 : Int
        i3 =
            unpack (P.unsafeIndex src (pos + 2))

        c1 : Int
        c1 =
            Bitwise.shiftLeftBy 12 (i1 - 0xE0)

        c2 : Int
        c2 =
            Bitwise.shiftLeftBy 6 (i2 - 0x80)

        c3 : Int
        c3 =
            i3 - 0x80
    in
    Char.fromCode (c1 + c2 + c3)


chr4 : String -> Int -> Char -> Char
chr4 src pos firstWord =
    let
        i1 : Int
        i1 =
            unpack firstWord

        i2 : Int
        i2 =
            unpack (P.unsafeIndex src (pos + 1))

        i3 : Int
        i3 =
            unpack (P.unsafeIndex src (pos + 2))

        i4 : Int
        i4 =
            unpack (P.unsafeIndex src (pos + 3))

        c1 : Int
        c1 =
            Bitwise.shiftLeftBy 18 (i1 - 0xF0)

        c2 : Int
        c2 =
            Bitwise.shiftLeftBy 12 (i2 - 0x80)

        c3 : Int
        c3 =
            Bitwise.shiftLeftBy 6 (i3 - 0x80)

        c4 : Int
        c4 =
            i4 - 0x80
    in
    Char.fromCode (c1 + c2 + c3 + c4)


unpack : Char -> Int
unpack =
    Char.toCode
