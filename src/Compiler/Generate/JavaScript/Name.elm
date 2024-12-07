module Compiler.Generate.JavaScript.Name exposing
    ( Name
    , dollar
    , fromCycle
    , fromGlobal
    , fromIndex
    , fromInt
    , fromKernel
    , fromLocal
    , makeA
    , makeF
    , makeLabel
    , makeTemp
    )

import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import System.TypeCheck.IO as IO



-- NAME


type alias Name =
    String



-- CONSTRUCTORS


fromIndex : Index.ZeroBased -> Name
fromIndex index =
    fromInt (Index.toMachine index)


fromInt : Int -> Name
fromInt n =
    intToAscii n


fromLocal : Name.Name -> Name
fromLocal name =
    if EverySet.member name reservedNames then
        "_" ++ name

    else
        name


fromGlobal : IO.Canonical -> Name.Name -> Name
fromGlobal home name =
    homeToBuilder home ++ usd ++ name


fromCycle : IO.Canonical -> Name.Name -> Name
fromCycle home name =
    homeToBuilder home ++ "$cyclic$" ++ name


fromKernel : Name.Name -> Name.Name -> Name
fromKernel home name =
    "_" ++ home ++ "_" ++ name


homeToBuilder : IO.Canonical -> String
homeToBuilder (IO.Canonical ( author, project ) home) =
    usd
        ++ String.replace "-" "_" author
        ++ usd
        ++ String.replace "-" "_" project
        ++ usd
        ++ String.replace "." "$" home



-- TEMPORARY NAMES


makeF : Int -> Name
makeF n =
    "F" ++ String.fromInt n


makeA : Int -> Name
makeA n =
    "A" ++ String.fromInt n


makeLabel : String -> Int -> Name
makeLabel name index =
    name ++ usd ++ String.fromInt index


makeTemp : String -> Name
makeTemp name =
    "$temp$" ++ name


dollar : Name
dollar =
    usd


usd : String
usd =
    Name.dollar



-- RESERVED NAMES


reservedNames : EverySet String
reservedNames =
    EverySet.union compare jsReservedWords elmReservedWords


jsReservedWords : EverySet String
jsReservedWords =
    EverySet.fromList compare
        [ "do"
        , "if"
        , "in"
        , "NaN"
        , "int"
        , "for"
        , "new"
        , "try"
        , "var"
        , "let"
        , "null"
        , "true"
        , "eval"
        , "byte"
        , "char"
        , "goto"
        , "long"
        , "case"
        , "else"
        , "this"
        , "void"
        , "with"
        , "enum"
        , "false"
        , "final"
        , "float"
        , "short"
        , "break"
        , "catch"
        , "throw"
        , "while"
        , "class"
        , "const"
        , "super"
        , "yield"
        , "double"
        , "native"
        , "throws"
        , "delete"
        , "return"
        , "switch"
        , "typeof"
        , "export"
        , "import"
        , "public"
        , "static"
        , "boolean"
        , "default"
        , "finally"
        , "extends"
        , "package"
        , "private"
        , "Infinity"
        , "abstract"
        , "volatile"
        , "function"
        , "continue"
        , "debugger"
        , "function"
        , "undefined"
        , "arguments"
        , "transient"
        , "interface"
        , "protected"
        , "instanceof"
        , "implements"
        , "synchronized"
        ]


elmReservedWords : EverySet String
elmReservedWords =
    EverySet.fromList compare
        [ "F2"
        , "F3"
        , "F4"
        , "F5"
        , "F6"
        , "F7"
        , "F8"
        , "F9"
        , "A2"
        , "A3"
        , "A4"
        , "A5"
        , "A6"
        , "A7"
        , "A8"
        , "A9"
        ]



-- INT TO ASCII


intToAscii : Int -> Name.Name
intToAscii n =
    if n < 53 then
        -- skip $ as a standalone name
        Name.fromWords [ toByte n ]

    else
        intToAsciiHelp 2 (numStartBytes * numInnerBytes) allBadFields (n - 53)


intToAsciiHelp : Int -> Int -> List BadFields -> Int -> Name.Name
intToAsciiHelp width blockSize badFields n =
    case badFields of
        [] ->
            if n < blockSize then
                unsafeIntToAscii width [] n

            else
                intToAsciiHelp (width + 1) (blockSize * numInnerBytes) [] (n - blockSize)

        (BadFields renamings) :: biggerBadFields ->
            let
                availableSize : Int
                availableSize =
                    blockSize - Dict.size renamings
            in
            if n < availableSize then
                let
                    name : Name.Name
                    name =
                        unsafeIntToAscii width [] n
                in
                Dict.get name renamings |> Maybe.withDefault name

            else
                intToAsciiHelp (width + 1) (blockSize * numInnerBytes) biggerBadFields (n - availableSize)



-- UNSAFE INT TO ASCII


unsafeIntToAscii : Int -> List Char -> Int -> Name.Name
unsafeIntToAscii width bytes n =
    if width <= 1 then
        Name.fromWords (toByte n :: bytes)

    else
        let
            quotient : Int
            quotient =
                n // numInnerBytes

            remainder : Int
            remainder =
                n - (numInnerBytes * quotient)
        in
        unsafeIntToAscii (width - 1) (toByte remainder :: bytes) quotient



-- ASCII BYTES


numStartBytes : Int
numStartBytes =
    54


numInnerBytes : Int
numInnerBytes =
    64


toByte : Int -> Char
toByte n =
    if n < 26 then
        -- lower
        Char.fromCode (97 + n)

    else if n < 52 then
        -- upper
        Char.fromCode (65 + n - 26)

    else if n == 52 then
        -- _
        Char.fromCode 95

    else if n == 53 then
        -- $
        Char.fromCode 36

    else if n < 64 then
        -- digit
        Char.fromCode (48 + n - 54)

    else
        -- crash ("cannot convert int " ++ String.fromInt n ++ " to ASCII")
        Char.fromCode n



-- BAD FIELDS


type BadFields
    = BadFields Renamings


type alias Renamings =
    Dict Name.Name Name.Name


allBadFields : List BadFields
allBadFields =
    let
        add : String -> Dict Int BadFields -> Dict Int BadFields
        add keyword dict =
            Dict.update compare (String.length keyword) (Just << addRenaming keyword) dict
    in
    Dict.values (EverySet.foldr add Dict.empty jsReservedWords)


addRenaming : String -> Maybe BadFields -> BadFields
addRenaming keyword maybeBadFields =
    let
        width : Int
        width =
            String.length keyword

        maxName : Int
        maxName =
            numStartBytes * numInnerBytes ^ (width - 1) - 1
    in
    case maybeBadFields of
        Nothing ->
            BadFields (Dict.singleton keyword (unsafeIntToAscii width [] maxName))

        Just (BadFields renamings) ->
            BadFields (Dict.insert compare keyword (unsafeIntToAscii width [] (maxName - Dict.size renamings)) renamings)
