module Compiler.Guida.ModuleName exposing
    ( Raw
    , array
    , basics
    , canonicalDecoder
    , canonicalEncoder
    , char
    , cmd
    , compareCanonical
    , debug
    , decoder
    , dict
    , encode
    , jsonDecode
    , jsonEncode
    , list
    , matrix4
    , maybe
    , platform
    , rawDecoder
    , rawEncoder
    , result
    , string
    , sub
    , texture
    , toChars
    , toComparableCanonical
    , toFilePath
    , toHyphenPath
    , tuple
    , vector2
    , vector3
    , vector4
    , virtualDom
    , webgl
    )

import Compiler.Data.Name as Name exposing (Name)
import Compiler.Generate.Target exposing (Target(..))
import Compiler.Guida.Package as Pkg
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P
import Compiler.Parse.Variable as Var
import System.TypeCheck.IO exposing (Canonical(..))
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



-- RAW


type alias Raw =
    Name


toChars : Raw -> List Char
toChars =
    Name.toChars


toFilePath : Raw -> String
toFilePath name =
    String.map
        (\c ->
            if c == '.' then
                -- TODO System.FilePath.pathSeparator
                '/'

            else
                c
        )
        name


toHyphenPath : Raw -> String
toHyphenPath name =
    String.map
        (\c ->
            if c == '.' then
                '-'

            else
                c
        )
        name



-- JSON


encode : Raw -> E.Value
encode =
    E.string


decoder : D.Decoder ( Int, Int ) Raw
decoder =
    D.customString parser Tuple.pair



-- PARSER


parser : P.Parser ( Int, Int ) Raw
parser =
    P.Parser
        (\(P.State src pos end indent row col) ->
            let
                ( isGood, newPos, newCol ) =
                    chompStart src pos end col
            in
            if isGood && (newPos - pos) < 256 then
                let
                    newState : P.State
                    newState =
                        P.State src newPos end indent row newCol
                in
                P.Cok (String.slice pos newPos src) newState

            else if col == newCol then
                P.Eerr row newCol Tuple.pair

            else
                P.Cerr row newCol Tuple.pair
        )


chompStart : String -> Int -> Int -> Int -> ( Bool, Int, Int )
chompStart src pos end col =
    let
        width : Int
        width =
            Var.getUpperWidth src pos end
    in
    if width == 0 then
        ( False, pos, col )

    else
        chompInner src (pos + width) end (col + 1)


chompInner : String -> Int -> Int -> Int -> ( Bool, Int, Int )
chompInner src pos end col =
    if pos >= end then
        ( True, pos, col )

    else
        let
            word : Char
            word =
                P.unsafeIndex src pos

            width : Int
            width =
                Var.getInnerWidthHelp src pos end word
        in
        if width == 0 then
            if word == '.' then
                chompStart src (pos + 1) end (col + 1)

            else
                ( True, pos, col )

        else
            chompInner src (pos + width) end (col + 1)



-- INSTANCES


compareCanonical : Canonical -> Canonical -> Order
compareCanonical (Canonical pkg1 name1) (Canonical pkg2 name2) =
    case compare name1 name2 of
        LT ->
            LT

        EQ ->
            Pkg.compareName pkg1 pkg2

        GT ->
            GT


toComparableCanonical : Canonical -> List String
toComparableCanonical (Canonical ( author, project ) name) =
    [ author, project, name ]



-- CORE


basics : Target -> Canonical
basics target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib Name.basics

        ElmTarget ->
            Canonical Pkg.core Name.basics


char : Target -> Canonical
char target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib Name.char

        ElmTarget ->
            Canonical Pkg.core Name.char


string : Target -> Canonical
string target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib Name.string

        ElmTarget ->
            Canonical Pkg.core Name.string


maybe : Target -> Canonical
maybe target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib Name.maybe

        ElmTarget ->
            Canonical Pkg.core Name.maybe


result : Target -> Canonical
result target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib Name.result

        ElmTarget ->
            Canonical Pkg.core Name.result


list : Target -> Canonical
list target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib Name.list

        ElmTarget ->
            Canonical Pkg.core Name.list


array : Target -> Canonical
array target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib Name.array

        ElmTarget ->
            Canonical Pkg.core Name.array


dict : Target -> Canonical
dict target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib Name.dict

        ElmTarget ->
            Canonical Pkg.core Name.dict


tuple : Target -> Canonical
tuple target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib Name.tuple

        ElmTarget ->
            Canonical Pkg.core Name.tuple


platform : Target -> Canonical
platform target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib Name.platform

        ElmTarget ->
            Canonical Pkg.core Name.platform


cmd : Target -> Canonical
cmd target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib Name.platformCmd

        ElmTarget ->
            Canonical Pkg.core Name.platformCmd


sub : Target -> Canonical
sub target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib Name.platformSub

        ElmTarget ->
            Canonical Pkg.core Name.platformSub


debug : Target -> Canonical
debug target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib Name.debug

        ElmTarget ->
            Canonical Pkg.core Name.debug



-- HTML


virtualDom : Target -> Canonical
virtualDom target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib Name.virtualDom

        ElmTarget ->
            Canonical Pkg.virtualDom Name.virtualDom



-- JSON


jsonDecode : Target -> Canonical
jsonDecode target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib "Json.Decode"

        ElmTarget ->
            Canonical Pkg.json "Json.Decode"


jsonEncode : Target -> Canonical
jsonEncode target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib "Json.Encode"

        ElmTarget ->
            Canonical Pkg.json "Json.Encode"



-- WEBGL


webgl : Target -> Canonical
webgl target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib "WebGL"

        ElmTarget ->
            Canonical Pkg.webgl "WebGL"


texture : Target -> Canonical
texture target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib "WebGL.Texture"

        ElmTarget ->
            Canonical Pkg.webgl "WebGL.Texture"


vector2 : Target -> Canonical
vector2 target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib "Math.Vector2"

        ElmTarget ->
            Canonical Pkg.linearAlgebra "Math.Vector2"


vector3 : Target -> Canonical
vector3 target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib "Math.Vector3"

        ElmTarget ->
            Canonical Pkg.linearAlgebra "Math.Vector3"


vector4 : Target -> Canonical
vector4 target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib "Math.Vector4"

        ElmTarget ->
            Canonical Pkg.linearAlgebra "Math.Vector4"


matrix4 : Target -> Canonical
matrix4 target =
    case target of
        GuidaTarget ->
            Canonical Pkg.stdlib "Math.Matrix4"

        ElmTarget ->
            Canonical Pkg.linearAlgebra "Math.Matrix4"



-- ENCODERS and DECODERS


canonicalEncoder : Canonical -> BE.Encoder
canonicalEncoder (Canonical pkgName name) =
    BE.sequence
        [ Pkg.nameEncoder pkgName
        , BE.string name
        ]


canonicalDecoder : BD.Decoder Canonical
canonicalDecoder =
    BD.map2 Canonical
        Pkg.nameDecoder
        BD.string


rawEncoder : Raw -> BE.Encoder
rawEncoder =
    BE.string


rawDecoder : BD.Decoder Raw
rawDecoder =
    BD.string
