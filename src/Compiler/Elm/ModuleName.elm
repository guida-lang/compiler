module Compiler.Elm.ModuleName exposing
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
import Compiler.Elm.Package as Pkg
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P
import Compiler.Parse.Variable as Var
import Json.Decode as Decode
import Json.Encode as Encode
import System.TypeCheck.IO exposing (Canonical(..))



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
                Ok (P.POk P.Consumed (String.slice pos newPos src) newState)

            else if col == newCol then
                Err (P.PErr P.Empty row newCol Tuple.pair)

            else
                Err (P.PErr P.Consumed row newCol Tuple.pair)
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



-- CORE


basics : Canonical
basics =
    Canonical Pkg.core Name.basics


char : Canonical
char =
    Canonical Pkg.core Name.char


string : Canonical
string =
    Canonical Pkg.core Name.string


maybe : Canonical
maybe =
    Canonical Pkg.core Name.maybe


result : Canonical
result =
    Canonical Pkg.core Name.result


list : Canonical
list =
    Canonical Pkg.core Name.list


array : Canonical
array =
    Canonical Pkg.core Name.array


dict : Canonical
dict =
    Canonical Pkg.core Name.dict


tuple : Canonical
tuple =
    Canonical Pkg.core Name.tuple


platform : Canonical
platform =
    Canonical Pkg.core Name.platform


cmd : Canonical
cmd =
    Canonical Pkg.core "Platform.Cmd"


sub : Canonical
sub =
    Canonical Pkg.core "Platform.Sub"


debug : Canonical
debug =
    Canonical Pkg.core Name.debug



-- HTML


virtualDom : Canonical
virtualDom =
    Canonical Pkg.virtualDom Name.virtualDom



-- JSON


jsonDecode : Canonical
jsonDecode =
    Canonical Pkg.json "Json.Decode"


jsonEncode : Canonical
jsonEncode =
    Canonical Pkg.json "Json.Encode"



-- WEBGL


webgl : Canonical
webgl =
    Canonical Pkg.webgl "WebGL"


texture : Canonical
texture =
    Canonical Pkg.webgl "WebGL.Texture"


vector2 : Canonical
vector2 =
    Canonical Pkg.linearAlgebra "Math.Vector2"


vector3 : Canonical
vector3 =
    Canonical Pkg.linearAlgebra "Math.Vector3"


vector4 : Canonical
vector4 =
    Canonical Pkg.linearAlgebra "Math.Vector4"


matrix4 : Canonical
matrix4 =
    Canonical Pkg.linearAlgebra "Math.Matrix4"



-- ENCODERS and DECODERS


canonicalEncoder : Canonical -> Encode.Value
canonicalEncoder (Canonical pkgName name) =
    Encode.object
        [ ( "pkgName", Pkg.nameEncoder pkgName )
        , ( "name", Encode.string name )
        ]


canonicalDecoder : Decode.Decoder Canonical
canonicalDecoder =
    Decode.map2 Canonical
        (Decode.field "pkgName" Pkg.nameDecoder)
        (Decode.field "name" Decode.string)


rawEncoder : Raw -> Encode.Value
rawEncoder =
    Encode.string


rawDecoder : Decode.Decoder Raw
rawDecoder =
    Decode.string
