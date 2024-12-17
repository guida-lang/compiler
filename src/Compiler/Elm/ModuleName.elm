module Compiler.Elm.ModuleName exposing
    ( CEMN_Raw
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

import Compiler.Data.Name as Name exposing (CDN_Name)
import Compiler.Elm.Package as Pkg
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P
import Compiler.Parse.Variable as Var
import Json.Decode as Decode
import Json.Encode as Encode
import System.TypeCheck.IO exposing (CEMN_Canonical(..))



-- RAW


type alias CEMN_Raw =
    CDN_Name


toChars : CEMN_Raw -> List Char
toChars =
    Name.toChars


toFilePath : CEMN_Raw -> String
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


toHyphenPath : CEMN_Raw -> String
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


encode : CEMN_Raw -> E.Value
encode =
    E.string


decoder : D.Decoder ( Int, Int ) CEMN_Raw
decoder =
    D.customString parser Tuple.pair



-- PARSER


parser : P.Parser ( Int, Int ) CEMN_Raw
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


compareCanonical : CEMN_Canonical -> CEMN_Canonical -> Order
compareCanonical (CEMN_Canonical pkg1 name1) (CEMN_Canonical pkg2 name2) =
    case compare name1 name2 of
        LT ->
            LT

        EQ ->
            Pkg.compareName pkg1 pkg2

        GT ->
            GT


toComparableCanonical : CEMN_Canonical -> List String
toComparableCanonical (CEMN_Canonical ( author, project ) name) =
    [ author, project, name ]



-- CORE


basics : CEMN_Canonical
basics =
    CEMN_Canonical Pkg.core Name.basics


char : CEMN_Canonical
char =
    CEMN_Canonical Pkg.core Name.char


string : CEMN_Canonical
string =
    CEMN_Canonical Pkg.core Name.string


maybe : CEMN_Canonical
maybe =
    CEMN_Canonical Pkg.core Name.maybe


result : CEMN_Canonical
result =
    CEMN_Canonical Pkg.core Name.result


list : CEMN_Canonical
list =
    CEMN_Canonical Pkg.core Name.list


array : CEMN_Canonical
array =
    CEMN_Canonical Pkg.core Name.array


dict : CEMN_Canonical
dict =
    CEMN_Canonical Pkg.core Name.dict


tuple : CEMN_Canonical
tuple =
    CEMN_Canonical Pkg.core Name.tuple


platform : CEMN_Canonical
platform =
    CEMN_Canonical Pkg.core Name.platform


cmd : CEMN_Canonical
cmd =
    CEMN_Canonical Pkg.core "Platform.Cmd"


sub : CEMN_Canonical
sub =
    CEMN_Canonical Pkg.core "Platform.Sub"


debug : CEMN_Canonical
debug =
    CEMN_Canonical Pkg.core Name.debug



-- HTML


virtualDom : CEMN_Canonical
virtualDom =
    CEMN_Canonical Pkg.virtualDom Name.virtualDom



-- JSON


jsonDecode : CEMN_Canonical
jsonDecode =
    CEMN_Canonical Pkg.json "Json.Decode"


jsonEncode : CEMN_Canonical
jsonEncode =
    CEMN_Canonical Pkg.json "Json.Encode"



-- WEBGL


webgl : CEMN_Canonical
webgl =
    CEMN_Canonical Pkg.webgl "WebGL"


texture : CEMN_Canonical
texture =
    CEMN_Canonical Pkg.webgl "WebGL.Texture"


vector2 : CEMN_Canonical
vector2 =
    CEMN_Canonical Pkg.linearAlgebra "Math.Vector2"


vector3 : CEMN_Canonical
vector3 =
    CEMN_Canonical Pkg.linearAlgebra "Math.Vector3"


vector4 : CEMN_Canonical
vector4 =
    CEMN_Canonical Pkg.linearAlgebra "Math.Vector4"


matrix4 : CEMN_Canonical
matrix4 =
    CEMN_Canonical Pkg.linearAlgebra "Math.Matrix4"



-- ENCODERS and DECODERS


canonicalEncoder : CEMN_Canonical -> Encode.Value
canonicalEncoder (CEMN_Canonical pkgName name) =
    Encode.object
        [ ( "pkgName", Pkg.nameEncoder pkgName )
        , ( "name", Encode.string name )
        ]


canonicalDecoder : Decode.Decoder CEMN_Canonical
canonicalDecoder =
    Decode.map2 CEMN_Canonical
        (Decode.field "pkgName" Pkg.nameDecoder)
        (Decode.field "name" Decode.string)


rawEncoder : CEMN_Raw -> Encode.Value
rawEncoder =
    Encode.string


rawDecoder : Decode.Decoder CEMN_Raw
rawDecoder =
    Decode.string
