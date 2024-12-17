module Compiler.Elm.ModuleName exposing
    ( array
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

import Compiler.Data.Name as Name
import Compiler.Elm.Package as Pkg
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P
import Compiler.Parse.Variable as Var
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T



-- RAW


toChars : T.CEMN_Raw -> List Char
toChars =
    Name.toChars


toFilePath : T.CEMN_Raw -> String
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


toHyphenPath : T.CEMN_Raw -> String
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


encode : T.CEMN_Raw -> E.Value
encode =
    E.string


decoder : D.Decoder ( Int, Int ) T.CEMN_Raw
decoder =
    D.customString parser Tuple.pair



-- PARSER


parser : P.Parser ( Int, Int ) T.CEMN_Raw
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


compareCanonical : T.CEMN_Canonical -> T.CEMN_Canonical -> Order
compareCanonical (T.CEMN_Canonical pkg1 name1) (T.CEMN_Canonical pkg2 name2) =
    case compare name1 name2 of
        LT ->
            LT

        EQ ->
            Pkg.compareName pkg1 pkg2

        GT ->
            GT


toComparableCanonical : T.CEMN_Canonical -> List String
toComparableCanonical (T.CEMN_Canonical ( author, project ) name) =
    [ author, project, name ]



-- CORE


basics : T.CEMN_Canonical
basics =
    T.CEMN_Canonical Pkg.core Name.basics


char : T.CEMN_Canonical
char =
    T.CEMN_Canonical Pkg.core Name.char


string : T.CEMN_Canonical
string =
    T.CEMN_Canonical Pkg.core Name.string


maybe : T.CEMN_Canonical
maybe =
    T.CEMN_Canonical Pkg.core Name.maybe


result : T.CEMN_Canonical
result =
    T.CEMN_Canonical Pkg.core Name.result


list : T.CEMN_Canonical
list =
    T.CEMN_Canonical Pkg.core Name.list


array : T.CEMN_Canonical
array =
    T.CEMN_Canonical Pkg.core Name.array


dict : T.CEMN_Canonical
dict =
    T.CEMN_Canonical Pkg.core Name.dict


tuple : T.CEMN_Canonical
tuple =
    T.CEMN_Canonical Pkg.core Name.tuple


platform : T.CEMN_Canonical
platform =
    T.CEMN_Canonical Pkg.core Name.platform


cmd : T.CEMN_Canonical
cmd =
    T.CEMN_Canonical Pkg.core "Platform.Cmd"


sub : T.CEMN_Canonical
sub =
    T.CEMN_Canonical Pkg.core "Platform.Sub"


debug : T.CEMN_Canonical
debug =
    T.CEMN_Canonical Pkg.core Name.debug



-- HTML


virtualDom : T.CEMN_Canonical
virtualDom =
    T.CEMN_Canonical Pkg.virtualDom Name.virtualDom



-- JSON


jsonDecode : T.CEMN_Canonical
jsonDecode =
    T.CEMN_Canonical Pkg.json "Json.Decode"


jsonEncode : T.CEMN_Canonical
jsonEncode =
    T.CEMN_Canonical Pkg.json "Json.Encode"



-- WEBGL


webgl : T.CEMN_Canonical
webgl =
    T.CEMN_Canonical Pkg.webgl "WebGL"


texture : T.CEMN_Canonical
texture =
    T.CEMN_Canonical Pkg.webgl "WebGL.Texture"


vector2 : T.CEMN_Canonical
vector2 =
    T.CEMN_Canonical Pkg.linearAlgebra "Math.Vector2"


vector3 : T.CEMN_Canonical
vector3 =
    T.CEMN_Canonical Pkg.linearAlgebra "Math.Vector3"


vector4 : T.CEMN_Canonical
vector4 =
    T.CEMN_Canonical Pkg.linearAlgebra "Math.Vector4"


matrix4 : T.CEMN_Canonical
matrix4 =
    T.CEMN_Canonical Pkg.linearAlgebra "Math.Matrix4"



-- ENCODERS and DECODERS


canonicalEncoder : T.CEMN_Canonical -> Encode.Value
canonicalEncoder (T.CEMN_Canonical pkgName name) =
    Encode.object
        [ ( "pkgName", Pkg.nameEncoder pkgName )
        , ( "name", Encode.string name )
        ]


canonicalDecoder : Decode.Decoder T.CEMN_Canonical
canonicalDecoder =
    Decode.map2 T.CEMN_Canonical
        (Decode.field "pkgName" Pkg.nameDecoder)
        (Decode.field "name" Decode.string)


rawEncoder : T.CEMN_Raw -> Encode.Value
rawEncoder =
    Encode.string


rawDecoder : Decode.Decoder T.CEMN_Raw
rawDecoder =
    Decode.string
