module Utils.Bytes.Decode exposing
    ( Decoder
    , andThen
    , assocListDict
    , bool
    , decode
    , everySet
    , fail
    , float
    , int
    , jsonPair
    , lazy
    , list
    , map
    , map2
    , map3
    , map4
    , map5
    , map6
    , map7
    , map8
    , maybe
    , nonempty
    , oneOrMore
    , result
    , string
    , succeed
    , unit
    , unsignedInt8
    )

import Bytes
import Bytes.Decode as BD
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore exposing (OneOrMore)
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)


endian : Bytes.Endianness
endian =
    Bytes.BE


type alias Decoder a =
    BD.Decoder a


unsignedInt8 : Decoder Int
unsignedInt8 =
    BD.unsignedInt8


decode : Decoder a -> Bytes.Bytes -> Maybe a
decode =
    BD.decode


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen =
    BD.andThen


string : Decoder String
string =
    BD.unsignedInt32 endian
        |> BD.andThen BD.string


unit : Decoder ()
unit =
    BD.unsignedInt8
        |> andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed ()

                    _ ->
                        BD.fail
            )


int : Decoder Int
int =
    BD.float64 endian |> BD.map round


float : Decoder Float
float =
    BD.float64 endian


bool : Decoder Bool
bool =
    BD.map ((==) 1) unsignedInt8


list : Decoder a -> Decoder (List a)
list decoder =
    BD.unsignedInt32 endian
        |> andThen (\len -> BD.loop ( len, [] ) (listStep decoder))


listStep : Decoder a -> ( Int, List a ) -> Decoder (BD.Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        succeed (BD.Done (List.reverse xs))

    else
        map (\x -> BD.Loop ( n - 1, x :: xs )) decoder


succeed : a -> Decoder a
succeed =
    BD.succeed


fail : Decoder a
fail =
    BD.fail


maybe : Decoder a -> Decoder (Maybe a)
maybe decoder =
    unsignedInt8
        |> andThen
            (\n ->
                if n == 0 then
                    succeed Nothing

                else
                    map Just decoder
            )


result : Decoder x -> Decoder a -> Decoder (Result x a)
result errDecoder successDecoder =
    BD.unsignedInt8
        |> andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map Ok successDecoder

                    1 ->
                        BD.map Err errDecoder

                    _ ->
                        BD.fail
            )


map : (a -> b) -> Decoder a -> Decoder b
map =
    BD.map


map2 : (a -> b -> result) -> Decoder a -> Decoder b -> Decoder result
map2 =
    BD.map2


map3 : (a -> b -> c -> result) -> Decoder a -> Decoder b -> Decoder c -> Decoder result
map3 =
    BD.map3


map4 : (a -> b -> c -> d -> result) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder result
map4 =
    BD.map4


map5 : (a -> b -> c -> d -> e -> result) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder result
map5 =
    BD.map5


map6 : (a -> b -> c -> d -> e -> f -> result) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder result
map6 func decodeA decodeB decodeC decodeD decodeE decodeF =
    map5 (\a b c d ( e, f ) -> func a b c d e f)
        decodeA
        decodeB
        decodeC
        decodeD
        (BD.map2 Tuple.pair
            decodeE
            decodeF
        )


map7 : (a -> b -> c -> d -> e -> f -> g -> result) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder result
map7 func decodeA decodeB decodeC decodeD decodeE decodeF decodeG =
    map6 (\a b c d e ( f, g ) -> func a b c d e f g)
        decodeA
        decodeB
        decodeC
        decodeD
        decodeE
        (BD.map2 Tuple.pair
            decodeF
            decodeG
        )


map8 : (a -> b -> c -> d -> e -> f -> g -> h -> result) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder result
map8 func decodeA decodeB decodeC decodeD decodeE decodeF decodeG decodeH =
    map7 (\a b c d e f ( g, h ) -> func a b c d e f g h)
        decodeA
        decodeB
        decodeC
        decodeD
        decodeE
        decodeF
        (BD.map2 Tuple.pair
            decodeG
            decodeH
        )


assocListDict : (k -> comparable) -> Decoder k -> Decoder v -> Decoder (Dict comparable k v)
assocListDict toComparable keyDecoder valueDecoder =
    list (jsonPair keyDecoder valueDecoder)
        |> map (Dict.fromList toComparable)


jsonPair : Decoder a -> Decoder b -> Decoder ( a, b )
jsonPair =
    map2 Tuple.pair


everySet : (a -> comparable) -> Decoder a -> Decoder (EverySet comparable a)
everySet toComparable decoder =
    list decoder
        |> map (EverySet.fromList toComparable)


nonempty : Decoder a -> Decoder (NE.Nonempty a)
nonempty decoder =
    list decoder
        |> andThen
            (\values ->
                case values of
                    x :: xs ->
                        succeed (NE.Nonempty x xs)

                    [] ->
                        fail
            )


oneOrMore : Decoder a -> Decoder (OneOrMore a)
oneOrMore decoder =
    BD.unsignedInt8
        |> andThen
            (\idx ->
                case idx of
                    0 ->
                        map OneOrMore.one decoder

                    1 ->
                        map2 OneOrMore.more
                            (lazy (\_ -> oneOrMore decoder))
                            (lazy (\_ -> oneOrMore decoder))

                    _ ->
                        fail
            )


lazy : (() -> Decoder a) -> Decoder a
lazy f =
    andThen f (succeed ())
