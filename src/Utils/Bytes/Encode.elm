module Utils.Bytes.Encode exposing
    ( Encoder
    , assocListDict
    , bool
    , encode
    , everySet
    , float
    , int
    , jsonPair
    , list
    , maybe
    , nonempty
    , oneOrMore
    , result
    , sequence
    , string
    , unit
    , unsignedInt8
    )

import Bytes
import Bytes.Encode as BE
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore exposing (OneOrMore(..))
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)


endian : Bytes.Endianness
endian =
    Bytes.BE


type alias Encoder =
    BE.Encoder


unsignedInt8 : Int -> Encoder
unsignedInt8 =
    BE.unsignedInt8


sequence : List Encoder -> Encoder
sequence =
    BE.sequence


encode : Encoder -> Bytes.Bytes
encode =
    BE.encode


unit : () -> Encoder
unit () =
    BE.unsignedInt8 0


int : Int -> Encoder
int =
    toFloat >> BE.float64 endian


float : Float -> Encoder
float =
    BE.float64 endian


string : String -> Encoder
string str =
    sequence
        [ BE.unsignedInt32 endian (BE.getStringWidth str)
        , BE.string str
        ]


bool : Bool -> Encoder
bool value =
    BE.unsignedInt8
        (if value then
            1

         else
            0
        )


list : (a -> Encoder) -> List a -> Encoder
list encoder aList =
    BE.sequence
        (BE.unsignedInt32 endian (List.length aList)
            :: List.map encoder aList
        )


maybe : (a -> Encoder) -> Maybe a -> Encoder
maybe encoder maybeValue =
    case maybeValue of
        Just value ->
            BE.sequence
                [ BE.unsignedInt8 1
                , encoder value
                ]

        Nothing ->
            BE.unsignedInt8 0


nonempty : (a -> Encoder) -> NE.Nonempty a -> Encoder
nonempty encoder (NE.Nonempty x xs) =
    list encoder (x :: xs)


result : (x -> Encoder) -> (a -> Encoder) -> Result x a -> Encoder
result errEncoder successEncoder resultValue =
    case resultValue of
        Ok value ->
            sequence
                [ BE.unsignedInt8 0
                , successEncoder value
                ]

        Err err ->
            sequence
                [ BE.unsignedInt8 1
                , errEncoder err
                ]


assocListDict : (k -> k -> Order) -> (k -> Encoder) -> (v -> Encoder) -> Dict c k v -> Encoder
assocListDict keyComparison keyEncoder valueEncoder =
    list (jsonPair keyEncoder valueEncoder) << List.reverse << Dict.toList keyComparison


jsonPair : (a -> Encoder) -> (b -> Encoder) -> ( a, b ) -> Encoder
jsonPair encoderA encoderB ( a, b ) =
    BE.sequence
        [ encoderA a
        , encoderB b
        ]


everySet : (a -> a -> Order) -> (a -> Encoder) -> EverySet c a -> Encoder
everySet keyComparison encoder =
    list encoder << List.reverse << EverySet.toList keyComparison


oneOrMore : (a -> Encoder) -> OneOrMore a -> Encoder
oneOrMore encoder oneOrMore_ =
    case oneOrMore_ of
        One value ->
            BE.sequence
                [ BE.unsignedInt8 0
                , encoder value
                ]

        More left right ->
            BE.sequence
                [ BE.unsignedInt8 1
                , oneOrMore encoder left
                , oneOrMore encoder right
                ]
