module Serialize exposing
    ( encodeToJson, decodeFromJson, encodeToBytes, decodeFromBytes, encodeToString, decodeFromString, getJsonDecoder
    , Codec, Error(..)
    , string, bool, float, int, unit, bytes, byte
    , maybe, list, array, dict, set, tuple, triple, result, enum
    , RecordCodec, record, field, finishRecord
    , CustomTypeCodec, customType, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, finishCustomType, VariantEncoder
    , map, mapValid, mapError
    , lazy
    , variant9
    )

{-| Ref.: **Initial implementation from `MartinSStewart/elm-serialize/1.3.1`**


# Serialization

You have three options when encoding data. You can represent the data either as json, bytes, or a string.
Here's some advice when choosing:

  - If performance is important, use `encodeToJson` and `decodeFromJson`
  - If space efficiency is important, use `encodeToBytes` and `decodeFromBytes`\*
  - `encodeToString` and `decodeFromString` are good for URL safe strings but otherwise one of the other choices is probably better.

\*`encodeToJson` is more compact when encoding integers with 6 or fewer digits. You may want to try both `encodeToBytes` and `encodeToJson` and see which is better for your use case.

@docs encodeToJson, decodeFromJson, encodeToBytes, decodeFromBytes, encodeToString, decodeFromString, getJsonDecoder


# Definition

@docs Codec, Error


# Primitives

@docs string, bool, float, int, unit, bytes, byte


# Data Structures

@docs maybe, list, array, dict, set, tuple, triple, result, enum


# Records

@docs RecordCodec, record, field, finishRecord


# Custom Types

@docs CustomTypeCodec, customType, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, finishCustomType, VariantEncoder


# Mapping

@docs map, mapValid, mapError


# Stack unsafe

@docs lazy

-}

import Array exposing (Array)
import Base64
import Bytes
import Bytes.Decode as BD
import Bytes.Encode as BE
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Regex exposing (Regex)
import Set exposing (Set)
import Toop exposing (T4(..), T5(..), T6(..), T7(..), T8(..), T9(..))



-- DEFINITION


{-| A value that knows how to encode and decode an Elm data structure.
-}
type Codec e a
    = Codec
        { encoder : a -> BE.Encoder
        , decoder : BD.Decoder (Result (Error e) a)
        , jsonEncoder : a -> JE.Value
        , jsonDecoder : JD.Decoder (Result (Error e) a)
        }


{-| Possible errors that can occur when decoding.

  - `CustomError` - An error caused by `andThen` returning an Err value.
  - `DataCorrupted` - This most likely will occur if you make breaking changes to your codec and try to decode old data\*. Have a look at `How do I change my codecs and still be able to decode old data?` in the readme for how to avoid introducing breaking changes.
  - `SerializerOutOfDate` - When encoding, this package will include a version number. This makes it possible for me to make improvements to how data gets encoded without introducing breaking changes to your codecs. This error then, says that you're trying to decode data encoded with a newer version of elm-serialize.

\*It's possible for corrupted data to still succeed in decoding (but with nonsense Elm values).
This is because internally we're just encoding Elm values and not storing any kind of structural information.
So if you encoded an Int and then a Float, and then tried decoding it as a Float and then an Int, there's no way for the decoder to know it read the data in the wrong order.

-}
type Error e
    = CustomError e
    | DataCorrupted
    | SerializerOutOfDate


version : Int
version =
    1



-- DECODE


endian : Bytes.Endianness
endian =
    Bytes.BE


{-| Extracts the `Decoder` contained inside the `Codec`.
-}
getBytesDecoderHelper : Codec e a -> BD.Decoder (Result (Error e) a)
getBytesDecoderHelper (Codec m) =
    m.decoder


{-| Extracts the json `Decoder` contained inside the `Codec`.
-}
getJsonDecoderHelper : Codec e a -> JD.Decoder (Result (Error e) a)
getJsonDecoderHelper (Codec m) =
    m.jsonDecoder


{-| Run a `Codec` to turn a sequence of bytes into an Elm value.
-}
decodeFromBytes : Codec e a -> Bytes.Bytes -> Result (Error e) a
decodeFromBytes codec bytes_ =
    let
        decoder =
            BD.unsignedInt8
                |> BD.andThen
                    (\value ->
                        if value <= 0 then
                            Err DataCorrupted |> BD.succeed

                        else if value == version then
                            getBytesDecoderHelper codec

                        else
                            Err SerializerOutOfDate |> BD.succeed
                    )
    in
    case BD.decode decoder bytes_ of
        Just value ->
            value

        Nothing ->
            Err DataCorrupted



--{-| Get the decoder from a `Codec` which you can use inside a elm/bytes decoder. Note that if you do this, you lose any error information that might have been returned.
---}
--getBytesDecoder : Codec e a -> BD.Decoder a
--getBytesDecoder codec =
--    BD.unsignedInt8
--        |> BD.andThen
--            (\value ->
--                if value <= 0 then
--                    BD.fail
--
--                else if value == version then
--                    getBytesDecoderHelper codec
--                        |> BD.andThen
--                            (\result_ ->
--                                case result_ of
--                                    Ok ok ->
--                                        BD.succeed ok
--
--                                    Err _ ->
--                                        BD.fail
--                            )
--
--                else
--                    BD.fail
--            )


{-| Run a `Codec` to turn a String encoded with `encodeToString` into an Elm value.
-}
decodeFromString : Codec e a -> String -> Result (Error e) a
decodeFromString codec base64 =
    case decode base64 of
        Just bytes_ ->
            decodeFromBytes codec bytes_

        Nothing ->
            Err DataCorrupted


{-| Run a `Codec` to turn a json value encoded with `encodeToJson` into an Elm value.
-}
decodeFromJson : Codec e a -> JE.Value -> Result (Error e) a
decodeFromJson codec json =
    let
        decoder =
            JD.index 0 JD.int
                |> JD.andThen
                    (\value ->
                        if value <= 0 then
                            Err DataCorrupted |> JD.succeed

                        else if value == version then
                            JD.index 1 (getJsonDecoderHelper codec)

                        else
                            Err SerializerOutOfDate |> JD.succeed
                    )
    in
    case JD.decodeValue decoder json of
        Ok value ->
            value

        Err _ ->
            Err DataCorrupted


{-| Get the decoder from a `Codec` which you can use inside a elm/json decoder.

    import Json.Decode
    import Serialize

    type alias Point =
        { x : Float, y : Float }

    pointCodec : Serialize.Codec e Point
    pointCodec =
        Serialize.record Point
            |> Serialize.field .x Serialize.float
            |> Serialize.field .y Serialize.float
            |> Serialize.finishRecord

    pointDecoder : Json.Decode.Decoder Point
    pointDecoder =
        -- Since pointCodec doesn't have any custom error values, we can use `never` for our errorToString parameter.
        Serialize.getJsonDecoder never pointCodec

-}
getJsonDecoder : (e -> String) -> Codec e a -> JD.Decoder a
getJsonDecoder errorToString codec =
    JD.value
        |> JD.andThen
            (\value ->
                case decodeFromJson codec value of
                    Ok ok ->
                        JD.succeed ok

                    Err (CustomError error) ->
                        errorToString error |> JD.fail

                    Err DataCorrupted ->
                        JD.fail "Data corrupted (elm-serialize error)"

                    Err SerializerOutOfDate ->
                        JD.fail "Serializer out of date (elm-serialize error)"
            )


decode : String -> Maybe Bytes.Bytes
decode base64text =
    let
        replaceChar rematch =
            case rematch.match of
                "-" ->
                    "+"

                _ ->
                    "/"

        strlen =
            String.length base64text
    in
    if strlen == 0 then
        BE.encode (BE.sequence []) |> Just

    else
        let
            hanging =
                modBy 4 strlen

            ilen =
                if hanging == 0 then
                    0

                else
                    4 - hanging
        in
        Regex.replace replaceFromUrl replaceChar (base64text ++ String.repeat ilen "=") |> Base64.toBytes


replaceFromUrl : Regex
replaceFromUrl =
    Regex.fromString "[-_]" |> Maybe.withDefault Regex.never



-- ENCODE


{-| Extracts the encoding function contained inside the `Codec`.
-}
getBytesEncoderHelper : Codec e a -> a -> BE.Encoder
getBytesEncoderHelper (Codec m) =
    m.encoder


{-| Extracts the json encoding function contained inside the `Codec`.
-}
getJsonEncoderHelper : Codec e a -> a -> JE.Value
getJsonEncoderHelper (Codec m) =
    m.jsonEncoder


{-| Convert an Elm value into a sequence of bytes.
-}
encodeToBytes : Codec e a -> a -> Bytes.Bytes
encodeToBytes codec value =
    BE.sequence
        [ BE.unsignedInt8 version
        , value |> getBytesEncoderHelper codec
        ]
        |> BE.encode


{-| Convert an Elm value into a string. This string contains only url safe characters, so you can do the following:

    import Serialize as S

    myUrl =
        "www.mywebsite.com/?data=" ++ S.encodeToString S.float 1234

and not risk generating an invalid url.

-}
encodeToString : Codec e a -> a -> String
encodeToString codec =
    encodeToBytes codec >> replaceBase64Chars


{-| Convert an Elm value into json data.
-}
encodeToJson : Codec e a -> a -> JE.Value
encodeToJson codec value =
    JE.list
        identity
        [ JE.int version
        , value |> getJsonEncoderHelper codec
        ]


replaceBase64Chars : Bytes.Bytes -> String
replaceBase64Chars =
    let
        replaceChar rematch =
            case rematch.match of
                "+" ->
                    "-"

                "/" ->
                    "_"

                _ ->
                    ""
    in
    Base64.fromBytes >> Maybe.withDefault "" >> Regex.replace replaceForUrl replaceChar


replaceForUrl : Regex
replaceForUrl =
    Regex.fromString "[\\+/=]" |> Maybe.withDefault Regex.never



-- BASE


build :
    (a -> BE.Encoder)
    -> BD.Decoder (Result (Error e) a)
    -> (a -> JE.Value)
    -> JD.Decoder (Result (Error e) a)
    -> Codec e a
build encoder_ decoder_ jsonEncoder jsonDecoder =
    Codec
        { encoder = encoder_
        , decoder = decoder_
        , jsonEncoder = jsonEncoder
        , jsonDecoder = jsonDecoder
        }


{-| Codec for serializing a `String`
-}
string : Codec e String
string =
    build
        (\text ->
            BE.sequence
                [ BE.unsignedInt32 endian (BE.getStringWidth text)
                , BE.string text
                ]
        )
        (BD.unsignedInt32 endian
            |> BD.andThen
                (\charCount -> BD.string charCount |> BD.map Ok)
        )
        JE.string
        (JD.string |> JD.map Ok)


{-| Codec for serializing a `Bool`
-}
bool : Codec e Bool
bool =
    build
        (\value ->
            if value then
                BE.unsignedInt8 1

            else
                BE.unsignedInt8 0
        )
        (BD.unsignedInt8
            |> BD.map
                (\value ->
                    case value of
                        0 ->
                            Ok False

                        1 ->
                            Ok True

                        _ ->
                            Err DataCorrupted
                )
        )
        JE.bool
        (JD.bool |> JD.map Ok)


{-| Codec for serializing an `Int`
-}
int : Codec e Int
int =
    build
        (toFloat >> BE.float64 endian)
        (BD.float64 endian |> BD.map (round >> Ok))
        JE.int
        (JD.int |> JD.map Ok)


{-| Codec for serializing a `Float`
-}
float : Codec e Float
float =
    build
        (BE.float64 endian)
        (BD.float64 endian |> BD.map Ok)
        JE.float
        (JD.float |> JD.map Ok)



-- DATA STRUCTURES


{-| Codec for serializing a `Maybe`

    import Serialize as S

    maybeIntCodec : S.Codec e (Maybe Int)
    maybeIntCodec =
        S.maybe S.int

-}
maybe : Codec e a -> Codec e (Maybe a)
maybe justCodec =
    customType
        (\nothingEncoder justEncoder value ->
            case value of
                Nothing ->
                    nothingEncoder

                Just value_ ->
                    justEncoder value_
        )
        |> variant0 Nothing
        |> variant1 Just justCodec
        |> finishCustomType


{-| Codec for serializing a `List`

    import Serialize as S

    listOfStringsCodec : S.Codec e (List String)
    listOfStringsCodec =
        S.list S.string

-}
list : Codec e a -> Codec e (List a)
list codec =
    build
        (listEncode (getBytesEncoderHelper codec))
        (BD.unsignedInt32 endian
            |> BD.andThen
                (\length -> BD.loop ( length, [] ) (listStep (getBytesDecoderHelper codec)))
        )
        (JE.list (getJsonEncoderHelper codec))
        (JD.list (getJsonDecoderHelper codec)
            |> JD.map
                (List.foldr
                    (\value state ->
                        case ( value, state ) of
                            ( Ok ok, Ok okState ) ->
                                ok :: okState |> Ok

                            ( _, Err _ ) ->
                                state

                            ( Err error, Ok _ ) ->
                                Err error
                    )
                    (Ok [])
                )
        )


listEncode : (a -> BE.Encoder) -> List a -> BE.Encoder
listEncode encoder_ list_ =
    list_
        |> List.map encoder_
        |> (::) (BE.unsignedInt32 endian (List.length list_))
        |> BE.sequence


listStep : BD.Decoder (Result (Error e) a) -> ( Int, List a ) -> BD.Decoder (BD.Step ( Int, List a ) (Result (Error e) (List a)))
listStep decoder_ ( n, xs ) =
    if n <= 0 then
        BD.succeed (BD.Done (xs |> List.reverse |> Ok))

    else
        BD.map
            (\x ->
                case x of
                    Ok ok ->
                        BD.Loop ( n - 1, ok :: xs )

                    Err err ->
                        BD.Done (Err err)
            )
            decoder_


{-| Codec for serializing an `Array`
-}
array : Codec e a -> Codec e (Array a)
array codec =
    list codec |> mapHelper (Result.map Array.fromList) Array.toList


{-| Codec for serializing a `Dict`

    import Serialize as S

    type alias Name =
        String

    peoplesAgeCodec : S.Codec e (Dict Name Int)
    peoplesAgeCodec =
        S.dict S.string S.int

-}
dict : Codec e comparable -> Codec e a -> Codec e (Dict comparable a)
dict keyCodec valueCodec =
    list (tuple keyCodec valueCodec)
        |> mapHelper (Result.map Dict.fromList) Dict.toList


{-| Codec for serializing a `Set`
-}
set : Codec e comparable -> Codec e (Set comparable)
set codec =
    list codec |> mapHelper (Result.map Set.fromList) Set.toList


{-| Codec for serializing `()` (aka `Unit`).
-}
unit : Codec e ()
unit =
    build
        (always (BE.sequence []))
        (BD.succeed (Ok ()))
        (\_ -> JE.int 0)
        (JD.succeed (Ok ()))


{-| Codec for serializing a tuple with 2 elements

    import Serialize as S

    pointCodec : S.Codec e ( Float, Float )
    pointCodec =
        S.tuple S.float S.float

-}
tuple : Codec e a -> Codec e b -> Codec e ( a, b )
tuple codecFirst codecSecond =
    record Tuple.pair
        |> field Tuple.first codecFirst
        |> field Tuple.second codecSecond
        |> finishRecord


{-| Codec for serializing a tuple with 3 elements

    import Serialize as S

    pointCodec : S.Codec e ( Float, Float, Float )
    pointCodec =
        S.tuple S.float S.float S.float

-}
triple : Codec e a -> Codec e b -> Codec e c -> Codec e ( a, b, c )
triple codecFirst codecSecond codecThird =
    record (\a b c -> ( a, b, c ))
        |> field (\( a, _, _ ) -> a) codecFirst
        |> field (\( _, b, _ ) -> b) codecSecond
        |> field (\( _, _, c ) -> c) codecThird
        |> finishRecord


{-| Codec for serializing a `Result`
-}
result : Codec e error -> Codec e value -> Codec e (Result error value)
result errorCodec valueCodec =
    customType
        (\errEncoder okEncoder value ->
            case value of
                Err err ->
                    errEncoder err

                Ok ok ->
                    okEncoder ok
        )
        |> variant1 Err errorCodec
        |> variant1 Ok valueCodec
        |> finishCustomType


{-| Codec for serializing [`Bytes`](https://package.elm-lang.org/packages/elm/bytes/latest/).
This is useful in combination with `mapValid` for encoding and decoding data using some specialized format.

    import Image exposing (Image)
    import Serialize as S

    imageCodec : S.Codec String Image
    imageCodec =
        S.bytes
            |> S.mapValid
                (Image.decode >> Result.fromMaybe "Failed to decode PNG image.")
                Image.toPng

-}
bytes : Codec e Bytes.Bytes
bytes =
    build
        (\bytes_ ->
            BE.sequence
                [ BE.unsignedInt32 endian (Bytes.width bytes_)
                , BE.bytes bytes_
                ]
        )
        (BD.unsignedInt32 endian |> BD.andThen (\length -> BD.bytes length |> BD.map Ok))
        (replaceBase64Chars >> JE.string)
        (JD.string
            |> JD.map
                (\text ->
                    case decode text of
                        Just bytes_ ->
                            Ok bytes_

                        Nothing ->
                            Err DataCorrupted
                )
        )


{-| Codec for serializing an integer ranging from 0 to 255.
This is useful if you have a small integer you want to serialize and not use up a lot of space.

    import Serialize as S

    type alias Color =
        { red : Int
        , green : Int
        , blue : Int
        }

    color : S.Codec e Color
    color =
        Color.record Color
            |> S.field .red byte
            |> S.field .green byte
            |> S.field .blue byte
            |> S.finishRecord

**Warning:** values greater than 255 or less than 0 will wrap around.
So if you encode -1 you'll get back 255 and if you encode 257 you'll get back 1.

-}
byte : Codec e Int
byte =
    build
        BE.unsignedInt8
        (BD.unsignedInt8 |> BD.map Ok)
        (modBy 256 >> JE.int)
        (JD.int |> JD.map Ok)


{-| A codec for serializing an item from a list of possible items.
If you try to encode an item that isn't in the list then the first item is defaulted to.

    import Serialize as S

    type DaysOfWeek
        = Monday
        | Tuesday
        | Wednesday
        | Thursday
        | Friday
        | Saturday
        | Sunday

    daysOfWeekCodec : S.Codec e DaysOfWeek
    daysOfWeekCodec =
        S.enum Monday [ Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday ]

Note that inserting new items in the middle of the list or removing items is a breaking change.
It's safe to add items to the end of the list though.

-}
enum : a -> List a -> Codec e a
enum defaultItem items =
    let
        getIndex value =
            items
                |> findIndex ((==) value)
                |> Maybe.withDefault -1
                |> (+) 1

        getItem index =
            if index < 0 then
                Err DataCorrupted

            else if index > List.length items then
                Err DataCorrupted

            else
                getAt (index - 1) items |> Maybe.withDefault defaultItem |> Ok
    in
    build
        (getIndex >> BE.unsignedInt32 endian)
        (BD.unsignedInt32 endian |> BD.map getItem)
        (getIndex >> JE.int)
        (JD.int |> JD.map getItem)


getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs


{-| <https://github.com/elm-community/list-extra/blob/f9faf1cfa1cec24f977313b1b63e2a1064c36eed/src/List/Extra.elm#L620>
-}
findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex =
    findIndexHelp 0


{-| <https://github.com/elm-community/list-extra/blob/f9faf1cfa1cec24f977313b1b63e2a1064c36eed/src/List/Extra.elm#L625>
-}
findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp index predicate list_ =
    case list_ of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index

            else
                findIndexHelp (index + 1) predicate xs



-- OBJECTS


{-| A partially built Codec for a record.
-}
type RecordCodec e a b
    = RecordCodec
        { encoder : a -> List BE.Encoder
        , decoder : BD.Decoder (Result (Error e) b)
        , jsonEncoder : a -> List JE.Value
        , jsonDecoder : JD.Decoder (Result (Error e) b)
        , fieldIndex : Int
        }


{-| Start creating a codec for a record.

    import Serialize as S

    type alias Point =
        { x : Int
        , y : Int
        }

    pointCodec : S.Codec e Point
    pointCodec =
        S.record Point
            -- Note that adding, removing, or reordering fields will prevent you from decoding any data you've previously encoded.
            |> S.field .x S.int
            |> S.field .y S.int
            |> S.finishRecord

-}
record : b -> RecordCodec e a b
record ctor =
    RecordCodec
        { encoder = \_ -> []
        , decoder = BD.succeed (Ok ctor)
        , jsonEncoder = \_ -> []
        , jsonDecoder = JD.succeed (Ok ctor)
        , fieldIndex = 0
        }


{-| Add a field to the record we are creating a codec for.
-}
field : (a -> f) -> Codec e f -> RecordCodec e a (f -> b) -> RecordCodec e a b
field getter codec (RecordCodec recordCodec) =
    RecordCodec
        { encoder = \v -> (getBytesEncoderHelper codec <| getter v) :: recordCodec.encoder v
        , decoder =
            BD.map2
                (\f x ->
                    case ( f, x ) of
                        ( Ok fOk, Ok xOk ) ->
                            fOk xOk |> Ok

                        ( Err err, _ ) ->
                            Err err

                        ( _, Err err ) ->
                            Err err
                )
                recordCodec.decoder
                (getBytesDecoderHelper codec)
        , jsonEncoder = \v -> (getJsonEncoderHelper codec <| getter v) :: recordCodec.jsonEncoder v
        , jsonDecoder =
            JD.map2
                (\f x ->
                    case ( f, x ) of
                        ( Ok fOk, Ok xOk ) ->
                            fOk xOk |> Ok

                        ( Err err, _ ) ->
                            Err err

                        ( _, Err err ) ->
                            Err err
                )
                recordCodec.jsonDecoder
                (JD.index recordCodec.fieldIndex (getJsonDecoderHelper codec))
        , fieldIndex = recordCodec.fieldIndex + 1
        }


{-| Finish creating a codec for a record.
-}
finishRecord : RecordCodec e a a -> Codec e a
finishRecord (RecordCodec codec) =
    Codec
        { encoder = codec.encoder >> List.reverse >> BE.sequence
        , decoder = codec.decoder
        , jsonEncoder = codec.jsonEncoder >> List.reverse >> JE.list identity
        , jsonDecoder = codec.jsonDecoder
        }



-- CUSTOM


{-| A partially built codec for a custom type.
-}
type CustomTypeCodec a e match v
    = CustomTypeCodec
        { match : match
        , jsonMatch : match
        , decoder : Int -> BD.Decoder (Result (Error e) v) -> BD.Decoder (Result (Error e) v)
        , jsonDecoder : Int -> JD.Decoder (Result (Error e) v) -> JD.Decoder (Result (Error e) v)
        , idCounter : Int
        }


{-| Starts building a `Codec` for a custom type.
You need to pass a pattern matching function, see the FAQ for details.

    import Serialize as S

    type Semaphore
        = Red Int String Bool
        | Yellow Float
        | Green

    semaphoreCodec : S.Codec e Semaphore
    semaphoreCodec =
        S.customType
            (\redEncoder yellowEncoder greenEncoder value ->
                case value of
                    Red i s b ->
                        redEncoder i s b

                    Yellow f ->
                        yellowEncoder f

                    Green ->
                        greenEncoder
            )
            -- Note that removing a variant, inserting a variant before an existing one, or swapping two variants will prevent you from decoding any data you've previously encoded.
            |> S.variant3 Red S.int S.string S.bool
            |> S.variant1 Yellow S.float
            |> S.variant0 Green
            -- It's safe to add new variants here later though
            |> S.finishCustomType

-}
customType : match -> CustomTypeCodec { youNeedAtLeastOneVariant : () } e match value
customType match =
    CustomTypeCodec
        { match = match
        , jsonMatch = match
        , decoder = \_ -> identity
        , jsonDecoder = \_ -> identity
        , idCounter = 0
        }


{-| -}
type VariantEncoder
    = VariantEncoder ( BE.Encoder, JE.Value )


variant :
    ((List BE.Encoder -> VariantEncoder) -> a)
    -> ((List JE.Value -> VariantEncoder) -> a)
    -> BD.Decoder (Result (Error error) v)
    -> JD.Decoder (Result (Error error) v)
    -> CustomTypeCodec z error (a -> b) v
    -> CustomTypeCodec () error b v
variant matchPiece matchJsonPiece decoderPiece jsonDecoderPiece (CustomTypeCodec am) =
    let
        enc : List BE.Encoder -> VariantEncoder
        enc v =
            ( BE.unsignedInt16 endian am.idCounter :: v |> BE.sequence
            , JE.null
            )
                |> VariantEncoder

        jsonEnc : List JE.Value -> VariantEncoder
        jsonEnc v =
            ( BE.sequence []
            , JE.int am.idCounter :: v |> JE.list identity
            )
                |> VariantEncoder

        decoder_ : Int -> BD.Decoder (Result (Error error) v) -> BD.Decoder (Result (Error error) v)
        decoder_ tag orElse =
            if tag == am.idCounter then
                decoderPiece

            else
                am.decoder tag orElse

        jsonDecoder_ : Int -> JD.Decoder (Result (Error error) v) -> JD.Decoder (Result (Error error) v)
        jsonDecoder_ tag orElse =
            if tag == am.idCounter then
                jsonDecoderPiece

            else
                am.jsonDecoder tag orElse
    in
    CustomTypeCodec
        { match = am.match <| matchPiece enc
        , jsonMatch = am.jsonMatch <| matchJsonPiece jsonEnc
        , decoder = decoder_
        , jsonDecoder = jsonDecoder_
        , idCounter = am.idCounter + 1
        }


{-| Define a variant with 0 parameters for a custom type.
-}
variant0 : v -> CustomTypeCodec z e (VariantEncoder -> a) v -> CustomTypeCodec () e a v
variant0 ctor =
    variant
        (\c -> c [])
        (\c -> c [])
        (BD.succeed (Ok ctor))
        (JD.succeed (Ok ctor))


{-| Define a variant with 1 parameters for a custom type.
-}
variant1 :
    (a -> v)
    -> Codec error a
    -> CustomTypeCodec z error ((a -> VariantEncoder) -> b) v
    -> CustomTypeCodec () error b v
variant1 ctor m1 =
    variant
        (\c v ->
            c
                [ getBytesEncoderHelper m1 v
                ]
        )
        (\c v ->
            c
                [ getJsonEncoderHelper m1 v
                ]
        )
        (BD.map (result1 ctor) (getBytesDecoderHelper m1))
        (JD.map (result1 ctor) (JD.index 1 (getJsonDecoderHelper m1)))


result1 :
    (value -> a)
    -> Result error value
    -> Result error a
result1 ctor value =
    case value of
        Ok ok ->
            ctor ok |> Ok

        Err err ->
            Err err


{-| Define a variant with 2 parameters for a custom type.
-}
variant2 :
    (a -> b -> v)
    -> Codec error a
    -> Codec error b
    -> CustomTypeCodec z error ((a -> b -> VariantEncoder) -> c) v
    -> CustomTypeCodec () error c v
variant2 ctor m1 m2 =
    variant
        (\c v1 v2 ->
            [ getBytesEncoderHelper m1 v1
            , getBytesEncoderHelper m2 v2
            ]
                |> c
        )
        (\c v1 v2 ->
            [ getJsonEncoderHelper m1 v1
            , getJsonEncoderHelper m2 v2
            ]
                |> c
        )
        (BD.map2
            (result2 ctor)
            (getBytesDecoderHelper m1)
            (getBytesDecoderHelper m2)
        )
        (JD.map2
            (result2 ctor)
            (JD.index 1 (getJsonDecoderHelper m1))
            (JD.index 2 (getJsonDecoderHelper m2))
        )


result2 :
    (value -> a -> b)
    -> Result error value
    -> Result error a
    -> Result error b
result2 ctor v1 v2 =
    case ( v1, v2 ) of
        ( Ok ok1, Ok ok2 ) ->
            ctor ok1 ok2 |> Ok

        ( Err err, _ ) ->
            Err err

        ( _, Err err ) ->
            Err err


{-| Define a variant with 3 parameters for a custom type.
-}
variant3 :
    (a -> b -> c -> v)
    -> Codec error a
    -> Codec error b
    -> Codec error c
    -> CustomTypeCodec z error ((a -> b -> c -> VariantEncoder) -> partial) v
    -> CustomTypeCodec () error partial v
variant3 ctor m1 m2 m3 =
    variant
        (\c v1 v2 v3 ->
            [ getBytesEncoderHelper m1 v1
            , getBytesEncoderHelper m2 v2
            , getBytesEncoderHelper m3 v3
            ]
                |> c
        )
        (\c v1 v2 v3 ->
            [ getJsonEncoderHelper m1 v1
            , getJsonEncoderHelper m2 v2
            , getJsonEncoderHelper m3 v3
            ]
                |> c
        )
        (BD.map3
            (result3 ctor)
            (getBytesDecoderHelper m1)
            (getBytesDecoderHelper m2)
            (getBytesDecoderHelper m3)
        )
        (JD.map3
            (result3 ctor)
            (JD.index 1 (getJsonDecoderHelper m1))
            (JD.index 2 (getJsonDecoderHelper m2))
            (JD.index 3 (getJsonDecoderHelper m3))
        )


result3 :
    (value -> a -> b -> c)
    -> Result error value
    -> Result error a
    -> Result error b
    -> Result error c
result3 ctor v1 v2 v3 =
    case ( v1, v2, v3 ) of
        ( Ok ok1, Ok ok2, Ok ok3 ) ->
            ctor ok1 ok2 ok3 |> Ok

        ( Err err, _, _ ) ->
            Err err

        ( _, Err err, _ ) ->
            Err err

        ( _, _, Err err ) ->
            Err err


{-| Define a variant with 4 parameters for a custom type.
-}
variant4 :
    (a -> b -> c -> d -> v)
    -> Codec error a
    -> Codec error b
    -> Codec error c
    -> Codec error d
    -> CustomTypeCodec z error ((a -> b -> c -> d -> VariantEncoder) -> partial) v
    -> CustomTypeCodec () error partial v
variant4 ctor m1 m2 m3 m4 =
    variant
        (\c v1 v2 v3 v4 ->
            [ getBytesEncoderHelper m1 v1
            , getBytesEncoderHelper m2 v2
            , getBytesEncoderHelper m3 v3
            , getBytesEncoderHelper m4 v4
            ]
                |> c
        )
        (\c v1 v2 v3 v4 ->
            [ getJsonEncoderHelper m1 v1
            , getJsonEncoderHelper m2 v2
            , getJsonEncoderHelper m3 v3
            , getJsonEncoderHelper m4 v4
            ]
                |> c
        )
        (BD.map4
            (result4 ctor)
            (getBytesDecoderHelper m1)
            (getBytesDecoderHelper m2)
            (getBytesDecoderHelper m3)
            (getBytesDecoderHelper m4)
        )
        (JD.map4
            (result4 ctor)
            (JD.index 1 (getJsonDecoderHelper m1))
            (JD.index 2 (getJsonDecoderHelper m2))
            (JD.index 3 (getJsonDecoderHelper m3))
            (JD.index 4 (getJsonDecoderHelper m4))
        )


result4 :
    (value -> a -> b -> c -> d)
    -> Result error value
    -> Result error a
    -> Result error b
    -> Result error c
    -> Result error d
result4 ctor v1 v2 v3 v4 =
    case T4 v1 v2 v3 v4 of
        T4 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) ->
            ctor ok1 ok2 ok3 ok4 |> Ok

        T4 (Err err) _ _ _ ->
            Err err

        T4 _ (Err err) _ _ ->
            Err err

        T4 _ _ (Err err) _ ->
            Err err

        T4 _ _ _ (Err err) ->
            Err err


{-| Define a variant with 5 parameters for a custom type.
-}
variant5 :
    (a -> b -> c -> d -> e -> v)
    -> Codec error a
    -> Codec error b
    -> Codec error c
    -> Codec error d
    -> Codec error e
    -> CustomTypeCodec z error ((a -> b -> c -> d -> e -> VariantEncoder) -> partial) v
    -> CustomTypeCodec () error partial v
variant5 ctor m1 m2 m3 m4 m5 =
    variant
        (\c v1 v2 v3 v4 v5 ->
            [ getBytesEncoderHelper m1 v1
            , getBytesEncoderHelper m2 v2
            , getBytesEncoderHelper m3 v3
            , getBytesEncoderHelper m4 v4
            , getBytesEncoderHelper m5 v5
            ]
                |> c
        )
        (\c v1 v2 v3 v4 v5 ->
            [ getJsonEncoderHelper m1 v1
            , getJsonEncoderHelper m2 v2
            , getJsonEncoderHelper m3 v3
            , getJsonEncoderHelper m4 v4
            , getJsonEncoderHelper m5 v5
            ]
                |> c
        )
        (BD.map5
            (result5 ctor)
            (getBytesDecoderHelper m1)
            (getBytesDecoderHelper m2)
            (getBytesDecoderHelper m3)
            (getBytesDecoderHelper m4)
            (getBytesDecoderHelper m5)
        )
        (JD.map5
            (result5 ctor)
            (JD.index 1 (getJsonDecoderHelper m1))
            (JD.index 2 (getJsonDecoderHelper m2))
            (JD.index 3 (getJsonDecoderHelper m3))
            (JD.index 4 (getJsonDecoderHelper m4))
            (JD.index 5 (getJsonDecoderHelper m5))
        )


result5 :
    (value -> a -> b -> c -> d -> e)
    -> Result error value
    -> Result error a
    -> Result error b
    -> Result error c
    -> Result error d
    -> Result error e
result5 ctor v1 v2 v3 v4 v5 =
    case T5 v1 v2 v3 v4 v5 of
        T5 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) (Ok ok5) ->
            ctor ok1 ok2 ok3 ok4 ok5 |> Ok

        T5 (Err err) _ _ _ _ ->
            Err err

        T5 _ (Err err) _ _ _ ->
            Err err

        T5 _ _ (Err err) _ _ ->
            Err err

        T5 _ _ _ (Err err) _ ->
            Err err

        T5 _ _ _ _ (Err err) ->
            Err err


{-| Define a variant with 6 parameters for a custom type.
-}
variant6 :
    (a -> b -> c -> d -> e -> f -> v)
    -> Codec error a
    -> Codec error b
    -> Codec error c
    -> Codec error d
    -> Codec error e
    -> Codec error f
    -> CustomTypeCodec z error ((a -> b -> c -> d -> e -> f -> VariantEncoder) -> partial) v
    -> CustomTypeCodec () error partial v
variant6 ctor m1 m2 m3 m4 m5 m6 =
    variant
        (\c v1 v2 v3 v4 v5 v6 ->
            [ getBytesEncoderHelper m1 v1
            , getBytesEncoderHelper m2 v2
            , getBytesEncoderHelper m3 v3
            , getBytesEncoderHelper m4 v4
            , getBytesEncoderHelper m5 v5
            , getBytesEncoderHelper m6 v6
            ]
                |> c
        )
        (\c v1 v2 v3 v4 v5 v6 ->
            [ getJsonEncoderHelper m1 v1
            , getJsonEncoderHelper m2 v2
            , getJsonEncoderHelper m3 v3
            , getJsonEncoderHelper m4 v4
            , getJsonEncoderHelper m5 v5
            , getJsonEncoderHelper m6 v6
            ]
                |> c
        )
        (BD.map5
            (result6 ctor)
            (getBytesDecoderHelper m1)
            (getBytesDecoderHelper m2)
            (getBytesDecoderHelper m3)
            (getBytesDecoderHelper m4)
            (BD.map2 Tuple.pair
                (getBytesDecoderHelper m5)
                (getBytesDecoderHelper m6)
            )
        )
        (JD.map5
            (result6 ctor)
            (JD.index 1 (getJsonDecoderHelper m1))
            (JD.index 2 (getJsonDecoderHelper m2))
            (JD.index 3 (getJsonDecoderHelper m3))
            (JD.index 4 (getJsonDecoderHelper m4))
            (JD.map2 Tuple.pair
                (JD.index 5 (getJsonDecoderHelper m5))
                (JD.index 6 (getJsonDecoderHelper m6))
            )
        )


result6 :
    (value -> a -> b -> c -> d -> e -> f)
    -> Result error value
    -> Result error a
    -> Result error b
    -> Result error c
    -> ( Result error d, Result error e )
    -> Result error f
result6 ctor v1 v2 v3 v4 ( v5, v6 ) =
    case T6 v1 v2 v3 v4 v5 v6 of
        T6 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) (Ok ok5) (Ok ok6) ->
            ctor ok1 ok2 ok3 ok4 ok5 ok6 |> Ok

        T6 (Err err) _ _ _ _ _ ->
            Err err

        T6 _ (Err err) _ _ _ _ ->
            Err err

        T6 _ _ (Err err) _ _ _ ->
            Err err

        T6 _ _ _ (Err err) _ _ ->
            Err err

        T6 _ _ _ _ (Err err) _ ->
            Err err

        T6 _ _ _ _ _ (Err err) ->
            Err err


{-| Define a variant with 7 parameters for a custom type.
-}
variant7 :
    (a -> b -> c -> d -> e -> f -> g -> v)
    -> Codec error a
    -> Codec error b
    -> Codec error c
    -> Codec error d
    -> Codec error e
    -> Codec error f
    -> Codec error g
    -> CustomTypeCodec z error ((a -> b -> c -> d -> e -> f -> g -> VariantEncoder) -> partial) v
    -> CustomTypeCodec () error partial v
variant7 ctor m1 m2 m3 m4 m5 m6 m7 =
    variant
        (\c v1 v2 v3 v4 v5 v6 v7 ->
            [ getBytesEncoderHelper m1 v1
            , getBytesEncoderHelper m2 v2
            , getBytesEncoderHelper m3 v3
            , getBytesEncoderHelper m4 v4
            , getBytesEncoderHelper m5 v5
            , getBytesEncoderHelper m6 v6
            , getBytesEncoderHelper m7 v7
            ]
                |> c
        )
        (\c v1 v2 v3 v4 v5 v6 v7 ->
            [ getJsonEncoderHelper m1 v1
            , getJsonEncoderHelper m2 v2
            , getJsonEncoderHelper m3 v3
            , getJsonEncoderHelper m4 v4
            , getJsonEncoderHelper m5 v5
            , getJsonEncoderHelper m6 v6
            , getJsonEncoderHelper m7 v7
            ]
                |> c
        )
        (BD.map5
            (result7 ctor)
            (getBytesDecoderHelper m1)
            (getBytesDecoderHelper m2)
            (getBytesDecoderHelper m3)
            (BD.map2 Tuple.pair
                (getBytesDecoderHelper m4)
                (getBytesDecoderHelper m5)
            )
            (BD.map2 Tuple.pair
                (getBytesDecoderHelper m6)
                (getBytesDecoderHelper m7)
            )
        )
        (JD.map5
            (result7 ctor)
            (JD.index 1 (getJsonDecoderHelper m1))
            (JD.index 2 (getJsonDecoderHelper m2))
            (JD.index 3 (getJsonDecoderHelper m3))
            (JD.map2 Tuple.pair
                (JD.index 4 (getJsonDecoderHelper m4))
                (JD.index 5 (getJsonDecoderHelper m5))
            )
            (JD.map2 Tuple.pair
                (JD.index 6 (getJsonDecoderHelper m6))
                (JD.index 7 (getJsonDecoderHelper m7))
            )
        )


result7 :
    (value -> a -> b -> c -> d -> e -> f -> g)
    -> Result error value
    -> Result error a
    -> Result error b
    -> ( Result error c, Result error d )
    -> ( Result error e, Result error f )
    -> Result error g
result7 ctor v1 v2 v3 ( v4, v5 ) ( v6, v7 ) =
    case T7 v1 v2 v3 v4 v5 v6 v7 of
        T7 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) (Ok ok5) (Ok ok6) (Ok ok7) ->
            ctor ok1 ok2 ok3 ok4 ok5 ok6 ok7 |> Ok

        T7 (Err err) _ _ _ _ _ _ ->
            Err err

        T7 _ (Err err) _ _ _ _ _ ->
            Err err

        T7 _ _ (Err err) _ _ _ _ ->
            Err err

        T7 _ _ _ (Err err) _ _ _ ->
            Err err

        T7 _ _ _ _ (Err err) _ _ ->
            Err err

        T7 _ _ _ _ _ (Err err) _ ->
            Err err

        T7 _ _ _ _ _ _ (Err err) ->
            Err err


{-| Define a variant with 8 parameters for a custom type.
-}
variant8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> v)
    -> Codec error a
    -> Codec error b
    -> Codec error c
    -> Codec error d
    -> Codec error e
    -> Codec error f
    -> Codec error g
    -> Codec error h
    -> CustomTypeCodec z error ((a -> b -> c -> d -> e -> f -> g -> h -> VariantEncoder) -> partial) v
    -> CustomTypeCodec () error partial v
variant8 ctor m1 m2 m3 m4 m5 m6 m7 m8 =
    variant
        (\c v1 v2 v3 v4 v5 v6 v7 v8 ->
            [ getBytesEncoderHelper m1 v1
            , getBytesEncoderHelper m2 v2
            , getBytesEncoderHelper m3 v3
            , getBytesEncoderHelper m4 v4
            , getBytesEncoderHelper m5 v5
            , getBytesEncoderHelper m6 v6
            , getBytesEncoderHelper m7 v7
            , getBytesEncoderHelper m8 v8
            ]
                |> c
        )
        (\c v1 v2 v3 v4 v5 v6 v7 v8 ->
            [ getJsonEncoderHelper m1 v1
            , getJsonEncoderHelper m2 v2
            , getJsonEncoderHelper m3 v3
            , getJsonEncoderHelper m4 v4
            , getJsonEncoderHelper m5 v5
            , getJsonEncoderHelper m6 v6
            , getJsonEncoderHelper m7 v7
            , getJsonEncoderHelper m8 v8
            ]
                |> c
        )
        (BD.map5
            (result8 ctor)
            (getBytesDecoderHelper m1)
            (getBytesDecoderHelper m2)
            (BD.map2 Tuple.pair
                (getBytesDecoderHelper m3)
                (getBytesDecoderHelper m4)
            )
            (BD.map2 Tuple.pair
                (getBytesDecoderHelper m5)
                (getBytesDecoderHelper m6)
            )
            (BD.map2 Tuple.pair
                (getBytesDecoderHelper m7)
                (getBytesDecoderHelper m8)
            )
        )
        (JD.map5
            (result8 ctor)
            (JD.index 1 (getJsonDecoderHelper m1))
            (JD.index 2 (getJsonDecoderHelper m2))
            (JD.map2 Tuple.pair
                (JD.index 3 (getJsonDecoderHelper m3))
                (JD.index 4 (getJsonDecoderHelper m4))
            )
            (JD.map2 Tuple.pair
                (JD.index 5 (getJsonDecoderHelper m5))
                (JD.index 6 (getJsonDecoderHelper m6))
            )
            (JD.map2 Tuple.pair
                (JD.index 7 (getJsonDecoderHelper m7))
                (JD.index 8 (getJsonDecoderHelper m8))
            )
        )


result8 :
    (value -> a -> b -> c -> d -> e -> f -> g -> h)
    -> Result error value
    -> Result error a
    -> ( Result error b, Result error c )
    -> ( Result error d, Result error e )
    -> ( Result error f, Result error g )
    -> Result error h
result8 ctor v1 v2 ( v3, v4 ) ( v5, v6 ) ( v7, v8 ) =
    case T8 v1 v2 v3 v4 v5 v6 v7 v8 of
        T8 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) (Ok ok5) (Ok ok6) (Ok ok7) (Ok ok8) ->
            ctor ok1 ok2 ok3 ok4 ok5 ok6 ok7 ok8 |> Ok

        T8 (Err err) _ _ _ _ _ _ _ ->
            Err err

        T8 _ (Err err) _ _ _ _ _ _ ->
            Err err

        T8 _ _ (Err err) _ _ _ _ _ ->
            Err err

        T8 _ _ _ (Err err) _ _ _ _ ->
            Err err

        T8 _ _ _ _ (Err err) _ _ _ ->
            Err err

        T8 _ _ _ _ _ (Err err) _ _ ->
            Err err

        T8 _ _ _ _ _ _ (Err err) _ ->
            Err err

        T8 _ _ _ _ _ _ _ (Err err) ->
            Err err


{-| Define a variant with 9 parameters for a custom type.
-}
variant9 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> v)
    -> Codec error a
    -> Codec error b
    -> Codec error c
    -> Codec error d
    -> Codec error e
    -> Codec error f
    -> Codec error g
    -> Codec error h
    -> Codec error i
    -> CustomTypeCodec z error ((a -> b -> c -> d -> e -> f -> g -> h -> i -> VariantEncoder) -> partial) v
    -> CustomTypeCodec () error partial v
variant9 ctor m1 m2 m3 m4 m5 m6 m7 m8 m9 =
    variant
        (\c v1 v2 v3 v4 v5 v6 v7 v8 v9 ->
            [ getBytesEncoderHelper m1 v1
            , getBytesEncoderHelper m2 v2
            , getBytesEncoderHelper m3 v3
            , getBytesEncoderHelper m4 v4
            , getBytesEncoderHelper m5 v5
            , getBytesEncoderHelper m6 v6
            , getBytesEncoderHelper m7 v7
            , getBytesEncoderHelper m8 v8
            , getBytesEncoderHelper m9 v9
            ]
                |> c
        )
        (\c v1 v2 v3 v4 v5 v6 v7 v8 v9 ->
            [ getJsonEncoderHelper m1 v1
            , getJsonEncoderHelper m2 v2
            , getJsonEncoderHelper m3 v3
            , getJsonEncoderHelper m4 v4
            , getJsonEncoderHelper m5 v5
            , getJsonEncoderHelper m6 v6
            , getJsonEncoderHelper m7 v7
            , getJsonEncoderHelper m8 v8
            , getJsonEncoderHelper m9 v9
            ]
                |> c
        )
        (BD.map5
            (result9 ctor)
            (getBytesDecoderHelper m1)
            (BD.map2 Tuple.pair
                (getBytesDecoderHelper m2)
                (getBytesDecoderHelper m3)
            )
            (BD.map2 Tuple.pair
                (getBytesDecoderHelper m4)
                (getBytesDecoderHelper m5)
            )
            (BD.map2 Tuple.pair
                (getBytesDecoderHelper m6)
                (getBytesDecoderHelper m7)
            )
            (BD.map2 Tuple.pair
                (getBytesDecoderHelper m8)
                (getBytesDecoderHelper m9)
            )
        )
        (JD.map5
            (result9 ctor)
            (JD.index 1 (getJsonDecoderHelper m1))
            (JD.map2 Tuple.pair
                (JD.index 2 (getJsonDecoderHelper m2))
                (JD.index 3 (getJsonDecoderHelper m3))
            )
            (JD.map2 Tuple.pair
                (JD.index 4 (getJsonDecoderHelper m4))
                (JD.index 5 (getJsonDecoderHelper m5))
            )
            (JD.map2 Tuple.pair
                (JD.index 6 (getJsonDecoderHelper m6))
                (JD.index 7 (getJsonDecoderHelper m7))
            )
            (JD.map2 Tuple.pair
                (JD.index 8 (getJsonDecoderHelper m8))
                (JD.index 9 (getJsonDecoderHelper m9))
            )
        )


result9 :
    (value -> a -> b -> c -> d -> e -> f -> g -> h -> i)
    -> Result error value
    -> ( Result error a, Result error b )
    -> ( Result error c, Result error d )
    -> ( Result error e, Result error f )
    -> ( Result error g, Result error h )
    -> Result error i
result9 ctor v1 ( v2, v3 ) ( v4, v5 ) ( v6, v7 ) ( v8, v9 ) =
    case T9 v1 v2 v3 v4 v5 v6 v7 v8 v9 of
        T9 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) (Ok ok5) (Ok ok6) (Ok ok7) (Ok ok8) (Ok ok9) ->
            ctor ok1 ok2 ok3 ok4 ok5 ok6 ok7 ok8 ok9 |> Ok

        T9 (Err err) _ _ _ _ _ _ _ _ ->
            Err err

        T9 _ (Err err) _ _ _ _ _ _ _ ->
            Err err

        T9 _ _ (Err err) _ _ _ _ _ _ ->
            Err err

        T9 _ _ _ (Err err) _ _ _ _ _ ->
            Err err

        T9 _ _ _ _ (Err err) _ _ _ _ ->
            Err err

        T9 _ _ _ _ _ (Err err) _ _ _ ->
            Err err

        T9 _ _ _ _ _ _ (Err err) _ _ ->
            Err err

        T9 _ _ _ _ _ _ _ (Err err) _ ->
            Err err

        T9 _ _ _ _ _ _ _ _ (Err err) ->
            Err err


{-| Finish creating a codec for a custom type.
-}
finishCustomType : CustomTypeCodec () e (a -> VariantEncoder) a -> Codec e a
finishCustomType (CustomTypeCodec am) =
    build
        (am.match >> (\(VariantEncoder ( a, _ )) -> a))
        (BD.unsignedInt16 endian
            |> BD.andThen
                (\tag ->
                    am.decoder tag (BD.succeed (Err DataCorrupted))
                )
        )
        (am.jsonMatch >> (\(VariantEncoder ( _, a )) -> a))
        (JD.index 0 JD.int
            |> JD.andThen
                (\tag ->
                    am.jsonDecoder tag (JD.succeed (Err DataCorrupted))
                )
        )



---- MAPPING


{-| Map from one codec to another codec

    import Serialize as S

    type UserId
        = UserId Int

    userIdCodec : S.Codec e UserId
    userIdCodec =
        S.int |> S.map UserId (\(UserId id) -> id)

Note that there's nothing preventing you from encoding Elm values that will map to some different value when you decode them.
I recommend writing tests for Codecs that use `map` to make sure you get back the same Elm value you put in.
[Here's some helper functions to get you started.](https://github.com/MartinSStewart/elm-geometry-serialize/blob/6f2244c28631ede1b864cb43541d1573dc628904/tests/Tests.elm#L49-L74)

-}
map : (a -> b) -> (b -> a) -> Codec e a -> Codec e b
map fromBytes_ toBytes_ codec =
    mapHelper
        (\value ->
            case value of
                Ok ok ->
                    fromBytes_ ok |> Ok

                Err err ->
                    Err err
        )
        toBytes_
        codec


mapHelper : (Result (Error e) a -> Result (Error e) b) -> (b -> a) -> Codec e a -> Codec e b
mapHelper fromBytes_ toBytes_ codec =
    build
        (\v -> toBytes_ v |> getBytesEncoderHelper codec)
        (getBytesDecoderHelper codec |> BD.map fromBytes_)
        (\v -> toBytes_ v |> getJsonEncoderHelper codec)
        (getJsonDecoderHelper codec |> JD.map fromBytes_)


{-| Map from one codec to another codec in a way that can potentially fail when decoding.

    -- Email module is from https://package.elm-lang.org/packages/tricycle/elm-email/1.0.2/


    import Email
    import Serialize as S

    emailCodec : S.Codec String Float
    emailCodec =
        S.string
            |> S.mapValid
                (\text ->
                    case Email.fromString text of
                        Just email ->
                            Ok email

                        Nothing ->
                            Err "Invalid email"
                )
                Email.toString

Note that there's nothing preventing you from encoding Elm values that will produce Err when you decode them.
I recommend writing tests for Codecs that use `mapValid` to make sure you get back the same Elm value you put in.
[Here's some helper functions to get you started.](https://github.com/MartinSStewart/elm-geometry-serialize/blob/6f2244c28631ede1b864cb43541d1573dc628904/tests/Tests.elm#L49-L74)

-}
mapValid : (a -> Result e b) -> (b -> a) -> Codec e a -> Codec e b
mapValid fromBytes_ toBytes_ codec =
    build
        (\v -> toBytes_ v |> getBytesEncoderHelper codec)
        (getBytesDecoderHelper codec
            |> BD.map
                (\value ->
                    case value of
                        Ok ok ->
                            fromBytes_ ok |> Result.mapError CustomError

                        Err err ->
                            Err err
                )
        )
        (\v -> toBytes_ v |> getJsonEncoderHelper codec)
        (getJsonDecoderHelper codec
            |> JD.map
                (\value ->
                    case value of
                        Ok ok ->
                            fromBytes_ ok |> Result.mapError CustomError

                        Err err ->
                            Err err
                )
        )


{-| Map errors generated by `mapValid`.
-}
mapError : (e1 -> e2) -> Codec e1 a -> Codec e2 a
mapError mapFunc codec =
    build
        (getBytesEncoderHelper codec)
        (getBytesDecoderHelper codec |> BD.map (mapErrorHelper mapFunc))
        (getJsonEncoderHelper codec)
        (getJsonDecoderHelper codec |> JD.map (mapErrorHelper mapFunc))


mapErrorHelper : (e -> a) -> Result (Error e) b -> Result (Error a) b
mapErrorHelper mapFunc =
    Result.mapError
        (\error ->
            case error of
                CustomError custom ->
                    mapFunc custom |> CustomError

                DataCorrupted ->
                    DataCorrupted

                SerializerOutOfDate ->
                    SerializerOutOfDate
        )



-- STACK UNSAFE


{-| Handle situations where you need to define a codec in terms of itself.

    import Serialize as S

    type Peano
        = Peano (Maybe Peano)

    {-| The compiler will complain that this function causes an infinite loop.
    -}
    badPeanoCodec : S.Codec e Peano
    badPeanoCodec =
        S.maybe badPeanoCodec |> S.map Peano (\(Peano a) -> a)

    {-| Now the compiler is happy!
    -}
    goodPeanoCodec : S.Codec e Peano
    goodPeanoCodec =
        S.maybe (S.lazy (\() -> goodPeanoCodec)) |> S.map Peano (\(Peano a) -> a)

**Warning:** This is not stack safe.

In general if you have a type that contains itself, like with our the Peano example, then you're at risk of a stack overflow while decoding.
Even if you're translating your nested data into a list before encoding, you're at risk, because the function translating back after decoding can cause a stack overflow if the original value was nested deeply enough.
Be careful here, and test your codecs using elm-test with larger inputs than you ever expect to see in real life.

-}
lazy : (() -> Codec e a) -> Codec e a
lazy f =
    build
        (\value -> getBytesEncoderHelper (f ()) value)
        (BD.succeed () |> BD.andThen (\() -> getBytesDecoderHelper (f ())))
        (\value -> getJsonEncoderHelper (f ()) value)
        (JD.succeed () |> JD.andThen (\() -> getJsonDecoderHelper (f ())))
