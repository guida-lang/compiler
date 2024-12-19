module Compiler.Reporting.Render.Type.Localizer exposing
    ( empty
    , fromModule
    , fromNames
    , localizerDecoder
    , localizerEncoder
    , toChars
    , toDoc
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Json.Decode as DecodeX
import Compiler.Json.Encode as EncodeX
import Compiler.Reporting.Doc as D
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T



-- LOCALIZER


empty : T.CRRTL_Localizer
empty =
    T.CRRTL_Localizer Dict.empty



-- LOCALIZE


toDoc : T.CRRTL_Localizer -> T.CEMN_Canonical -> T.CDN_Name -> D.Doc
toDoc localizer home name =
    D.fromChars (toChars localizer home name)


toChars : T.CRRTL_Localizer -> T.CEMN_Canonical -> T.CDN_Name -> String
toChars (T.CRRTL_Localizer localizer) ((T.CEMN_Canonical _ home) as moduleName) name =
    case Dict.get identity home localizer of
        Nothing ->
            home ++ "." ++ name

        Just import_ ->
            case import_.exposing_ of
                T.CRRTL_All ->
                    name

                T.CRRTL_Only set ->
                    if EverySet.member identity name set then
                        name

                    else if name == Name.list && moduleName == ModuleName.list then
                        "List"

                    else
                        Maybe.withDefault home import_.alias ++ "." ++ name



-- FROM NAMES


fromNames : Dict String T.CDN_Name a -> T.CRRTL_Localizer
fromNames names =
    T.CRRTL_Localizer (Dict.map (\_ _ -> { alias = Nothing, exposing_ = T.CRRTL_All }) names)



-- FROM MODULE


fromModule : T.CASTS_Module -> T.CRRTL_Localizer
fromModule ((T.CASTS_Module _ _ _ imports _ _ _ _ _) as modul) =
    T.CRRTL_Localizer <|
        Dict.fromList identity <|
            (( Src.getName modul, { alias = Nothing, exposing_ = T.CRRTL_All } ) :: List.map toPair imports)


toPair : T.CASTS_Import -> ( T.CDN_Name, T.CRRTL_Import )
toPair (T.CASTS_Import (T.CRA_At _ name) alias_ exposing_) =
    ( name
    , T.CRRTL_Import alias_ (toExposing exposing_)
    )


toExposing : T.CASTS_Exposing -> T.CRRTL_Exposing
toExposing exposing_ =
    case exposing_ of
        T.CASTS_Open ->
            T.CRRTL_All

        T.CASTS_Explicit exposedList ->
            T.CRRTL_Only (List.foldr addType EverySet.empty exposedList)


addType : T.CASTS_Exposed -> EverySet String T.CDN_Name -> EverySet String T.CDN_Name
addType exposed types =
    case exposed of
        T.CASTS_Lower _ ->
            types

        T.CASTS_Upper (T.CRA_At _ name) _ ->
            EverySet.insert identity name types

        T.CASTS_Operator _ _ ->
            types



-- ENCODERS and DECODERS


localizerEncoder : T.CRRTL_Localizer -> Encode.Value
localizerEncoder (T.CRRTL_Localizer localizer) =
    EncodeX.assocListDict compare Encode.string importEncoder localizer


localizerDecoder : Decode.Decoder T.CRRTL_Localizer
localizerDecoder =
    Decode.map T.CRRTL_Localizer (DecodeX.assocListDict identity Decode.string importDecoder)


importEncoder : T.CRRTL_Import -> Encode.Value
importEncoder import_ =
    Encode.object
        [ ( "type", Encode.string "Import" )
        , ( "alias", EncodeX.maybe Encode.string import_.alias )
        , ( "exposing", exposingEncoder import_.exposing_ )
        ]


importDecoder : Decode.Decoder T.CRRTL_Import
importDecoder =
    Decode.map2 T.CRRTL_Import
        (Decode.field "alias" (Decode.maybe Decode.string))
        (Decode.field "exposing" exposingDecoder)


exposingEncoder : T.CRRTL_Exposing -> Encode.Value
exposingEncoder exposing_ =
    case exposing_ of
        T.CRRTL_All ->
            Encode.object
                [ ( "type", Encode.string "All" )
                ]

        T.CRRTL_Only set ->
            Encode.object
                [ ( "type", Encode.string "Only" )
                , ( "set", EncodeX.everySet compare Encode.string set )
                ]


exposingDecoder : Decode.Decoder T.CRRTL_Exposing
exposingDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "All" ->
                        Decode.succeed T.CRRTL_All

                    "Only" ->
                        Decode.map T.CRRTL_Only (Decode.field "set" (DecodeX.everySet identity Decode.string))

                    _ ->
                        Decode.fail ("Unknown Exposing's type: " ++ type_)
            )
