module Compiler.Reporting.Render.Type.Localizer exposing
    ( CRRTL_Localizer
    , empty
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


type CRRTL_Localizer
    = CRRTL_Localizer (Dict String T.CDN_Name CRRTL_Import)


type alias CRRTL_Import =
    { alias : Maybe T.CDN_Name
    , exposing_ : CRRTL_Exposing
    }


type CRRTL_Exposing
    = CRRTL_All
    | CRRTL_Only (EverySet String T.CDN_Name)


empty : CRRTL_Localizer
empty =
    CRRTL_Localizer Dict.empty



-- LOCALIZE


toDoc : CRRTL_Localizer -> T.CEMN_Canonical -> T.CDN_Name -> D.Doc
toDoc localizer home name =
    D.fromChars (toChars localizer home name)


toChars : CRRTL_Localizer -> T.CEMN_Canonical -> T.CDN_Name -> String
toChars (CRRTL_Localizer localizer) ((T.CEMN_Canonical _ home) as moduleName) name =
    case Dict.get identity home localizer of
        Nothing ->
            home ++ "." ++ name

        Just import_ ->
            case import_.exposing_ of
                CRRTL_All ->
                    name

                CRRTL_Only set ->
                    if EverySet.member identity name set then
                        name

                    else if name == Name.list && moduleName == ModuleName.list then
                        "List"

                    else
                        Maybe.withDefault home import_.alias ++ "." ++ name



-- FROM NAMES


fromNames : Dict String T.CDN_Name a -> CRRTL_Localizer
fromNames names =
    CRRTL_Localizer (Dict.map (\_ _ -> { alias = Nothing, exposing_ = CRRTL_All }) names)



-- FROM MODULE


fromModule : T.CASTS_Module -> CRRTL_Localizer
fromModule ((T.CASTS_Module _ _ _ imports _ _ _ _ _) as modul) =
    CRRTL_Localizer <|
        Dict.fromList identity <|
            (( Src.getName modul, { alias = Nothing, exposing_ = CRRTL_All } ) :: List.map toPair imports)


toPair : T.CASTS_Import -> ( T.CDN_Name, CRRTL_Import )
toPair (T.CASTS_Import (T.CRA_At _ name) alias_ exposing_) =
    ( name
    , CRRTL_Import alias_ (toExposing exposing_)
    )


toExposing : T.CASTS_Exposing -> CRRTL_Exposing
toExposing exposing_ =
    case exposing_ of
        T.CASTS_Open ->
            CRRTL_All

        T.CASTS_Explicit exposedList ->
            CRRTL_Only (List.foldr addType EverySet.empty exposedList)


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


localizerEncoder : CRRTL_Localizer -> Encode.Value
localizerEncoder (CRRTL_Localizer localizer) =
    EncodeX.assocListDict compare Encode.string importEncoder localizer


localizerDecoder : Decode.Decoder CRRTL_Localizer
localizerDecoder =
    Decode.map CRRTL_Localizer (DecodeX.assocListDict identity Decode.string importDecoder)


importEncoder : CRRTL_Import -> Encode.Value
importEncoder import_ =
    Encode.object
        [ ( "type", Encode.string "Import" )
        , ( "alias", EncodeX.maybe Encode.string import_.alias )
        , ( "exposing", exposingEncoder import_.exposing_ )
        ]


importDecoder : Decode.Decoder CRRTL_Import
importDecoder =
    Decode.map2 CRRTL_Import
        (Decode.field "alias" (Decode.maybe Decode.string))
        (Decode.field "exposing" exposingDecoder)


exposingEncoder : CRRTL_Exposing -> Encode.Value
exposingEncoder exposing_ =
    case exposing_ of
        CRRTL_All ->
            Encode.object
                [ ( "type", Encode.string "All" )
                ]

        CRRTL_Only set ->
            Encode.object
                [ ( "type", Encode.string "Only" )
                , ( "set", EncodeX.everySet compare Encode.string set )
                ]


exposingDecoder : Decode.Decoder CRRTL_Exposing
exposingDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "All" ->
                        Decode.succeed CRRTL_All

                    "Only" ->
                        Decode.map CRRTL_Only (Decode.field "set" (DecodeX.everySet identity Decode.string))

                    _ ->
                        Decode.fail ("Unknown Exposing's type: " ++ type_)
            )
