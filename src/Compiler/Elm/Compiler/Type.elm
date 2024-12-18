module Compiler.Elm.Compiler.Type exposing
    ( Alias(..)
    , CECT_Type(..)
    , DebugMetadata(..)
    , Union(..)
    , decoder
    , encode
    , encodeMetadata
    , jsonDecoder
    , jsonEncoder
    , toDoc
    )

import Compiler.Json.Decode as D exposing (Decoder)
import Compiler.Json.Encode as E exposing (Value)
import Compiler.Json.String as Json
import Compiler.Parse.Primitives as P
import Compiler.Parse.Type as Type
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T
import Utils.Crash exposing (crash)



-- TYPES


type CECT_Type
    = CECT_Lambda CECT_Type CECT_Type
    | CECT_Var T.CDN_Name
    | CECT_Type T.CDN_Name (List CECT_Type)
    | CECT_Record (List ( T.CDN_Name, CECT_Type )) (Maybe T.CDN_Name)
    | CECT_Unit
    | CECT_Tuple CECT_Type CECT_Type (List CECT_Type)


type DebugMetadata
    = DebugMetadata CECT_Type (List Alias) (List Union)


type Alias
    = Alias T.CDN_Name (List T.CDN_Name) CECT_Type


type Union
    = Union T.CDN_Name (List T.CDN_Name) (List ( T.CDN_Name, List CECT_Type ))



-- TO DOC


toDoc : L.CRRTL_Localizer -> RT.Context -> CECT_Type -> D.Doc
toDoc localizer context tipe =
    case tipe of
        CECT_Lambda _ _ ->
            case List.map (toDoc localizer RT.Func) (collectLambdas tipe) of
                a :: b :: cs ->
                    RT.lambda context a b cs

                _ ->
                    crash "toDoc Lambda"

        CECT_Var name ->
            D.fromName name

        CECT_Unit ->
            D.fromChars "()"

        CECT_Tuple a b cs ->
            RT.tuple
                (toDoc localizer RT.None a)
                (toDoc localizer RT.None b)
                (List.map (toDoc localizer RT.None) cs)

        CECT_Type name args ->
            RT.apply
                context
                (D.fromName name)
                (List.map (toDoc localizer RT.App) args)

        CECT_Record fields ext ->
            RT.record
                (List.map (entryToDoc localizer) fields)
                (Maybe.map D.fromName ext)


entryToDoc : L.CRRTL_Localizer -> ( T.CDN_Name, CECT_Type ) -> ( D.Doc, D.Doc )
entryToDoc localizer ( field, fieldType ) =
    ( D.fromName field, toDoc localizer RT.None fieldType )


collectLambdas : CECT_Type -> List CECT_Type
collectLambdas tipe =
    case tipe of
        CECT_Lambda arg body ->
            arg :: collectLambdas body

        _ ->
            [ tipe ]



-- JSON for TYPE


encode : CECT_Type -> Value
encode tipe =
    E.string (D.toLine (toDoc L.empty RT.None tipe))


decoder : Decoder () CECT_Type
decoder =
    D.customString parser (\_ _ -> ())


parser : P.Parser () CECT_Type
parser =
    P.specialize (\_ _ _ -> ()) (P.fmap fromRawType (P.fmap Tuple.first Type.expression))


fromRawType : T.CASTS_Type -> CECT_Type
fromRawType (T.CRA_At _ astType) =
    case astType of
        T.CASTS_TLambda t1 t2 ->
            CECT_Lambda (fromRawType t1) (fromRawType t2)

        T.CASTS_TVar x ->
            CECT_Var x

        T.CASTS_TUnit ->
            CECT_Unit

        T.CASTS_TTuple a b cs ->
            CECT_Tuple
                (fromRawType a)
                (fromRawType b)
                (List.map fromRawType cs)

        T.CASTS_TType _ name args ->
            CECT_Type name (List.map fromRawType args)

        T.CASTS_TTypeQual _ _ name args ->
            CECT_Type name (List.map fromRawType args)

        T.CASTS_TRecord fields ext ->
            let
                fromField : ( T.CRA_Located a, T.CASTS_Type ) -> ( a, CECT_Type )
                fromField ( T.CRA_At _ field, tipe ) =
                    ( field, fromRawType tipe )
            in
            CECT_Record
                (List.map fromField fields)
                (Maybe.map A.toValue ext)



-- JSON for PROGRAM


encodeMetadata : DebugMetadata -> Value
encodeMetadata (DebugMetadata msg aliases unions) =
    E.object
        [ ( "message", encode msg )
        , ( "aliases", E.object (List.map toTypeAliasField aliases) )
        , ( "unions", E.object (List.map toCustomTypeField unions) )
        ]


toTypeAliasField : Alias -> ( String, Value )
toTypeAliasField (Alias name args tipe) =
    ( Json.fromName name
    , E.object
        [ ( "args", E.list E.string args )
        , ( "type", encode tipe )
        ]
    )


toCustomTypeField : Union -> ( String, Value )
toCustomTypeField (Union name args constructors) =
    ( Json.fromName name
    , E.object
        [ ( "args", E.list E.string args )
        , ( "tags", E.object (List.map toVariantObject constructors) )
        ]
    )


toVariantObject : ( T.CDN_Name, List CECT_Type ) -> ( String, Value )
toVariantObject ( name, args ) =
    ( Json.fromName name, E.list encode args )



-- ENCODERS and DECODERS


jsonEncoder : CECT_Type -> Encode.Value
jsonEncoder type_ =
    case type_ of
        CECT_Lambda arg body ->
            Encode.object
                [ ( "type", Encode.string "Lambda" )
                , ( "arg", jsonEncoder arg )
                , ( "body", jsonEncoder body )
                ]

        CECT_Var name ->
            Encode.object
                [ ( "type", Encode.string "Var" )
                , ( "name", Encode.string name )
                ]

        CECT_Type name args ->
            Encode.object
                [ ( "type", Encode.string "Type" )
                , ( "name", Encode.string name )
                , ( "args", Encode.list jsonEncoder args )
                ]

        CECT_Record fields ext ->
            Encode.object
                [ ( "type", Encode.string "Record" )
                , ( "fields", Encode.list (E.jsonPair Encode.string jsonEncoder) fields )
                , ( "ext", E.maybe Encode.string ext )
                ]

        CECT_Unit ->
            Encode.object
                [ ( "type", Encode.string "Unit" )
                ]

        CECT_Tuple a b cs ->
            Encode.object
                [ ( "type", Encode.string "Tuple" )
                , ( "a", jsonEncoder a )
                , ( "b", jsonEncoder b )
                , ( "cs", Encode.list jsonEncoder cs )
                ]


jsonDecoder : Decode.Decoder CECT_Type
jsonDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Lambda" ->
                        Decode.map2 CECT_Lambda
                            (Decode.field "arg" jsonDecoder)
                            (Decode.field "body" jsonDecoder)

                    "Var" ->
                        Decode.map CECT_Var
                            (Decode.field "name" Decode.string)

                    "Type" ->
                        Decode.map2 CECT_Type
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list jsonDecoder))

                    "Record" ->
                        Decode.map2 CECT_Record
                            (Decode.field "fields" (Decode.list (D.jsonPair Decode.string jsonDecoder)))
                            (Decode.field "ext" (Decode.maybe Decode.string))

                    "Unit" ->
                        Decode.succeed CECT_Unit

                    "Tuple" ->
                        Decode.map3 CECT_Tuple
                            (Decode.field "a" jsonDecoder)
                            (Decode.field "b" jsonDecoder)
                            (Decode.field "cs" (Decode.list jsonDecoder))

                    _ ->
                        Decode.fail ("Failed to decode Type's type: " ++ type_)
            )
