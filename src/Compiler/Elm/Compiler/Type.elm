module Compiler.Elm.Compiler.Type exposing
    ( Alias(..)
    , DebugMetadata(..)
    , Type(..)
    , Union(..)
    , codec
    , decoder
    , encode
    , encodeMetadata
    , toDoc
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Json.Decode as D exposing (Decoder)
import Compiler.Json.Encode as E exposing (Value)
import Compiler.Json.String as Json
import Compiler.Parse.Primitives as P
import Compiler.Parse.Type as Type
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Serialize exposing (Codec)
import Utils.Crash exposing (crash)



-- TYPES


type Type
    = Lambda Type Type
    | Var Name.Name
    | Type Name.Name (List Type)
    | Record (List ( Name.Name, Type )) (Maybe Name.Name)
    | Unit
    | Tuple Type Type (List Type)


type DebugMetadata
    = DebugMetadata Type (List Alias) (List Union)


type Alias
    = Alias Name.Name (List Name.Name) Type


type Union
    = Union Name.Name (List Name.Name) (List ( Name.Name, List Type ))



-- TO DOC


toDoc : L.Localizer -> RT.Context -> Type -> D.Doc
toDoc localizer context tipe =
    case tipe of
        Lambda _ _ ->
            case List.map (toDoc localizer RT.Func) (collectLambdas tipe) of
                a :: b :: cs ->
                    RT.lambda context a b cs

                _ ->
                    crash "toDoc Lambda"

        Var name ->
            D.fromName name

        Unit ->
            D.fromChars "()"

        Tuple a b cs ->
            RT.tuple
                (toDoc localizer RT.None a)
                (toDoc localizer RT.None b)
                (List.map (toDoc localizer RT.None) cs)

        Type name args ->
            RT.apply
                context
                (D.fromName name)
                (List.map (toDoc localizer RT.App) args)

        Record fields ext ->
            RT.record
                (List.map (entryToDoc localizer) fields)
                (Maybe.map D.fromName ext)


entryToDoc : L.Localizer -> ( Name.Name, Type ) -> ( D.Doc, D.Doc )
entryToDoc localizer ( field, fieldType ) =
    ( D.fromName field, toDoc localizer RT.None fieldType )


collectLambdas : Type -> List Type
collectLambdas tipe =
    case tipe of
        Lambda arg body ->
            arg :: collectLambdas body

        _ ->
            [ tipe ]



-- JSON for TYPE


encode : Type -> Value
encode tipe =
    E.string (D.toLine (toDoc L.empty RT.None tipe))


decoder : Decoder () Type
decoder =
    D.customString parser (\_ _ -> ())


parser : P.Parser () Type
parser =
    P.specialize (\_ _ _ -> ()) (P.fmap fromRawType (P.fmap Tuple.first Type.expression))


fromRawType : Src.Type -> Type
fromRawType (A.At _ astType) =
    case astType of
        Src.TLambda t1 t2 ->
            Lambda (fromRawType t1) (fromRawType t2)

        Src.TVar x ->
            Var x

        Src.TUnit ->
            Unit

        Src.TTuple a b cs ->
            Tuple
                (fromRawType a)
                (fromRawType b)
                (List.map fromRawType cs)

        Src.TType _ name args ->
            Type name (List.map fromRawType args)

        Src.TTypeQual _ _ name args ->
            Type name (List.map fromRawType args)

        Src.TRecord fields ext ->
            let
                fromField : ( A.Located a, Src.Type ) -> ( a, Type )
                fromField ( A.At _ field, tipe ) =
                    ( field, fromRawType tipe )
            in
            Record
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


toVariantObject : ( Name.Name, List Type ) -> ( String, Value )
toVariantObject ( name, args ) =
    ( Json.fromName name, E.list encode args )



-- ENCODERS and DECODERS


codec : Codec e Type
codec =
    Serialize.customType
        (\lambdaEncoder varEncoder typeEncoder recordEncoder unitEncoder tupleEncoder value ->
            case value of
                Lambda arg body ->
                    lambdaEncoder arg body

                Var name ->
                    varEncoder name

                Type name args ->
                    typeEncoder name args

                Record fields ext ->
                    recordEncoder fields ext

                Unit ->
                    unitEncoder

                Tuple a b cs ->
                    tupleEncoder a b cs
        )
        |> Serialize.variant2 Lambda (Serialize.lazy (\() -> codec)) (Serialize.lazy (\() -> codec))
        |> Serialize.variant1 Var Serialize.string
        |> Serialize.variant2 Type Serialize.string (Serialize.list (Serialize.lazy (\() -> codec)))
        |> Serialize.variant2
            Record
            (Serialize.list (Serialize.tuple Serialize.string (Serialize.lazy (\() -> codec))))
            (Serialize.maybe Serialize.string)
        |> Serialize.variant0 Unit
        |> Serialize.variant3
            Tuple
            (Serialize.lazy (\() -> codec))
            (Serialize.lazy (\() -> codec))
            (Serialize.list (Serialize.lazy (\() -> codec)))
        |> Serialize.finishCustomType
