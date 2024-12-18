module Compiler.Optimize.Port exposing
    ( toDecoder
    , toEncoder
    , toFlagsDecoder
    )

import Compiler.AST.Utils.Type as Type
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Optimize.Names as Names
import Data.Map as Dict exposing (Dict)
import Types as T
import Utils.Crash exposing (crash)



-- ENCODE


toEncoder : T.CASTC_Type -> Names.Tracker T.CASTO_Expr
toEncoder tipe =
    case tipe of
        T.CASTC_TAlias _ _ args alias ->
            toEncoder (Type.dealias args alias)

        T.CASTC_TLambda _ _ ->
            crash "toEncoder: function"

        T.CASTC_TVar _ ->
            crash "toEncoder: type variable"

        T.CASTC_TUnit ->
            Names.fmap (T.CASTO_Function [ Name.dollar ]) (encode "null")

        T.CASTC_TTuple a b c ->
            encodeTuple a b c

        T.CASTC_TType _ name args ->
            case args of
                [] ->
                    if name == Name.float then
                        encode "float"

                    else if name == Name.int then
                        encode "int"

                    else if name == Name.bool then
                        encode "bool"

                    else if name == Name.string then
                        encode "string"

                    else if name == Name.value then
                        Names.registerGlobal ModuleName.basics Name.identity_

                    else
                        crash "toEncoder: bad custom type"

                [ arg ] ->
                    if name == Name.maybe then
                        encodeMaybe arg

                    else if name == Name.list then
                        encodeList arg

                    else if name == Name.array then
                        encodeArray arg

                    else
                        crash "toEncoder: bad custom type"

                _ ->
                    crash "toEncoder: bad custom type"

        T.CASTC_TRecord _ (Just _) ->
            crash "toEncoder: bad record"

        T.CASTC_TRecord fields Nothing ->
            let
                encodeField : ( T.CDN_Name, T.CASTC_FieldType ) -> Names.Tracker T.CASTO_Expr
                encodeField ( name, T.CASTC_FieldType _ fieldType ) =
                    toEncoder fieldType
                        |> Names.fmap
                            (\encoder ->
                                let
                                    value : T.CASTO_Expr
                                    value =
                                        T.CASTO_Call encoder [ T.CASTO_Access (T.CASTO_VarLocal Name.dollar) name ]
                                in
                                T.CASTO_Tuple (T.CASTO_Str (Name.toElmString name)) value Nothing
                            )
            in
            encode "object"
                |> Names.bind
                    (\object ->
                        Names.traverse encodeField (Dict.toList compare fields)
                            |> Names.bind
                                (\keyValuePairs ->
                                    Names.registerFieldDict fields
                                        (T.CASTO_Function [ Name.dollar ] (T.CASTO_Call object [ T.CASTO_List keyValuePairs ]))
                                )
                    )



-- ENCODE HELPERS


encodeMaybe : T.CASTC_Type -> Names.Tracker T.CASTO_Expr
encodeMaybe tipe =
    encode "null"
        |> Names.bind
            (\null ->
                toEncoder tipe
                    |> Names.bind
                        (\encoder ->
                            Names.registerGlobal ModuleName.maybe "destruct"
                                |> Names.fmap
                                    (\destruct ->
                                        T.CASTO_Function [ Name.dollar ]
                                            (T.CASTO_Call destruct
                                                [ null
                                                , encoder
                                                , T.CASTO_VarLocal Name.dollar
                                                ]
                                            )
                                    )
                        )
            )


encodeList : T.CASTC_Type -> Names.Tracker T.CASTO_Expr
encodeList tipe =
    encode "list"
        |> Names.bind
            (\list ->
                toEncoder tipe
                    |> Names.fmap (T.CASTO_Call list << List.singleton)
            )


encodeArray : T.CASTC_Type -> Names.Tracker T.CASTO_Expr
encodeArray tipe =
    encode "array"
        |> Names.bind
            (\array ->
                toEncoder tipe
                    |> Names.fmap (T.CASTO_Call array << List.singleton)
            )


encodeTuple : T.CASTC_Type -> T.CASTC_Type -> Maybe T.CASTC_Type -> Names.Tracker T.CASTO_Expr
encodeTuple a b maybeC =
    let
        let_ : T.CDN_Name -> T.CDI_ZeroBased -> T.CASTO_Expr -> T.CASTO_Expr
        let_ arg index body =
            T.CASTO_Destruct (T.CASTO_Destructor arg (T.CASTO_Index index (T.CASTO_Root Name.dollar))) body

        encodeArg : T.CDN_Name -> T.CASTC_Type -> Names.Tracker T.CASTO_Expr
        encodeArg arg tipe =
            toEncoder tipe
                |> Names.fmap (\encoder -> T.CASTO_Call encoder [ T.CASTO_VarLocal arg ])
    in
    encode "list"
        |> Names.bind
            (\list ->
                Names.registerGlobal ModuleName.basics Name.identity_
                    |> Names.bind
                        (\identity ->
                            Names.bind
                                (\arg1 ->
                                    Names.bind
                                        (\arg2 ->
                                            case maybeC of
                                                Nothing ->
                                                    Names.pure
                                                        (T.CASTO_Function [ Name.dollar ]
                                                            (let_ "a"
                                                                Index.first
                                                                (let_ "b"
                                                                    Index.second
                                                                    (T.CASTO_Call list
                                                                        [ identity
                                                                        , T.CASTO_List [ arg1, arg2 ]
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        )

                                                Just c ->
                                                    Names.fmap
                                                        (\arg3 ->
                                                            T.CASTO_Function [ Name.dollar ]
                                                                (let_ "a"
                                                                    Index.first
                                                                    (let_ "b"
                                                                        Index.second
                                                                        (let_ "c"
                                                                            Index.third
                                                                            (T.CASTO_Call list
                                                                                [ identity
                                                                                , T.CASTO_List [ arg1, arg2, arg3 ]
                                                                                ]
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                        )
                                                        (encodeArg "c" c)
                                        )
                                        (encodeArg "b" b)
                                )
                                (encodeArg "a" a)
                        )
            )



-- FLAGS DECODER


toFlagsDecoder : T.CASTC_Type -> Names.Tracker T.CASTO_Expr
toFlagsDecoder tipe =
    case tipe of
        T.CASTC_TUnit ->
            Names.fmap (\succeed -> T.CASTO_Call succeed [ T.CASTO_Unit ])
                (decode "succeed")

        _ ->
            toDecoder tipe



-- DECODE


toDecoder : T.CASTC_Type -> Names.Tracker T.CASTO_Expr
toDecoder tipe =
    case tipe of
        T.CASTC_TLambda _ _ ->
            crash "functions should not be allowed through input ports"

        T.CASTC_TVar _ ->
            crash "type variables should not be allowed through input ports"

        T.CASTC_TAlias _ _ args alias ->
            toDecoder (Type.dealias args alias)

        T.CASTC_TUnit ->
            decodeTuple0

        T.CASTC_TTuple a b c ->
            decodeTuple a b c

        T.CASTC_TType _ name args ->
            case ( name, args ) of
                ( "Float", [] ) ->
                    decode "float"

                ( "Int", [] ) ->
                    decode "int"

                ( "Bool", [] ) ->
                    decode "bool"

                ( "String", [] ) ->
                    decode "string"

                ( "Value", [] ) ->
                    decode "value"

                ( "Maybe", [ arg ] ) ->
                    decodeMaybe arg

                ( "List", [ arg ] ) ->
                    decodeList arg

                ( "Array", [ arg ] ) ->
                    decodeArray arg

                _ ->
                    crash "toDecoder: bad type"

        T.CASTC_TRecord _ (Just _) ->
            crash "toDecoder: bad record"

        T.CASTC_TRecord fields Nothing ->
            decodeRecord fields



-- DECODE MAYBE


decodeMaybe : T.CASTC_Type -> Names.Tracker T.CASTO_Expr
decodeMaybe tipe =
    Names.bind
        (\nothing ->
            Names.bind
                (\just ->
                    Names.bind
                        (\oneOf ->
                            Names.bind
                                (\null ->
                                    Names.bind
                                        (\map_ ->
                                            Names.fmap
                                                (\subDecoder ->
                                                    T.CASTO_Call oneOf
                                                        [ T.CASTO_List
                                                            [ T.CASTO_Call null [ nothing ]
                                                            , T.CASTO_Call map_ [ just, subDecoder ]
                                                            ]
                                                        ]
                                                )
                                                (toDecoder tipe)
                                        )
                                        (decode "map")
                                )
                                (decode "null")
                        )
                        (decode "oneOf")
                )
                (Names.registerGlobal ModuleName.maybe "Just")
        )
        (Names.registerGlobal ModuleName.maybe "Nothing")



-- DECODE LIST


decodeList : T.CASTC_Type -> Names.Tracker T.CASTO_Expr
decodeList tipe =
    Names.bind
        (\list ->
            Names.fmap (T.CASTO_Call list << List.singleton)
                (toDecoder tipe)
        )
        (decode "list")



-- DECODE ARRAY


decodeArray : T.CASTC_Type -> Names.Tracker T.CASTO_Expr
decodeArray tipe =
    Names.bind
        (\array ->
            Names.fmap (T.CASTO_Call array << List.singleton)
                (toDecoder tipe)
        )
        (decode "array")



-- DECODE TUPLES


decodeTuple0 : Names.Tracker T.CASTO_Expr
decodeTuple0 =
    Names.fmap (\null -> T.CASTO_Call null [ T.CASTO_Unit ])
        (decode "null")


decodeTuple : T.CASTC_Type -> T.CASTC_Type -> Maybe T.CASTC_Type -> Names.Tracker T.CASTO_Expr
decodeTuple a b maybeC =
    Names.bind
        (\succeed ->
            case maybeC of
                Nothing ->
                    let
                        tuple : T.CASTO_Expr
                        tuple =
                            T.CASTO_Tuple (toLocal 0) (toLocal 1) Nothing
                    in
                    indexAndThen 1 b (T.CASTO_Call succeed [ tuple ])
                        |> Names.bind (indexAndThen 0 a)

                Just c ->
                    let
                        tuple : T.CASTO_Expr
                        tuple =
                            T.CASTO_Tuple (toLocal 0) (toLocal 1) (Just (toLocal 2))
                    in
                    indexAndThen 2 c (T.CASTO_Call succeed [ tuple ])
                        |> Names.bind (indexAndThen 1 b)
                        |> Names.bind (indexAndThen 0 a)
        )
        (decode "succeed")


toLocal : Int -> T.CASTO_Expr
toLocal index =
    T.CASTO_VarLocal (Name.fromVarIndex index)


indexAndThen : Int -> T.CASTC_Type -> T.CASTO_Expr -> Names.Tracker T.CASTO_Expr
indexAndThen i tipe decoder =
    Names.bind
        (\andThen ->
            Names.bind
                (\index ->
                    Names.fmap
                        (\typeDecoder ->
                            T.CASTO_Call andThen
                                [ T.CASTO_Function [ Name.fromVarIndex i ] decoder
                                , T.CASTO_Call index [ T.CASTO_Int i, typeDecoder ]
                                ]
                        )
                        (toDecoder tipe)
                )
                (decode "index")
        )
        (decode "andThen")



-- DECODE RECORDS


decodeRecord : Dict String T.CDN_Name T.CASTC_FieldType -> Names.Tracker T.CASTO_Expr
decodeRecord fields =
    let
        toFieldExpr : T.CDN_Name -> b -> T.CASTO_Expr
        toFieldExpr name _ =
            T.CASTO_VarLocal name

        record : T.CASTO_Expr
        record =
            T.CASTO_Record (Dict.map toFieldExpr fields)
    in
    Names.bind
        (\succeed ->
            Names.registerFieldDict fields (Dict.toList compare fields)
                |> Names.bind
                    (\fieldDecoders ->
                        List.foldl (\fieldDecoder -> Names.bind (\optCall -> fieldAndThen optCall fieldDecoder))
                            (Names.pure (T.CASTO_Call succeed [ record ]))
                            fieldDecoders
                    )
        )
        (decode "succeed")


fieldAndThen : T.CASTO_Expr -> ( T.CDN_Name, T.CASTC_FieldType ) -> Names.Tracker T.CASTO_Expr
fieldAndThen decoder ( key, T.CASTC_FieldType _ tipe ) =
    Names.bind
        (\andThen ->
            Names.bind
                (\field ->
                    Names.fmap
                        (\typeDecoder ->
                            T.CASTO_Call andThen
                                [ T.CASTO_Function [ key ] decoder
                                , T.CASTO_Call field [ T.CASTO_Str (Name.toElmString key), typeDecoder ]
                                ]
                        )
                        (toDecoder tipe)
                )
                (decode "field")
        )
        (decode "andThen")



-- GLOBALS HELPERS


encode : T.CDN_Name -> Names.Tracker T.CASTO_Expr
encode name =
    Names.registerGlobal ModuleName.jsonEncode name


decode : T.CDN_Name -> Names.Tracker T.CASTO_Expr
decode name =
    Names.registerGlobal ModuleName.jsonDecode name
