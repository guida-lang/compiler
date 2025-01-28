module Compiler.Optimize.Port exposing
    ( toDecoder
    , toEncoder
    , toFlagsDecoder
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Utils.Type as Type
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Optimize.Names as Names
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import Utils.Crash exposing (crash)



-- ENCODE


toEncoder : Can.Type -> Names.Tracker Opt.Expr
toEncoder tipe =
    case tipe of
        Can.TAlias _ _ args alias ->
            toEncoder (Type.dealias args alias)

        Can.TLambda _ _ ->
            crash "toEncoder: function"

        Can.TVar _ ->
            crash "toEncoder: type variable"

        Can.TUnit ->
            Names.fmap (Opt.Function [ Name.dollar ]) (encode "null")

        Can.TTuple a b c ->
            encodeTuple a b c

        Can.TType _ name args ->
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
                        Names.registerGlobal A.zero ModuleName.basics Name.identity_

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

        Can.TRecord _ (Just _) ->
            crash "toEncoder: bad record"

        Can.TRecord fields Nothing ->
            let
                encodeField : ( Name, Can.FieldType ) -> Names.Tracker Opt.Expr
                encodeField ( name, Can.FieldType _ fieldType ) =
                    toEncoder fieldType
                        |> Names.fmap
                            (\encoder ->
                                let
                                    value : Opt.Expr
                                    value =
                                        Opt.Call A.zero encoder [ Opt.Access (Opt.VarLocal Name.dollar) A.zero name ]
                                in
                                Opt.Tuple A.zero (Opt.Str A.zero (Name.toElmString name)) value Nothing
                            )
            in
            encode "object"
                |> Names.bind
                    (\object ->
                        Names.traverse encodeField (Dict.toList compare fields)
                            |> Names.bind
                                (\keyValuePairs ->
                                    Names.registerFieldDict fields
                                        (Opt.Function [ Name.dollar ] (Opt.Call A.zero object [ Opt.List A.zero keyValuePairs ]))
                                )
                    )



-- ENCODE HELPERS


encodeMaybe : Can.Type -> Names.Tracker Opt.Expr
encodeMaybe tipe =
    encode "null"
        |> Names.bind
            (\null ->
                toEncoder tipe
                    |> Names.bind
                        (\encoder ->
                            Names.registerGlobal A.zero ModuleName.maybe "destruct"
                                |> Names.fmap
                                    (\destruct ->
                                        Opt.Function [ Name.dollar ]
                                            (Opt.Call A.zero
                                                destruct
                                                [ null
                                                , encoder
                                                , Opt.VarLocal Name.dollar
                                                ]
                                            )
                                    )
                        )
            )


encodeList : Can.Type -> Names.Tracker Opt.Expr
encodeList tipe =
    encode "list"
        |> Names.bind
            (\list ->
                toEncoder tipe
                    |> Names.fmap (Opt.Call A.zero list << List.singleton)
            )


encodeArray : Can.Type -> Names.Tracker Opt.Expr
encodeArray tipe =
    encode "array"
        |> Names.bind
            (\array ->
                toEncoder tipe
                    |> Names.fmap (Opt.Call A.zero array << List.singleton)
            )


encodeTuple : Can.Type -> Can.Type -> Maybe Can.Type -> Names.Tracker Opt.Expr
encodeTuple a b maybeC =
    let
        let_ : Name -> Index.ZeroBased -> Opt.Expr -> Opt.Expr
        let_ arg index body =
            Opt.Destruct (Opt.Destructor arg (Opt.Index index (Opt.Root Name.dollar))) body

        encodeArg : Name -> Can.Type -> Names.Tracker Opt.Expr
        encodeArg arg tipe =
            toEncoder tipe
                |> Names.fmap (\encoder -> Opt.Call A.zero encoder [ Opt.VarLocal arg ])
    in
    encode "list"
        |> Names.bind
            (\list ->
                Names.registerGlobal A.zero ModuleName.basics Name.identity_
                    |> Names.bind
                        (\identity ->
                            Names.bind
                                (\arg1 ->
                                    Names.bind
                                        (\arg2 ->
                                            case maybeC of
                                                Nothing ->
                                                    Names.pure
                                                        (Opt.Function [ Name.dollar ]
                                                            (let_ "a"
                                                                Index.first
                                                                (let_ "b"
                                                                    Index.second
                                                                    (Opt.Call A.zero
                                                                        list
                                                                        [ identity
                                                                        , Opt.List A.zero [ arg1, arg2 ]
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        )

                                                Just c ->
                                                    Names.fmap
                                                        (\arg3 ->
                                                            Opt.Function [ Name.dollar ]
                                                                (let_ "a"
                                                                    Index.first
                                                                    (let_ "b"
                                                                        Index.second
                                                                        (let_ "c"
                                                                            Index.third
                                                                            (Opt.Call A.zero
                                                                                list
                                                                                [ identity
                                                                                , Opt.List A.zero [ arg1, arg2, arg3 ]
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


toFlagsDecoder : Can.Type -> Names.Tracker Opt.Expr
toFlagsDecoder tipe =
    case tipe of
        Can.TUnit ->
            Names.fmap (\succeed -> Opt.Call A.zero succeed [ Opt.Unit ])
                (decode "succeed")

        _ ->
            toDecoder tipe



-- DECODE


toDecoder : Can.Type -> Names.Tracker Opt.Expr
toDecoder tipe =
    case tipe of
        Can.TLambda _ _ ->
            crash "functions should not be allowed through input ports"

        Can.TVar _ ->
            crash "type variables should not be allowed through input ports"

        Can.TAlias _ _ args alias ->
            toDecoder (Type.dealias args alias)

        Can.TUnit ->
            decodeTuple0

        Can.TTuple a b c ->
            decodeTuple a b c

        Can.TType _ name args ->
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

        Can.TRecord _ (Just _) ->
            crash "toDecoder: bad record"

        Can.TRecord fields Nothing ->
            decodeRecord fields



-- DECODE MAYBE


decodeMaybe : Can.Type -> Names.Tracker Opt.Expr
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
                                                    Opt.Call A.zero
                                                        oneOf
                                                        [ Opt.List A.zero
                                                            [ Opt.Call A.zero null [ nothing ]
                                                            , Opt.Call A.zero map_ [ just, subDecoder ]
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
                (Names.registerGlobal A.zero ModuleName.maybe "Just")
        )
        (Names.registerGlobal A.zero ModuleName.maybe "Nothing")



-- DECODE LIST


decodeList : Can.Type -> Names.Tracker Opt.Expr
decodeList tipe =
    Names.bind
        (\list ->
            Names.fmap (Opt.Call A.zero list << List.singleton)
                (toDecoder tipe)
        )
        (decode "list")



-- DECODE ARRAY


decodeArray : Can.Type -> Names.Tracker Opt.Expr
decodeArray tipe =
    Names.bind
        (\array ->
            Names.fmap (Opt.Call A.zero array << List.singleton)
                (toDecoder tipe)
        )
        (decode "array")



-- DECODE TUPLES


decodeTuple0 : Names.Tracker Opt.Expr
decodeTuple0 =
    Names.fmap (\null -> Opt.Call A.zero null [ Opt.Unit ])
        (decode "null")


decodeTuple : Can.Type -> Can.Type -> Maybe Can.Type -> Names.Tracker Opt.Expr
decodeTuple a b maybeC =
    Names.bind
        (\succeed ->
            case maybeC of
                Nothing ->
                    let
                        tuple : Opt.Expr
                        tuple =
                            Opt.Tuple A.zero (toLocal 0) (toLocal 1) Nothing
                    in
                    indexAndThen 1 b (Opt.Call A.zero succeed [ tuple ])
                        |> Names.bind (indexAndThen 0 a)

                Just c ->
                    let
                        tuple : Opt.Expr
                        tuple =
                            Opt.Tuple A.zero (toLocal 0) (toLocal 1) (Just (toLocal 2))
                    in
                    indexAndThen 2 c (Opt.Call A.zero succeed [ tuple ])
                        |> Names.bind (indexAndThen 1 b)
                        |> Names.bind (indexAndThen 0 a)
        )
        (decode "succeed")


toLocal : Int -> Opt.Expr
toLocal index =
    Opt.VarLocal (Name.fromVarIndex index)


indexAndThen : Int -> Can.Type -> Opt.Expr -> Names.Tracker Opt.Expr
indexAndThen i tipe decoder =
    Names.bind
        (\andThen ->
            Names.bind
                (\index ->
                    Names.fmap
                        (\typeDecoder ->
                            Opt.Call A.zero
                                andThen
                                [ Opt.Function [ Name.fromVarIndex i ] decoder
                                , Opt.Call A.zero index [ Opt.Int A.zero i, typeDecoder ]
                                ]
                        )
                        (toDecoder tipe)
                )
                (decode "index")
        )
        (decode "andThen")



-- DECODE RECORDS


decodeRecord : Dict String Name.Name Can.FieldType -> Names.Tracker Opt.Expr
decodeRecord fields =
    let
        toFieldExpr : Name -> b -> Opt.Expr
        toFieldExpr name _ =
            Opt.VarLocal name

        record : Opt.Expr
        record =
            Opt.Record (Dict.map toFieldExpr fields)
    in
    Names.bind
        (\succeed ->
            Names.registerFieldDict fields (Dict.toList compare fields)
                |> Names.bind
                    (\fieldDecoders ->
                        List.foldl (\fieldDecoder -> Names.bind (\optCall -> fieldAndThen optCall fieldDecoder))
                            (Names.pure (Opt.Call A.zero succeed [ record ]))
                            fieldDecoders
                    )
        )
        (decode "succeed")


fieldAndThen : Opt.Expr -> ( Name.Name, Can.FieldType ) -> Names.Tracker Opt.Expr
fieldAndThen decoder ( key, Can.FieldType _ tipe ) =
    Names.bind
        (\andThen ->
            Names.bind
                (\field ->
                    Names.fmap
                        (\typeDecoder ->
                            Opt.Call A.zero
                                andThen
                                [ Opt.Function [ key ] decoder
                                , Opt.Call A.zero field [ Opt.Str A.zero (Name.toElmString key), typeDecoder ]
                                ]
                        )
                        (toDecoder tipe)
                )
                (decode "field")
        )
        (decode "andThen")



-- GLOBALS HELPERS


encode : Name -> Names.Tracker Opt.Expr
encode name =
    Names.registerGlobal A.zero ModuleName.jsonEncode name


decode : Name -> Names.Tracker Opt.Expr
decode name =
    Names.registerGlobal A.zero ModuleName.jsonDecode name
