module Compiler.Optimize.Port exposing
    ( toDecoder
    , toEncoder
    , toFlagsDecoder
    )

import Basics.Extra exposing (flip)
import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Utils.Type as Type
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Generate.JavaScript.Name as JsName
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Optimize.Names as Names
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import Utils.Crash exposing (crash)



-- ENCODE


toEncoder : Target -> Can.Type -> Names.Tracker Opt.Expr
toEncoder target tipe =
    case tipe of
        Can.TAlias _ _ args alias ->
            toEncoder target (Type.dealias args alias)

        Can.TLambda _ _ ->
            crash "toEncoder: function"

        Can.TVar _ ->
            crash "toEncoder: type variable"

        Can.TUnit ->
            Names.fmap (Opt.Function [ Name.dollar ]) (encode target "null")

        Can.TTuple a b cs ->
            encodeTuple target a b cs

        Can.TType _ name args ->
            case args of
                [] ->
                    if name == Name.float then
                        encode target "float"

                    else if name == Name.int then
                        encode target "int"

                    else if name == Name.bool then
                        encode target "bool"

                    else if name == Name.string then
                        encode target "string"

                    else if name == Name.value then
                        Names.registerGlobal A.zero (ModuleName.basics target) Name.identity_

                    else if name == Name.bytes then
                        Names.registerGlobal A.zero (ModuleName.basics target) Name.identity_

                    else
                        crash "toEncoder: bad custom type"

                [ arg ] ->
                    if name == Name.maybe then
                        encodeMaybe target arg

                    else if name == Name.list then
                        encodeList target arg

                    else if name == Name.array then
                        encodeArray target arg

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
                    toEncoder target fieldType
                        |> Names.fmap
                            (\encoder ->
                                let
                                    value : Opt.Expr
                                    value =
                                        Opt.Call A.zero encoder [ Opt.Access (Opt.VarLocal Name.dollar) A.zero name ]
                                in
                                Opt.Tuple A.zero (Opt.Str A.zero (Name.toElmString name)) value []
                            )
            in
            encode target "object"
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


encodeMaybe : Target -> Can.Type -> Names.Tracker Opt.Expr
encodeMaybe target tipe =
    encode target "null"
        |> Names.bind
            (\null ->
                toEncoder target tipe
                    |> Names.bind
                        (\encoder ->
                            Names.registerGlobal A.zero (ModuleName.maybe target) "destruct"
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


encodeList : Target -> Can.Type -> Names.Tracker Opt.Expr
encodeList target tipe =
    encode target "list"
        |> Names.bind
            (\list ->
                toEncoder target tipe
                    |> Names.fmap (Opt.Call A.zero list << List.singleton)
            )


encodeArray : Target -> Can.Type -> Names.Tracker Opt.Expr
encodeArray target tipe =
    encode target "array"
        |> Names.bind
            (\array ->
                toEncoder target tipe
                    |> Names.fmap (Opt.Call A.zero array << List.singleton)
            )


encodeTuple : Target -> Can.Type -> Can.Type -> List Can.Type -> Names.Tracker Opt.Expr
encodeTuple target a b cs =
    let
        let_ : Name -> Index.ZeroBased -> Opt.Expr -> Opt.Expr
        let_ arg index body =
            Opt.Destruct (Opt.Destructor arg (Opt.Index index (Opt.Root Name.dollar))) body

        letCs_ : Name -> Int -> Opt.Expr -> Opt.Expr
        letCs_ arg index body =
            Opt.Destruct (Opt.Destructor arg (Opt.ArrayIndex index (Opt.Field "cs" (Opt.Root Name.dollar)))) body

        encodeArg : Name -> Can.Type -> Names.Tracker Opt.Expr
        encodeArg arg tipe =
            toEncoder target tipe
                |> Names.fmap (\encoder -> Opt.Call A.zero encoder [ Opt.VarLocal arg ])
    in
    encode target "list"
        |> Names.bind
            (\list ->
                Names.registerGlobal A.zero (ModuleName.basics target) Name.identity_
                    |> Names.bind
                        (\identity ->
                            Names.bind
                                (\arg1 ->
                                    Names.bind
                                        (\arg2 ->
                                            let
                                                ( _, indexedCs ) =
                                                    List.foldl (\( i, c ) ( index, acc ) -> ( Index.next index, ( i, index, c ) :: acc ))
                                                        ( Index.third, [] )
                                                        (List.indexedMap Tuple.pair cs)
                                                        |> Tuple.mapSecond List.reverse
                                            in
                                            List.foldl
                                                (\( _, i, tipe ) acc ->
                                                    Names.bind (\encodedArg -> Names.fmap (flip (++) [ encodedArg ]) acc)
                                                        (encodeArg (JsName.fromIndex i) tipe)
                                                )
                                                (Names.pure [ arg1, arg2 ])
                                                indexedCs
                                                |> Names.fmap
                                                    (\args ->
                                                        Opt.Function [ Name.dollar ]
                                                            (let_ "a"
                                                                Index.first
                                                                (let_ "b"
                                                                    Index.second
                                                                    (List.foldr (\( i, index, _ ) -> letCs_ (JsName.fromIndex index) i)
                                                                        (Opt.Call A.zero list [ identity, Opt.List A.zero args ])
                                                                        indexedCs
                                                                    )
                                                                )
                                                            )
                                                    )
                                        )
                                        (encodeArg "b" b)
                                )
                                (encodeArg "a" a)
                        )
            )



-- FLAGS DECODER


toFlagsDecoder : Target -> Can.Type -> Names.Tracker Opt.Expr
toFlagsDecoder target tipe =
    case tipe of
        Can.TUnit ->
            Names.fmap (\succeed -> Opt.Call A.zero succeed [ Opt.Unit ])
                (decode target "succeed")

        _ ->
            toDecoder target tipe



-- DECODE


toDecoder : Target -> Can.Type -> Names.Tracker Opt.Expr
toDecoder target tipe =
    case tipe of
        Can.TLambda _ _ ->
            crash "functions should not be allowed through input ports"

        Can.TVar _ ->
            crash "type variables should not be allowed through input ports"

        Can.TAlias _ _ args alias ->
            toDecoder target (Type.dealias args alias)

        Can.TUnit ->
            decodeTuple0 target

        Can.TTuple a b cs ->
            decodeTuple target a b cs

        Can.TType _ name args ->
            case ( name, args ) of
                ( "Float", [] ) ->
                    decode target "float"

                ( "Int", [] ) ->
                    decode target "int"

                ( "Bool", [] ) ->
                    decode target "bool"

                ( "String", [] ) ->
                    decode target "string"

                ( "Value", [] ) ->
                    decode target "value"

                ( "Bytes", [] ) ->
                    decode target "bytes"

                ( "Maybe", [ arg ] ) ->
                    decodeMaybe target arg

                ( "List", [ arg ] ) ->
                    decodeList target arg

                ( "Array", [ arg ] ) ->
                    decodeArray target arg

                _ ->
                    crash "toDecoder: bad type"

        Can.TRecord _ (Just _) ->
            crash "toDecoder: bad record"

        Can.TRecord fields Nothing ->
            decodeRecord target fields



-- DECODE MAYBE


decodeMaybe : Target -> Can.Type -> Names.Tracker Opt.Expr
decodeMaybe target tipe =
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
                                                (toDecoder target tipe)
                                        )
                                        (decode target "map")
                                )
                                (decode target "null")
                        )
                        (decode target "oneOf")
                )
                (Names.registerGlobal A.zero (ModuleName.maybe target) "Just")
        )
        (Names.registerGlobal A.zero (ModuleName.maybe target) "Nothing")



-- DECODE LIST


decodeList : Target -> Can.Type -> Names.Tracker Opt.Expr
decodeList target tipe =
    Names.bind
        (\list ->
            Names.fmap (Opt.Call A.zero list << List.singleton)
                (toDecoder target tipe)
        )
        (decode target "list")



-- DECODE ARRAY


decodeArray : Target -> Can.Type -> Names.Tracker Opt.Expr
decodeArray target tipe =
    Names.bind
        (\array ->
            Names.fmap (Opt.Call A.zero array << List.singleton)
                (toDecoder target tipe)
        )
        (decode target "array")



-- DECODE TUPLES


decodeTuple0 : Target -> Names.Tracker Opt.Expr
decodeTuple0 target =
    Names.fmap (\null -> Opt.Call A.zero null [ Opt.Unit ])
        (decode target "null")


decodeTuple : Target -> Can.Type -> Can.Type -> List Can.Type -> Names.Tracker Opt.Expr
decodeTuple target a b cs =
    Names.bind
        (\succeed ->
            let
                ( allElems, lastElem ) =
                    case List.reverse cs of
                        c :: rest ->
                            ( a :: b :: List.reverse rest, c )

                        _ ->
                            ( [ a ], b )

                tuple : Opt.Expr
                tuple =
                    Opt.Tuple A.zero (toLocal 0) (toLocal 1) (List.indexedMap (\i _ -> toLocal (i + 2)) cs)
            in
            List.foldr (\( i, c ) -> Names.bind (indexAndThen target i c))
                (indexAndThen target (List.length cs + 1) lastElem (Opt.Call A.zero succeed [ tuple ]))
                (List.indexedMap Tuple.pair allElems)
        )
        (decode target "succeed")


toLocal : Int -> Opt.Expr
toLocal index =
    Opt.VarLocal (Name.fromVarIndex index)


indexAndThen : Target -> Int -> Can.Type -> Opt.Expr -> Names.Tracker Opt.Expr
indexAndThen target i tipe decoder =
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
                        (toDecoder target tipe)
                )
                (decode target "index")
        )
        (decode target "andThen")



-- DECODE RECORDS


decodeRecord : Target -> Dict String Name.Name Can.FieldType -> Names.Tracker Opt.Expr
decodeRecord target fields =
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
                        List.foldl (\fieldDecoder -> Names.bind (\optCall -> fieldAndThen target optCall fieldDecoder))
                            (Names.pure (Opt.Call A.zero succeed [ record ]))
                            fieldDecoders
                    )
        )
        (decode target "succeed")


fieldAndThen : Target -> Opt.Expr -> ( Name.Name, Can.FieldType ) -> Names.Tracker Opt.Expr
fieldAndThen target decoder ( key, Can.FieldType _ tipe ) =
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
                        (toDecoder target tipe)
                )
                (decode target "field")
        )
        (decode target "andThen")



-- GLOBALS HELPERS


encode : Target -> Name -> Names.Tracker Opt.Expr
encode target name =
    Names.registerGlobal A.zero (ModuleName.jsonEncode target) name


decode : Target -> Name -> Names.Tracker Opt.Expr
decode target name =
    Names.registerGlobal A.zero (ModuleName.jsonDecode target) name
