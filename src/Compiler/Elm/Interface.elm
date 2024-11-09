module Compiler.Elm.Interface exposing
    ( Alias(..)
    , Binop(..)
    , DependencyInterface(..)
    , Interface(..)
    , Union(..)
    , dependencyInterfaceCodec
    , dependencyInterfaceDecoder
    , dependencyInterfaceEncoder
    , extractAlias
    , extractUnion
    , fromModule
    , interfaceCodec
    , interfaceDecoder
    , interfaceEncoder
    , private
    , privatize
    , public
    , toPublicAlias
    , toPublicUnion
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Binop as Binop
import Compiler.Data.Name as Name
import Compiler.Elm.Package as Pkg
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Reporting.Annotation as A
import Compiler.Serialize as S
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Serialize exposing (Codec)
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- INTERFACE


type Interface
    = Interface Pkg.Name (Dict Name.Name Can.Annotation) (Dict Name.Name Union) (Dict Name.Name Alias) (Dict Name.Name Binop)


type Union
    = OpenUnion Can.Union
    | ClosedUnion Can.Union
    | PrivateUnion Can.Union


type Alias
    = PublicAlias Can.Alias
    | PrivateAlias Can.Alias


type Binop
    = Binop Name.Name Can.Annotation Binop.Associativity Binop.Precedence



-- FROM MODULE


fromModule : Pkg.Name -> Can.Module -> Dict Name.Name Can.Annotation -> Interface
fromModule home (Can.Module _ exports _ _ unions aliases binops _) annotations =
    Interface home
        (restrict exports annotations)
        (restrictUnions exports unions)
        (restrictAliases exports aliases)
        (restrict exports (Dict.map (\_ -> toOp annotations) binops))


restrict : Can.Exports -> Dict Name.Name a -> Dict Name.Name a
restrict exports dict =
    case exports of
        Can.ExportEverything _ ->
            dict

        Can.Export explicitExports ->
            Dict.intersection dict explicitExports


toOp : Dict Name.Name Can.Annotation -> Can.Binop -> Binop
toOp types (Can.Binop_ associativity precedence name) =
    Binop name (Utils.find name types) associativity precedence


restrictUnions : Can.Exports -> Dict Name.Name Can.Union -> Dict Name.Name Union
restrictUnions exports unions =
    case exports of
        Can.ExportEverything _ ->
            Dict.map (\_ -> OpenUnion) unions

        Can.Export explicitExports ->
            Dict.merge
                (\_ _ result -> result)
                (\k (A.At _ export) union result ->
                    case export of
                        Can.ExportUnionOpen ->
                            Dict.insert compare k (OpenUnion union) result

                        Can.ExportUnionClosed ->
                            Dict.insert compare k (ClosedUnion union) result

                        _ ->
                            crash "impossible exports discovered in restrictUnions"
                )
                (\k union result -> Dict.insert compare k (PrivateUnion union) result)
                explicitExports
                unions
                Dict.empty


restrictAliases : Can.Exports -> Dict Name.Name Can.Alias -> Dict Name.Name Alias
restrictAliases exports aliases =
    case exports of
        Can.ExportEverything _ ->
            Dict.map (\_ alias -> PublicAlias alias) aliases

        Can.Export explicitExports ->
            Dict.merge (\_ _ result -> result)
                (\k _ alias result -> Dict.insert compare k (PublicAlias alias) result)
                (\k alias result -> Dict.insert compare k (PrivateAlias alias) result)
                explicitExports
                aliases
                Dict.empty



-- TO PUBLIC


toPublicUnion : Union -> Maybe Can.Union
toPublicUnion iUnion =
    case iUnion of
        OpenUnion union ->
            Just union

        ClosedUnion (Can.Union vars _ _ opts) ->
            Just (Can.Union vars [] 0 opts)

        PrivateUnion _ ->
            Nothing


toPublicAlias : Alias -> Maybe Can.Alias
toPublicAlias iAlias =
    case iAlias of
        PublicAlias alias ->
            Just alias

        PrivateAlias _ ->
            Nothing



-- DEPENDENCY INTERFACE


type DependencyInterface
    = Public Interface
    | Private Pkg.Name (Dict Name.Name Can.Union) (Dict Name.Name Can.Alias)


public : Interface -> DependencyInterface
public =
    Public


private : Interface -> DependencyInterface
private (Interface pkg _ unions aliases _) =
    Private pkg (Dict.map (\_ -> extractUnion) unions) (Dict.map (\_ -> extractAlias) aliases)


extractUnion : Union -> Can.Union
extractUnion iUnion =
    case iUnion of
        OpenUnion union ->
            union

        ClosedUnion union ->
            union

        PrivateUnion union ->
            union


extractAlias : Alias -> Can.Alias
extractAlias iAlias =
    case iAlias of
        PublicAlias alias ->
            alias

        PrivateAlias alias ->
            alias


privatize : DependencyInterface -> DependencyInterface
privatize di =
    case di of
        Public i ->
            private i

        Private _ _ _ ->
            di



-- ENCODERS and DECODERS


interfaceEncoder : Interface -> Encode.Value
interfaceEncoder (Interface home values unions aliases binops) =
    Encode.object
        [ ( "type", Encode.string "Interface" )
        , ( "home", Pkg.nameEncoder home )
        , ( "values", E.assocListDict Encode.string Can.annotationEncoder values )
        , ( "unions", E.assocListDict Encode.string unionEncoder unions )
        , ( "aliases", E.assocListDict Encode.string aliasEncoder aliases )
        , ( "binops", E.assocListDict Encode.string binopEncoder binops )
        ]


interfaceDecoder : Decode.Decoder Interface
interfaceDecoder =
    Decode.map5 Interface
        (Decode.field "home" Pkg.nameDecoder)
        (Decode.field "values" (D.assocListDict compare Decode.string Can.annotationDecoder))
        (Decode.field "unions" (D.assocListDict compare Decode.string unionDecoder))
        (Decode.field "aliases" (D.assocListDict compare Decode.string aliasDecoder))
        (Decode.field "binops" (D.assocListDict compare Decode.string binopDecoder))


interfaceCodec : Codec e Interface
interfaceCodec =
    Serialize.customType
        (\interfaceCodecEncoder (Interface home values unions aliases binops) ->
            interfaceCodecEncoder home values unions aliases binops
        )
        |> Serialize.variant5 Interface
            Pkg.nameCodec
            (S.assocListDict compare Serialize.string Can.annotationCodec)
            (S.assocListDict compare Serialize.string unionCodec)
            (S.assocListDict compare Serialize.string aliasCodec)
            (S.assocListDict compare Serialize.string binopCodec)
        |> Serialize.finishCustomType


unionEncoder : Union -> Encode.Value
unionEncoder union_ =
    case union_ of
        OpenUnion union ->
            Encode.object
                [ ( "type", Encode.string "OpenUnion" )
                , ( "union", Can.unionEncoder union )
                ]

        ClosedUnion union ->
            Encode.object
                [ ( "type", Encode.string "ClosedUnion" )
                , ( "union", Can.unionEncoder union )
                ]

        PrivateUnion union ->
            Encode.object
                [ ( "type", Encode.string "ClosedUnion" )
                , ( "union", Can.unionEncoder union )
                ]


unionDecoder : Decode.Decoder Union
unionDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "OpenUnion" ->
                        Decode.map OpenUnion
                            (Decode.field "union" Can.unionDecoder)

                    "ClosedUnion" ->
                        Decode.map ClosedUnion
                            (Decode.field "union" Can.unionDecoder)

                    "PrivateUnion" ->
                        Decode.map ClosedUnion
                            (Decode.field "union" Can.unionDecoder)

                    _ ->
                        Decode.fail ("Unknown Union's type: " ++ type_)
            )


unionCodec : Codec e Union
unionCodec =
    Serialize.customType
        (\openUnionEncoder closedUnionEncoder privateUnionEncoder value ->
            case value of
                OpenUnion union ->
                    openUnionEncoder union

                ClosedUnion union ->
                    closedUnionEncoder union

                PrivateUnion union ->
                    privateUnionEncoder union
        )
        |> Serialize.variant1 OpenUnion Can.unionCodec
        |> Serialize.variant1 ClosedUnion Can.unionCodec
        |> Serialize.variant1 PrivateUnion Can.unionCodec
        |> Serialize.finishCustomType


aliasEncoder : Alias -> Encode.Value
aliasEncoder aliasValue =
    case aliasValue of
        PublicAlias alias_ ->
            Encode.object
                [ ( "type", Encode.string "PublicAlias" )
                , ( "alias", Can.aliasEncoder alias_ )
                ]

        PrivateAlias alias_ ->
            Encode.object
                [ ( "type", Encode.string "PrivateAlias" )
                , ( "alias", Can.aliasEncoder alias_ )
                ]


aliasDecoder : Decode.Decoder Alias
aliasDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "PublicAlias" ->
                        Decode.map PublicAlias
                            (Decode.field "alias" Can.aliasDecoder)

                    "PrivateAlias" ->
                        Decode.map PrivateAlias
                            (Decode.field "alias" Can.aliasDecoder)

                    _ ->
                        Decode.fail ("Unknown Alias' type: " ++ type_)
            )


aliasCodec : Codec e Alias
aliasCodec =
    Serialize.customType
        (\publicAliasEncoder privateAliasEncoder value ->
            case value of
                PublicAlias alias_ ->
                    publicAliasEncoder alias_

                PrivateAlias alias_ ->
                    privateAliasEncoder alias_
        )
        |> Serialize.variant1 PublicAlias Can.aliasCodec
        |> Serialize.variant1 PrivateAlias Can.aliasCodec
        |> Serialize.finishCustomType


binopEncoder : Binop -> Encode.Value
binopEncoder (Binop name annotation associativity precedence) =
    Encode.object
        [ ( "type", Encode.string "Binop" )
        , ( "name", Encode.string name )
        , ( "annotation", Can.annotationEncoder annotation )
        , ( "associativity", Binop.associativityEncoder associativity )
        , ( "precedence", Binop.precedenceEncoder precedence )
        ]


binopDecoder : Decode.Decoder Binop
binopDecoder =
    Decode.map4 Binop
        (Decode.field "name" Decode.string)
        (Decode.field "annotation" Can.annotationDecoder)
        (Decode.field "associativity" Binop.associativityDecoder)
        (Decode.field "precedence" Binop.precedenceDecoder)


binopCodec : Codec e Binop
binopCodec =
    Serialize.customType
        (\binopCodecEncoder (Binop name annotation associativity precedence) ->
            binopCodecEncoder name annotation associativity precedence
        )
        |> Serialize.variant4 Binop Serialize.string Can.annotationCodec Binop.associativityCodec Binop.precedenceCodec
        |> Serialize.finishCustomType


dependencyInterfaceEncoder : DependencyInterface -> Encode.Value
dependencyInterfaceEncoder dependencyInterface =
    case dependencyInterface of
        Public i ->
            Encode.object
                [ ( "type", Encode.string "Public" )
                , ( "i", interfaceEncoder i )
                ]

        Private pkg unions aliases ->
            Encode.object
                [ ( "type", Encode.string "Private" )
                , ( "pkg", Pkg.nameEncoder pkg )
                , ( "unions", E.assocListDict Encode.string Can.unionEncoder unions )
                , ( "aliases", E.assocListDict Encode.string Can.aliasEncoder aliases )
                ]


dependencyInterfaceDecoder : Decode.Decoder DependencyInterface
dependencyInterfaceDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Public" ->
                        Decode.map Public (Decode.field "i" interfaceDecoder)

                    "Private" ->
                        Decode.map3 Private
                            (Decode.field "pkg" Pkg.nameDecoder)
                            (Decode.field "unions" (D.assocListDict compare Decode.string Can.unionDecoder))
                            (Decode.field "aliases" (D.assocListDict compare Decode.string Can.aliasDecoder))

                    _ ->
                        Decode.fail ("Failed to decode DependencyInterface's type: " ++ type_)
            )


dependencyInterfaceCodec : Codec e DependencyInterface
dependencyInterfaceCodec =
    Serialize.customType
        (\publicEncoder privateEncoder dependencyInterface ->
            case dependencyInterface of
                Public i ->
                    publicEncoder i

                Private pkg unions aliases ->
                    privateEncoder pkg unions aliases
        )
        |> Serialize.variant1 Public interfaceCodec
        |> Serialize.variant3 Private Pkg.nameCodec (S.assocListDict compare Serialize.string Can.unionCodec) (S.assocListDict compare Serialize.string Can.aliasCodec)
        |> Serialize.finishCustomType
