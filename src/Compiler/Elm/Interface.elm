module Compiler.Elm.Interface exposing
    ( Alias(..)
    , Binop(..)
    , DependencyInterface(..)
    , Interface(..)
    , Union(..)
    , dependencyInterfaceCodec
    , extractAlias
    , extractUnion
    , fromModule
    , interfaceCodec
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
import Compiler.Reporting.Annotation as A
import Compiler.Serialize as S
import Data.Map as Dict exposing (Dict)
import Serialize exposing (Codec)
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- INTERFACE


type Interface
    = Interface Pkg.Name (Dict String Name.Name Can.Annotation) (Dict String Name.Name Union) (Dict String Name.Name Alias) (Dict String Name.Name Binop)


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


fromModule : Pkg.Name -> Can.Module -> Dict String Name.Name Can.Annotation -> Interface
fromModule home (Can.Module _ exports _ _ unions aliases binops _) annotations =
    Interface home
        (restrict exports annotations)
        (restrictUnions exports unions)
        (restrictAliases exports aliases)
        (restrict exports (Dict.map (\_ -> toOp annotations) binops))


restrict : Can.Exports -> Dict String Name.Name a -> Dict String Name.Name a
restrict exports dict =
    case exports of
        Can.ExportEverything _ ->
            dict

        Can.Export explicitExports ->
            Dict.intersection compare dict explicitExports


toOp : Dict String Name.Name Can.Annotation -> Can.Binop -> Binop
toOp types (Can.Binop_ associativity precedence name) =
    Binop name (Utils.find identity name types) associativity precedence


restrictUnions : Can.Exports -> Dict String Name.Name Can.Union -> Dict String Name.Name Union
restrictUnions exports unions =
    case exports of
        Can.ExportEverything _ ->
            Dict.map (\_ -> OpenUnion) unions

        Can.Export explicitExports ->
            Dict.merge compare
                (\_ _ result -> result)
                (\k (A.At _ export) union result ->
                    case export of
                        Can.ExportUnionOpen ->
                            Dict.insert identity k (OpenUnion union) result

                        Can.ExportUnionClosed ->
                            Dict.insert identity k (ClosedUnion union) result

                        _ ->
                            crash "impossible exports discovered in restrictUnions"
                )
                (\k union result -> Dict.insert identity k (PrivateUnion union) result)
                explicitExports
                unions
                Dict.empty


restrictAliases : Can.Exports -> Dict String Name.Name Can.Alias -> Dict String Name.Name Alias
restrictAliases exports aliases =
    case exports of
        Can.ExportEverything _ ->
            Dict.map (\_ alias -> PublicAlias alias) aliases

        Can.Export explicitExports ->
            Dict.merge compare
                (\_ _ result -> result)
                (\k _ alias result -> Dict.insert identity k (PublicAlias alias) result)
                (\k alias result -> Dict.insert identity k (PrivateAlias alias) result)
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
    | Private Pkg.Name (Dict String Name.Name Can.Union) (Dict String Name.Name Can.Alias)


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


interfaceCodec : Codec e Interface
interfaceCodec =
    Serialize.customType
        (\interfaceCodecEncoder (Interface home values unions aliases binops) ->
            interfaceCodecEncoder home values unions aliases binops
        )
        |> Serialize.variant5 Interface
            Pkg.nameCodec
            (S.assocListDict identity compare Serialize.string Can.annotationCodec)
            (S.assocListDict identity compare Serialize.string unionCodec)
            (S.assocListDict identity compare Serialize.string aliasCodec)
            (S.assocListDict identity compare Serialize.string binopCodec)
        |> Serialize.finishCustomType


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


binopCodec : Codec e Binop
binopCodec =
    Serialize.customType
        (\binopCodecEncoder (Binop name annotation associativity precedence) ->
            binopCodecEncoder name annotation associativity precedence
        )
        |> Serialize.variant4 Binop Serialize.string Can.annotationCodec Binop.associativityCodec Binop.precedenceCodec
        |> Serialize.finishCustomType


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
        |> Serialize.variant3 Private Pkg.nameCodec (S.assocListDict identity compare Serialize.string Can.unionCodec) (S.assocListDict identity compare Serialize.string Can.aliasCodec)
        |> Serialize.finishCustomType
