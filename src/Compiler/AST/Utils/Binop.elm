module Compiler.AST.Utils.Binop exposing
    ( Associativity(..)
    , Precedence
    , associativityCodec
    , precedenceCodec
    )

import Serialize exposing (Codec)



-- BINOP STUFF


type alias Precedence =
    Int


type Associativity
    = Left
    | Non
    | Right


precedenceCodec : Codec e Precedence
precedenceCodec =
    Serialize.int


associativityCodec : Codec e Associativity
associativityCodec =
    Serialize.customType
        (\leftEncoder nonEncoder rightEncoder value ->
            case value of
                Left ->
                    leftEncoder

                Non ->
                    nonEncoder

                Right ->
                    rightEncoder
        )
        |> Serialize.variant0 Left
        |> Serialize.variant0 Non
        |> Serialize.variant0 Right
        |> Serialize.finishCustomType
