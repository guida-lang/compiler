module Compiler.Parse.SyntaxVersion exposing
    ( SyntaxVersion(..)
    , decoder
    , encoder
    , fileSyntaxVersion
    )

{-| Compiler.Parse.SyntaxVersion
-}

import Json.Decode as Decode
import Json.Encode as Encode


{-| The `SyntaxVersion` type is used to specify which syntax version to work
with. It provides options to differentiate between the "legacy" Elm syntax,
which the Guida language builds upon, and the new Guida-specific syntax.

This type is useful when building parsers that need to distinguish between
the two syntactic styles and adapt behavior accordingly.

-}
type SyntaxVersion
    = Elm
    | Guida


{-| Returns the syntax version based on a filepath.
-}
fileSyntaxVersion : String -> SyntaxVersion
fileSyntaxVersion path =
    if String.endsWith ".elm" path then
        Elm

    else
        Guida



-- ENCODERS and DECODERS


encoder : SyntaxVersion -> Encode.Value
encoder syntaxVersion =
    case syntaxVersion of
        Elm ->
            Encode.string "Elm"

        Guida ->
            Encode.string "Guida"


decoder : Decode.Decoder SyntaxVersion
decoder =
    Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Elm" ->
                        Decode.succeed Elm

                    "Guida" ->
                        Decode.succeed Guida

                    _ ->
                        Decode.fail ("Failed to decode SyntaxVersion's type: " ++ type_)
            )
