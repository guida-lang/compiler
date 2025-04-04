module Compiler.Parse.SyntaxVersion exposing
    ( SyntaxVersion(..)
    , decoder
    , encoder
    , fileSyntaxVersion
    )

{-| Compiler.Parse.SyntaxVersion
-}

import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE


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


encoder : SyntaxVersion -> BE.Encoder
encoder syntaxVersion =
    BE.unsignedInt8
        (case syntaxVersion of
            Elm ->
                0

            Guida ->
                1
        )


decoder : BD.Decoder SyntaxVersion
decoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed Elm

                    1 ->
                        BD.succeed Guida

                    _ ->
                        BD.fail
            )
