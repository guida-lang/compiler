module Compiler.Parse.SyntaxVersion exposing
    ( SyntaxVersion(..)
    , fileSyntaxVersion
    )

{-| Compiler.Parse.SyntaxVersion
-}

import Utils.Main as Utils exposing (FilePath)


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
fileSyntaxVersion : FilePath -> SyntaxVersion
fileSyntaxVersion path =
    case Utils.fpTakeExtension path of
        ".guida" ->
            Guida

        _ ->
            Elm
