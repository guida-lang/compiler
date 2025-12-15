module Compiler.Generate.Target exposing (Target(..), decoder, encoder)

{-| Represents which output format the compiler should generate.

A `Target` is determined at the project root by checking for either a
`guida.json` file or an `elm.json` file:

  - If a `guida.json` file exists, the compiler uses the `Guida` target.
  - If an `elm.json` file exists, the compiler uses the `Elm` target.

-}

import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE


type Target
    = GuidaTarget
    | ElmTarget


encoder : Target -> BE.Encoder
encoder target =
    case target of
        GuidaTarget ->
            BE.unsignedInt8 0

        ElmTarget ->
            BE.unsignedInt8 1


decoder : BD.Decoder Target
decoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed GuidaTarget

                    1 ->
                        BD.succeed ElmTarget

                    _ ->
                        BD.fail
            )
