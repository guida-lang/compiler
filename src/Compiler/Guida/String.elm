module Compiler.Guida.String exposing
    ( Chunk(..)
    , fromChunks
    )

import Hex
import Numeric.Integer as NI



-- FROM CHUNKS


type Chunk
    = Slice Int Int
    | Escape Char
    | CodePoint Int


fromChunks : String -> List Chunk -> String
fromChunks src chunks =
    writeChunks src "" 0 chunks


writeChunks : String -> String -> Int -> List Chunk -> String
writeChunks src mba offset chunks =
    case chunks of
        [] ->
            mba

        chunk :: otherChunks ->
            case chunk of
                Slice ptr len ->
                    let
                        newOffset : Int
                        newOffset =
                            offset + len
                    in
                    writeChunks src (mba ++ String.slice ptr (ptr + len) src) newOffset otherChunks

                Escape word ->
                    let
                        newOffset : Int
                        newOffset =
                            offset + 2
                    in
                    writeChunks src (mba ++ "\\" ++ String.fromChar word) newOffset otherChunks

                CodePoint code ->
                    if code < 0xFFFF then
                        let
                            newOffset : Int
                            newOffset =
                                offset + 6
                        in
                        writeChunks src (mba ++ writeCode code) newOffset otherChunks

                    else
                        let
                            ( hi, lo ) =
                                NI.divMod (code - 0x00010000) 0x0400

                            hiCode : String
                            hiCode =
                                writeCode (hi + 0xD800)

                            lowCode : String
                            lowCode =
                                writeCode (lo + 0xDC00)

                            newOffset : Int
                            newOffset =
                                offset + 12
                        in
                        writeChunks src (mba ++ hiCode ++ lowCode) newOffset otherChunks


writeCode : Int -> String
writeCode code =
    "\\u" ++ String.padLeft 4 '0' (String.toUpper (Hex.toString code))
