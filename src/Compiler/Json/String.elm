module Compiler.Json.String exposing
    ( fromComment
    , fromName
    , fromSnippet
    , isEmpty
    )

import Compiler.Data.Name as Name
import Compiler.Parse.Primitives as P



-- JSON STRINGS


isEmpty : String -> Bool
isEmpty =
    String.isEmpty



-- FROM


fromSnippet : P.Snippet -> String
fromSnippet (P.Snippet { fptr, offset, length }) =
    String.slice offset (offset + length) fptr


fromName : Name.Name -> String
fromName =
    identity



-- FROM COMMENT


fromComment : P.Snippet -> String
fromComment ((P.Snippet { fptr, offset, length }) as snippet) =
    let
        pos : Int
        pos =
            offset

        end : Int
        end =
            pos + length
    in
    fromChunks snippet (chompChunks fptr pos end pos [])


chompChunks : String -> Int -> Int -> Int -> List Chunk -> List Chunk
chompChunks src pos end start revChunks =
    if pos >= end then
        List.reverse (addSlice start end revChunks)

    else
        let
            word : Char
            word =
                P.unsafeIndex src pos
        in
        case word of
            '\n' ->
                chompChunks src (pos + 1) end (pos + 1) (Escape 'n' :: addSlice start pos revChunks)

            '"' ->
                chompChunks src (pos + 1) end (pos + 1) (Escape '"' :: addSlice start pos revChunks)

            '\\' ->
                chompChunks src (pos + 1) end (pos + 1) (Escape '\\' :: addSlice start pos revChunks)

            {- \r -}
            '\u{000D}' ->
                let
                    newPos : Int
                    newPos =
                        pos + 1
                in
                chompChunks src newPos end newPos (addSlice start pos revChunks)

            _ ->
                let
                    width : Int
                    width =
                        P.getCharWidth word

                    newPos : Int
                    newPos =
                        pos + width
                in
                chompChunks src newPos end start revChunks


addSlice : Int -> Int -> List Chunk -> List Chunk
addSlice start end revChunks =
    if start == end then
        revChunks

    else
        Slice start (end - start) :: revChunks



-- FROM CHUNKS


type Chunk
    = Slice Int Int
    | Escape Char


fromChunks : P.Snippet -> List Chunk -> String
fromChunks snippet chunks =
    writeChunks snippet chunks


writeChunks : P.Snippet -> List Chunk -> String
writeChunks snippet chunks =
    writeChunksHelp snippet chunks ""


writeChunksHelp : P.Snippet -> List Chunk -> String -> String
writeChunksHelp ((P.Snippet { fptr }) as snippet) chunks acc =
    case chunks of
        [] ->
            acc

        chunk :: chunks_ ->
            writeChunksHelp snippet
                chunks_
                (case chunk of
                    Slice offset len ->
                        acc ++ String.slice offset (offset + len) fptr

                    Escape 'n' ->
                        acc ++ String.fromChar '\n'

                    Escape '"' ->
                        acc ++ String.fromChar '"'

                    Escape '\\' ->
                        acc ++ String.fromChar '\\'

                    Escape word ->
                        acc ++ String.fromList [ '\\', word ]
                )
