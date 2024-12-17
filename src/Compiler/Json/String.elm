module Compiler.Json.String exposing
    ( fromComment
    , fromName
    , fromSnippet
    , isEmpty
    )

import Compiler.Parse.Primitives as P
import Types as T



-- JSON STRINGS


isEmpty : String -> Bool
isEmpty =
    String.isEmpty



-- FROM


fromSnippet : T.CPP_Snippet -> String
fromSnippet (T.CPP_Snippet { fptr, offset, length }) =
    String.slice offset (offset + length) fptr


fromName : T.CDN_Name -> String
fromName =
    identity



-- FROM COMMENT


fromComment : T.CPP_Snippet -> String
fromComment ((T.CPP_Snippet { fptr, offset, length }) as snippet) =
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
                chompEscape src 'n' pos end start revChunks

            '"' ->
                chompEscape src '"' pos end start revChunks

            '\\' ->
                chompEscape src '\\' pos end start revChunks

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


chompEscape : String -> Char -> Int -> Int -> Int -> List Chunk -> List Chunk
chompEscape src escape pos end start revChunks =
    let
        pos1 : Int
        pos1 =
            pos + 1
    in
    chompChunks src pos1 end pos1 (Escape escape :: addSlice start pos revChunks)


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


fromChunks : T.CPP_Snippet -> List Chunk -> String
fromChunks snippet chunks =
    writeChunks snippet chunks


writeChunks : T.CPP_Snippet -> List Chunk -> String
writeChunks ((T.CPP_Snippet { fptr }) as snippet) chunks =
    case chunks of
        [] ->
            ""

        chunk :: chunks_ ->
            case chunk of
                Slice offset len ->
                    String.left len (String.dropLeft offset fptr) ++ writeChunks snippet chunks_

                Escape 'n' ->
                    String.fromChar '\n' ++ writeChunks snippet chunks_

                Escape '"' ->
                    String.fromChar '"' ++ writeChunks snippet chunks_

                Escape '\\' ->
                    String.fromChar '\\' ++ writeChunks snippet chunks_

                Escape word ->
                    String.fromList [ '\\', word ] ++ writeChunks snippet chunks_
