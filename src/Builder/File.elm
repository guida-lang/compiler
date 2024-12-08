module Builder.File exposing
    ( Time(..)
    , exists
    , getTime
    , readBinary
    , readUtf8
    , remove
    , timeDecoder
    , timeEncoder
    , writeBinary
    , writeBuilder
    , writePackage
    , writeUtf8
    , zeroTime
    )

import Codec.Archive.Zip as Zip
import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO exposing (IO(..))
import Time
import Utils.Main as Utils exposing (FilePath)



-- TIME


type Time
    = Time Time.Posix


getTime : FilePath -> IO Time
getTime path =
    IO.fmap Time (Utils.dirGetModificationTime path)


zeroTime : Time
zeroTime =
    Time (Time.millisToPosix 0)



-- BINARY


writeBinary : (a -> Encode.Value) -> FilePath -> a -> IO ()
writeBinary encoder path value =
    let
        dir : FilePath
        dir =
            Utils.fpDropFileName path
    in
    Utils.dirCreateDirectoryIfMissing True dir
        |> IO.bind (\_ -> Utils.binaryEncodeFile encoder path value)


readBinary : Decode.Decoder a -> FilePath -> IO (Maybe a)
readBinary decoder path =
    Utils.dirDoesFileExist path
        |> IO.bind
            (\pathExists ->
                if pathExists then
                    Utils.binaryDecodeFileOrFail decoder path
                        |> IO.bind
                            (\result ->
                                case result of
                                    Ok a ->
                                        IO.pure (Just a)

                                    Err ( offset, message ) ->
                                        IO.hPutStrLn IO.stderr
                                            (Utils.unlines
                                                [ "+-------------------------------------------------------------------------------"
                                                , "|  Corrupt File: " ++ path
                                                , "|   Byte Offset: " ++ String.fromInt offset
                                                , "|       Message: " ++ message
                                                , "|"
                                                , "| Please report this to https://github.com/elm/compiler/issues"
                                                , "| Trying to continue anyway."
                                                , "+-------------------------------------------------------------------------------"
                                                ]
                                            )
                                            |> IO.fmap (\_ -> Nothing)
                            )

                else
                    IO.pure Nothing
            )



-- WRITE UTF-8


writeUtf8 : FilePath -> String -> IO ()
writeUtf8 path content =
    IO (\s -> ( s, IO.WriteString IO.pure path content ))



-- READ UTF-8


readUtf8 : FilePath -> IO String
readUtf8 path =
    IO (\s -> ( s, IO.Read IO.pure path ))



-- WRITE BUILDER


writeBuilder : FilePath -> String -> IO ()
writeBuilder path builder =
    IO (\s -> ( s, IO.WriteString IO.pure path builder ))



-- WRITE PACKAGE


writePackage : FilePath -> Zip.Archive -> IO ()
writePackage destination archive =
    case Zip.zEntries archive of
        [] ->
            IO.pure ()

        entry :: entries ->
            let
                root : Int
                root =
                    String.length (Zip.eRelativePath entry)
            in
            Utils.mapM_ (writeEntry destination root) entries


writeEntry : FilePath -> Int -> Zip.Entry -> IO ()
writeEntry destination root entry =
    let
        path : String
        path =
            String.dropLeft root (Zip.eRelativePath entry)
    in
    if
        String.startsWith "src/" path
            || (path == "LICENSE")
            || (path == "README.md")
            || (path == "elm.json")
    then
        if not (String.isEmpty path) && String.endsWith "/" path then
            Utils.dirCreateDirectoryIfMissing True (Utils.fpForwardSlash destination path)

        else
            writeUtf8 (Utils.fpForwardSlash destination path) (Zip.fromEntry entry)

    else
        IO.pure ()



-- EXISTS


exists : FilePath -> IO Bool
exists path =
    Utils.dirDoesFileExist path



-- REMOVE FILES


remove : FilePath -> IO ()
remove path =
    Utils.dirDoesFileExist path
        |> IO.bind
            (\exists_ ->
                if exists_ then
                    Utils.dirRemoveFile path

                else
                    IO.pure ()
            )



-- ENCODERS and DECODERS


timeEncoder : Time -> Encode.Value
timeEncoder (Time posix) =
    Encode.int (Time.posixToMillis posix)


timeDecoder : Decode.Decoder Time
timeDecoder =
    Decode.map (Time << Time.millisToPosix) Decode.int
