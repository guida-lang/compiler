module Builder.File exposing
    ( Time(..)
    , exists
    , getTime
    , readBinary
    , readStdin
    , readUtf8
    , remove
    , timeCodec
    , writeBinary
    , writeBuilder
    , writePackage
    , writeUtf8
    , zeroTime
    )

import Codec.Archive.Zip as Zip
import Serialize exposing (Codec)
import System.IO as IO exposing (IO)
import Time
import Utils.Impure as Impure
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


writeBinary : Codec e a -> FilePath -> a -> IO ()
writeBinary codec path value =
    let
        dir : FilePath
        dir =
            Utils.fpDropFileName path
    in
    Utils.dirCreateDirectoryIfMissing True dir
        |> IO.bind (\_ -> Utils.binaryEncodeFile codec path value)


readBinary : Codec e a -> FilePath -> IO (Maybe a)
readBinary codec path =
    Utils.dirDoesFileExist path
        |> IO.bind
            (\pathExists ->
                if pathExists then
                    Utils.binaryDecodeFileOrFail codec path
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
writeUtf8 =
    IO.writeString



-- READ UTF-8


readUtf8 : FilePath -> IO String
readUtf8 path =
    Impure.task "read" [] (Impure.StringBody path) (Impure.StringResolver identity)


readStdin : IO String
readStdin =
    Impure.task "readStdin" [] Impure.EmptyBody (Impure.StringResolver identity)



-- WRITE BUILDER


writeBuilder : FilePath -> String -> IO ()
writeBuilder =
    IO.writeString



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


timeCodec : Codec e Time
timeCodec =
    Serialize.int |> Serialize.map (Time << Time.millisToPosix) (\(Time posix) -> Time.posixToMillis posix)
