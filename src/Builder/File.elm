module Builder.File exposing
    ( Time(..)
    , exists
    , getTime
    , readBinary
    , readStdin
    , readUtf8
    , remove
    , timeDecoder
    , timeEncoder
    , writeBinary
    , writePackage
    , writeUtf8
    , zeroTime
    )

import Codec.Archive.Zip as Zip
import System.IO as IO
import Task exposing (Task)
import Time
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Impure as Impure
import Utils.Main as Utils exposing (FilePath)
import Utils.Task.Extra as Task



-- TIME


type Time
    = Time Time.Posix


getTime : FilePath -> Task Never Time
getTime path =
    Task.fmap Time (Utils.dirGetModificationTime path)


zeroTime : Time
zeroTime =
    Time (Time.millisToPosix 0)



-- BINARY


writeBinary : (a -> BE.Encoder) -> FilePath -> a -> Task Never ()
writeBinary toEncoder path value =
    let
        dir : FilePath
        dir =
            Utils.fpDropFileName path
    in
    Utils.dirCreateDirectoryIfMissing True dir
        |> Task.bind (\_ -> Utils.binaryEncodeFile toEncoder path value)


readBinary : BD.Decoder a -> FilePath -> Task Never (Maybe a)
readBinary decoder path =
    Utils.dirDoesFileExist path
        |> Task.bind
            (\pathExists ->
                if pathExists then
                    Utils.binaryDecodeFileOrFail decoder path
                        |> Task.bind
                            (\result ->
                                case result of
                                    Ok a ->
                                        Task.pure (Just a)

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
                                            |> Task.fmap (\_ -> Nothing)
                            )

                else
                    Task.pure Nothing
            )



-- WRITE UTF-8


writeUtf8 : FilePath -> String -> Task Never ()
writeUtf8 =
    IO.writeString



-- READ UTF-8


readUtf8 : FilePath -> Task Never String
readUtf8 path =
    Impure.task "read" [] (Impure.StringBody path) (Impure.StringResolver identity)


readStdin : Task Never String
readStdin =
    Impure.task "readStdin" [] Impure.EmptyBody (Impure.StringResolver identity)



-- WRITE PACKAGE


writePackage : FilePath -> Zip.Archive -> Task Never ()
writePackage destination archive =
    case Zip.zEntries archive of
        [] ->
            Task.pure ()

        entry :: entries ->
            let
                root : Int
                root =
                    String.length (Zip.eRelativePath entry)
            in
            Utils.mapM_ (writeEntry destination root) entries


writeEntry : FilePath -> Int -> Zip.Entry -> Task Never ()
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
            Utils.dirCreateDirectoryIfMissing True (Utils.fpCombine destination path)

        else
            writeUtf8 (Utils.fpCombine destination path) (Zip.fromEntry entry)

    else
        Task.pure ()



-- EXISTS


exists : FilePath -> Task Never Bool
exists path =
    Utils.dirDoesFileExist path



-- REMOVE FILES


remove : FilePath -> Task Never ()
remove path =
    Utils.dirDoesFileExist path
        |> Task.bind
            (\exists_ ->
                if exists_ then
                    Utils.dirRemoveFile path

                else
                    Task.pure ()
            )



-- ENCODERS and DECODERS


timeEncoder : Time -> BE.Encoder
timeEncoder (Time posix) =
    BE.int (Time.posixToMillis posix)


timeDecoder : BD.Decoder Time
timeDecoder =
    BD.map (Time << Time.millisToPosix) BD.int
