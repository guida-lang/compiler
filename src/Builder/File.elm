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
import System.Directory as Dir
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
    Task.map Time (Dir.getModificationTime path)


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
    Dir.createDirectoryIfMissing True dir
        |> Task.andThen (\_ -> Utils.binaryEncodeFile toEncoder path value)


readBinary : BD.Decoder a -> FilePath -> Task Never (Maybe a)
readBinary decoder path =
    Dir.doesFileExist path
        |> Task.andThen
            (\pathExists ->
                if pathExists then
                    Utils.binaryDecodeFileOrFail decoder path
                        |> Task.andThen
                            (\result ->
                                case result of
                                    Ok a ->
                                        Task.succeed (Just a)

                                    Err ( offset, message ) ->
                                        IO.hPutStrLn IO.stderr
                                            (Utils.unlines
                                                [ "+-------------------------------------------------------------------------------"
                                                , "|  Corrupt File: " ++ path
                                                , "|   Byte Offset: " ++ String.fromInt offset
                                                , "|       Message: " ++ message
                                                , "|"
                                                , "| Please report this to https://github.com/guida-lang/compiler/issues"
                                                , "| Trying to continue anyway."
                                                , "+-------------------------------------------------------------------------------"
                                                ]
                                            )
                                            |> Task.map (\_ -> Nothing)
                            )

                else
                    Task.succeed Nothing
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
            Task.succeed ()

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
            || (path == "guida.json")
            || (path == "elm.json")
    then
        if not (String.isEmpty path) && String.endsWith "/" path then
            Dir.createDirectoryIfMissing True (Utils.fpCombine destination path)

        else
            writeUtf8 (Utils.fpCombine destination path) (Zip.fromEntry entry)

    else
        Task.succeed ()



-- EXISTS


exists : FilePath -> Task Never Bool
exists path =
    Dir.doesFileExist path



-- REMOVE FILES


remove : FilePath -> Task Never ()
remove path =
    Dir.doesFileExist path
        |> Task.andThen
            (\exists_ ->
                if exists_ then
                    Dir.removeFile path

                else
                    Task.succeed ()
            )



-- ENCODERS and DECODERS


timeEncoder : Time -> BE.Encoder
timeEncoder (Time posix) =
    BE.int (Time.posixToMillis posix)


timeDecoder : BD.Decoder Time
timeDecoder =
    BD.map (Time << Time.millisToPosix) BD.int
