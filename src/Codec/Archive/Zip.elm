module Codec.Archive.Zip exposing
    ( Archive
    , Entry
    , FilePath
    , eRelativePath
    , fromEntry
    , zEntries
    )

{-| The module provides everything you may need to manipulate Zip archives.
There are three things that should be clarified right away, to avoid confusion.

Ref.: <https://hackage.haskell.org/package/zip-2.1.0/docs/Codec-Archive-Zip.html>

-}


{-| FIXME System.IO.FilePath
-}
type alias FilePath =
    String


type alias Archive =
    List Entry


type alias Entry =
    { eRelativePath : FilePath
    , eData : String
    }


zEntries : Archive -> List Entry
zEntries =
    identity


eRelativePath : Entry -> FilePath
eRelativePath zipEntry =
    zipEntry.eRelativePath


fromEntry : Entry -> String
fromEntry zipEntry =
    zipEntry.eData
