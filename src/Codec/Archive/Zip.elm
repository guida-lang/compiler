module Codec.Archive.Zip exposing
    ( eRelativePath
    , fromEntry
    , zEntries
    )

{-| The module provides everything you may need to manipulate Zip archives.
There are three things that should be clarified right away, to avoid confusion.

Ref.: <https://hackage.haskell.org/package/zip-2.1.0/docs/Codec-Archive-Zip.html>

-}

import Types as T


zEntries : T.CAZ_Archive -> List T.CAZ_Entry
zEntries =
    identity


eRelativePath : T.CAZ_Entry -> T.FilePath
eRelativePath zipEntry =
    zipEntry.eRelativePath


fromEntry : T.CAZ_Entry -> String
fromEntry zipEntry =
    zipEntry.eData
