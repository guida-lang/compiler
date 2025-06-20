module Common.Format.KnownContents exposing
    ( KnownContents
    , fromFunction
    , get
    , isKnown
    , mempty
    )

import Maybe.Extra as Maybe


type KnownContents
    = KnownContents (String -> Maybe (List String)) -- return Nothing if the contents are unknown



-- instance Semigroup KnownContents where
--     (KnownContents a) <> (KnownContents b) = KnownContents (\ns -> a ns <> b ns)


mempty : KnownContents
mempty =
    fromFunction (always Nothing)


fromFunction : (String -> Maybe (List String)) -> KnownContents
fromFunction =
    KnownContents


isKnown : KnownContents -> String -> Bool
isKnown (KnownContents lookup) =
    Maybe.unwrap False (always True) << lookup


get : String -> KnownContents -> Maybe (List String)
get ns (KnownContents lookup) =
    lookup ns
