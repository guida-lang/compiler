module Common.Format.Cheapskate.Util exposing
    ( Scanner
    , isEscapable
    , isWhitespace
    , joinLines
    , nfb
    , nfbChar
    , normalizeReference
    , scanBlankline
    , scanChar
    , scanIndentSpace
    , scanNonindentSpace
    , scanSpaces
    , scanSpacesToColumn
    , scanSpnl
    , tabFilter
    , upToCountChars
    )

import Common.Format.Cheapskate.ParserCombinators exposing (..)



-- Utility functions.


{-| Like T.unlines but does not add a final newline.
Concatenates lines with newlines between.
-}
joinLines : List String -> String
joinLines =
    String.join "\n"


{-| Convert tabs to spaces using a 4-space tab stop.
-}
tabFilter : String -> String
tabFilter =
    let
        pad value =
            case value of
                [] ->
                    []

                [ t ] ->
                    [ t ]

                t :: ts ->
                    let
                        tl =
                            String.length t

                        n =
                            tl + 4 - modBy 4 tl
                    in
                    String.padRight n ' ' t :: pad ts
    in
    String.concat << pad << String.split "\t"


{-| These are the whitespace characters that are significant in
parsing markdown. We can treat \\160 (nonbreaking space) etc.
as regular characters. This function should be considerably
faster than the unicode-aware isSpace from Data.Char.
-}
isWhitespace : Char -> Bool
isWhitespace c =
    case c of
        ' ' ->
            True

        '\t' ->
            True

        '\n' ->
            True

        '\u{000D}' ->
            True

        _ ->
            False


{-| The original Markdown only allowed certain symbols
to be backslash-escaped. It was hard to remember
which ones could be, so we now allow any ascii punctuation mark or
symbol to be escaped, whether or not it has a use in Markdown.
-}
isEscapable : Char -> Bool
isEscapable c =
    isAscii c && (isSymbol c || isPunctuation c)


{-| Link references are case sensitive and ignore line breaks
and repeated spaces.
-}
normalizeReference : String -> String
normalizeReference =
    -- T.toCaseFold << T.concat << T.split isWhitespace
    Debug.todo "normalizeReference"


{-| Scanners are implemented here as attoparsec parsers,
which consume input and capture nothing. They could easily
be implemented as regexes in other languages, or hand-coded.
With the exception of scanSpnl, they are all intended to
operate on a single line of input (so endOfInput = endOfLine).
-}
type alias Scanner =
    Parser ()


{-| Scan four spaces.
-}
scanIndentSpace : Scanner
scanIndentSpace =
    fmap (\_ -> ()) (count 4 (skip ((==) ' ')))


scanSpacesToColumn : Int -> Scanner
scanSpacesToColumn col =
    -- do
    --   currentCol <- column <$> getPosition
    --   case col - currentCol of
    --        n | n >= 1 -> () <$ (count n (skip (==' ')))
    --          | otherwise -> return ()
    Debug.todo "scanSpacesToColumn"


{-| Scan 0-3 spaces.
-}
scanNonindentSpace : Scanner
scanNonindentSpace =
    fmap (\_ -> ()) (upToCountChars 3 ((==) ' '))


{-| Scan a specified character.
-}
scanChar : Char -> Scanner
scanChar c =
    skip ((==) c) |> bind (\_ -> return ())


{-| Scan a blankline.
-}
scanBlankline : Scanner
scanBlankline =
    scanSpaces |> bind (\_ -> endOfInput)


{-| Scan 0 or more spaces
-}
scanSpaces : Scanner
scanSpaces =
    skipWhile ((==) ' ')


{-| Scan 0 or more spaces, and optionally a newline
and more spaces.
-}
scanSpnl : Scanner
scanSpnl =
    scanSpaces |> bind (\_ -> option () (char '\n' |> bind (\_ -> scanSpaces)))


{-| Not followed by: Succeed without consuming input if the specified
scanner would not succeed.
-}
nfb : Parser a -> Scanner
nfb =
    notFollowedBy


{-| Succeed if not followed by a character. Consumes no input.
-}
nfbChar : Char -> Scanner
nfbChar c =
    nfb (skip ((==) c))


upToCountChars : Int -> (Char -> Bool) -> Parser String
upToCountChars cnt f =
    scan 0
        (\n c ->
            if n < cnt && f c then
                Just (n + 1)

            else
                Nothing
        )


{-| Selects the first 128 characters of the Unicode character set,
corresponding to the ASCII character set.
-}
isAscii : Char -> Bool
isAscii c =
    c < '\u{0080}'


isSymbol : Char -> Bool
isSymbol c =
    Debug.todo "isSymbol"


isPunctuation : Char -> Bool
isPunctuation c =
    Debug.todo "isPunctuation"
