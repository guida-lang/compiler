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

import Common.Format.Cheapskate.ParserCombinators exposing (Parser, bind, char, column, count, endOfInput, fmap, getPosition, notFollowedBy, option, return, scan, skip, skipWhile)
import List.Extra as List
import Utils.Crash exposing (crash)



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
        pad : List String -> List String
        pad value =
            case value of
                [] ->
                    []

                [ t ] ->
                    [ t ]

                t :: ts ->
                    let
                        tl : Int
                        tl =
                            String.length t

                        n : Int
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
    String.toLower << String.concat << split isWhitespace


span_ : (Char -> Bool) -> String -> ( String, String )
span_ p t =
    List.splitWhen p (String.toList t)
        |> Maybe.map (Tuple.mapBoth String.fromList String.fromList)
        |> Maybe.withDefault ( t, "" )


split : (Char -> Bool) -> String -> List String
split p t =
    if String.isEmpty t then
        [ "" ]

    else
        let
            loop : String -> List String
            loop s =
                let
                    ( l, s_ ) =
                        span_ (not << p) s
                in
                if String.isEmpty s_ then
                    [ l ]

                else
                    let
                        s__ : String
                        s__ =
                            case String.uncons s_ of
                                Just ( _, tail_ ) ->
                                    tail_

                                _ ->
                                    crash "unsafeTail"
                    in
                    l :: loop s__
        in
        loop t


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
    getPosition
        |> fmap column
        |> bind
            (\currentCol ->
                let
                    n : Int
                    n =
                        col - currentCol
                in
                if n >= 1 then
                    count n (skip ((==) ' '))
                        |> fmap (\_ -> ())

                else
                    return ()
            )


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
    String.contains (String.fromChar c) "+-/*=.<>:&|^?%!"


{-| Selects Unicode punctuation characters, including various kinds
of connectors, brackets and quotes.

This function returns 'True' if its argument has one of the
following 'GeneralCategory's, or 'False' otherwise:

  - 'ConnectorPunctuation'
  - 'DashPunctuation'
  - 'OpenPunctuation'
  - 'ClosePunctuation'
  - 'InitialQuote'
  - 'FinalQuote'
  - 'OtherPunctuation'

These classes are defined in the
<<http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table> Unicode Character Database>,
part of the Unicode standard. The same document defines what is
and is not a "Punctuation".

-}
isPunctuation : Char -> Bool
isPunctuation c =
    List.member (Char.toCode c)
        [ 0x21
        , 0x22
        , 0x23
        , 0x25
        , 0x26
        , 0x27
        , 0x28
        , 0x29
        , 0x2A
        , 0x2C
        , 0x2D
        , 0x2E
        , 0x2F
        , 0x3A
        , 0x3B
        , 0x3F
        , 0x40
        , 0x5B
        , 0x5C
        , 0x5D
        , 0x5F
        , 0x7B
        , 0x7D
        , 0xA1
        , 0xA7
        , 0xAB
        , 0xB6
        , 0xB7
        , 0xBB
        , 0xBF
        , 0x037E
        , 0x0387
        , 0x055A
        , 0x055B
        , 0x055C
        , 0x055D
        , 0x055E
        , 0x055F
        , 0x0589
        , 0x058A
        , 0x05BE
        , 0x05C0
        , 0x05C3
        , 0x05C6
        , 0x05F3
        , 0x05F4
        , 0x0609
        , 0x060A
        , 0x060C
        , 0x060D
        , 0x061B
        , 0x061E
        , 0x061F
        , 0x066A
        , 0x066B
        , 0x066C
        , 0x066D
        , 0x06D4
        , 0x0700
        , 0x0701
        , 0x0702
        , 0x0703
        , 0x0704
        , 0x0705
        , 0x0706
        , 0x0707
        , 0x0708
        , 0x0709
        , 0x070A
        , 0x070B
        , 0x070C
        , 0x070D
        , 0x07F7
        , 0x07F8
        , 0x07F9
        , 0x0830
        , 0x0831
        , 0x0832
        , 0x0833
        , 0x0834
        , 0x0835
        , 0x0836
        , 0x0837
        , 0x0838
        , 0x0839
        , 0x083A
        , 0x083B
        , 0x083C
        , 0x083D
        , 0x083E
        , 0x085E
        , 0x0964
        , 0x0965
        , 0x0970
        , 0x09FD
        , 0x0A76
        , 0x0AF0
        , 0x0C77
        , 0x0C84
        , 0x0DF4
        , 0x0E4F
        , 0x0E5A
        , 0x0E5B
        , 0x0F04
        , 0x0F05
        , 0x0F06
        , 0x0F07
        , 0x0F08
        , 0x0F09
        , 0x0F0A
        , 0x0F0B
        , 0x0F0C
        , 0x0F0D
        , 0x0F0E
        , 0x0F0F
        , 0x0F10
        , 0x0F11
        , 0x0F12
        , 0x0F14
        , 0x0F3A
        , 0x0F3B
        , 0x0F3C
        , 0x0F3D
        , 0x0F85
        , 0x0FD0
        , 0x0FD1
        , 0x0FD2
        , 0x0FD3
        , 0x0FD4
        , 0x0FD9
        , 0x0FDA
        , 0x104A
        , 0x104B
        , 0x104C
        , 0x104D
        , 0x104E
        , 0x104F
        , 0x10FB
        , 0x1360
        , 0x1361
        , 0x1362
        , 0x1363
        , 0x1364
        , 0x1365
        , 0x1366
        , 0x1367
        , 0x1368
        , 0x1400
        , 0x166E
        , 0x169B
        , 0x169C
        , 0x16EB
        , 0x16EC
        , 0x16ED
        , 0x1735
        , 0x1736
        , 0x17D4
        , 0x17D5
        , 0x17D6
        , 0x17D8
        , 0x17D9
        , 0x17DA
        , 0x1800
        , 0x1801
        , 0x1802
        , 0x1803
        , 0x1804
        , 0x1805
        , 0x1806
        , 0x1807
        , 0x1808
        , 0x1809
        , 0x180A
        , 0x1944
        , 0x1945
        , 0x1A1E
        , 0x1A1F
        , 0x1AA0
        , 0x1AA1
        , 0x1AA2
        , 0x1AA3
        , 0x1AA4
        , 0x1AA5
        , 0x1AA6
        , 0x1AA8
        , 0x1AA9
        , 0x1AAA
        , 0x1AAB
        , 0x1AAC
        , 0x1AAD
        , 0x1B5A
        , 0x1B5B
        , 0x1B5C
        , 0x1B5D
        , 0x1B5E
        , 0x1B5F
        , 0x1B60
        , 0x1BFC
        , 0x1BFD
        , 0x1BFE
        , 0x1BFF
        , 0x1C3B
        , 0x1C3C
        , 0x1C3D
        , 0x1C3E
        , 0x1C3F
        , 0x1C7E
        , 0x1C7F
        , 0x1CC0
        , 0x1CC1
        , 0x1CC2
        , 0x1CC3
        , 0x1CC4
        , 0x1CC5
        , 0x1CC6
        , 0x1CC7
        , 0x1CD3
        , 0x2010
        , 0x2011
        , 0x2012
        , 0x2013
        , 0x2014
        , 0x2015
        , 0x2016
        , 0x2017
        , 0x2018
        , 0x2019
        , 0x201A
        , 0x201B
        , 0x201C
        , 0x201D
        , 0x201E
        , 0x201F
        , 0x2020
        , 0x2021
        , 0x2022
        , 0x2023
        , 0x2024
        , 0x2025
        , 0x2026
        , 0x2027
        , 0x2030
        , 0x2031
        , 0x2032
        , 0x2033
        , 0x2034
        , 0x2035
        , 0x2036
        , 0x2037
        , 0x2038
        , 0x2039
        , 0x203A
        , 0x203B
        , 0x203C
        , 0x203D
        , 0x203E
        , 0x203F
        , 0x2040
        , 0x2041
        , 0x2042
        , 0x2043
        , 0x2045
        , 0x2046
        , 0x2047
        , 0x2048
        , 0x2049
        , 0x204A
        , 0x204B
        , 0x204C
        , 0x204D
        , 0x204E
        , 0x204F
        , 0x2050
        , 0x2051
        , 0x2053
        , 0x2054
        , 0x2055
        , 0x2056
        , 0x2057
        , 0x2058
        , 0x2059
        , 0x205A
        , 0x205B
        , 0x205C
        , 0x205D
        , 0x205E
        , 0x207D
        , 0x207E
        , 0x208D
        , 0x208E
        , 0x2308
        , 0x2309
        , 0x230A
        , 0x230B
        , 0x2329
        , 0x232A
        , 0x2768
        , 0x2769
        , 0x276A
        , 0x276B
        , 0x276C
        , 0x276D
        , 0x276E
        , 0x276F
        , 0x2770
        , 0x2771
        , 0x2772
        , 0x2773
        , 0x2774
        , 0x2775
        , 0x27C5
        , 0x27C6
        , 0x27E6
        , 0x27E7
        , 0x27E8
        , 0x27E9
        , 0x27EA
        , 0x27EB
        , 0x27EC
        , 0x27ED
        , 0x27EE
        , 0x27EF
        , 0x2983
        , 0x2984
        , 0x2985
        , 0x2986
        , 0x2987
        , 0x2988
        , 0x2989
        , 0x298A
        , 0x298B
        , 0x298C
        , 0x298D
        , 0x298E
        , 0x298F
        , 0x2990
        , 0x2991
        , 0x2992
        , 0x2993
        , 0x2994
        , 0x2995
        , 0x2996
        , 0x2997
        , 0x2998
        , 0x29D8
        , 0x29D9
        , 0x29DA
        , 0x29DB
        , 0x29FC
        , 0x29FD
        , 0x2CF9
        , 0x2CFA
        , 0x2CFB
        , 0x2CFC
        , 0x2CFE
        , 0x2CFF
        , 0x2D70
        , 0x2E00
        , 0x2E01
        , 0x2E02
        , 0x2E03
        , 0x2E04
        , 0x2E05
        , 0x2E06
        , 0x2E07
        , 0x2E08
        , 0x2E09
        , 0x2E0A
        , 0x2E0B
        , 0x2E0C
        , 0x2E0D
        , 0x2E0E
        , 0x2E0F
        , 0x2E10
        , 0x2E11
        , 0x2E12
        , 0x2E13
        , 0x2E14
        , 0x2E15
        , 0x2E16
        , 0x2E17
        , 0x2E18
        , 0x2E19
        , 0x2E1A
        , 0x2E1B
        , 0x2E1C
        , 0x2E1D
        , 0x2E1E
        , 0x2E1F
        , 0x2E20
        , 0x2E21
        , 0x2E22
        , 0x2E23
        , 0x2E24
        , 0x2E25
        , 0x2E26
        , 0x2E27
        , 0x2E28
        , 0x2E29
        , 0x2E2A
        , 0x2E2B
        , 0x2E2C
        , 0x2E2D
        , 0x2E2E
        , 0x2E30
        , 0x2E31
        , 0x2E32
        , 0x2E33
        , 0x2E34
        , 0x2E35
        , 0x2E36
        , 0x2E37
        , 0x2E38
        , 0x2E39
        , 0x2E3A
        , 0x2E3B
        , 0x2E3C
        , 0x2E3D
        , 0x2E3E
        , 0x2E3F
        , 0x2E40
        , 0x2E41
        , 0x2E42
        , 0x2E43
        , 0x2E44
        , 0x2E45
        , 0x2E46
        , 0x2E47
        , 0x2E48
        , 0x2E49
        , 0x2E4A
        , 0x2E4B
        , 0x2E4C
        , 0x2E4D
        , 0x2E4E
        , 0x2E4F
        , 0x3001
        , 0x3002
        , 0x3003
        , 0x3008
        , 0x3009
        , 0x300A
        , 0x300B
        , 0x300C
        , 0x300D
        , 0x300E
        , 0x300F
        , 0x3010
        , 0x3011
        , 0x3014
        , 0x3015
        , 0x3016
        , 0x3017
        , 0x3018
        , 0x3019
        , 0x301A
        , 0x301B
        , 0x301C
        , 0x301D
        , 0x301E
        , 0x301F
        , 0x3030
        , 0x303D
        , 0x30A0
        , 0x30FB
        , 0xA4FE
        , 0xA4FF
        , 0xA60D
        , 0xA60E
        , 0xA60F
        , 0xA673
        , 0xA67E
        , 0xA6F2
        , 0xA6F3
        , 0xA6F4
        , 0xA6F5
        , 0xA6F6
        , 0xA6F7
        , 0xA874
        , 0xA875
        , 0xA876
        , 0xA877
        , 0xA8CE
        , 0xA8CF
        , 0xA8F8
        , 0xA8F9
        , 0xA8FA
        , 0xA8FC
        , 0xA92E
        , 0xA92F
        , 0xA95F
        , 0xA9C1
        , 0xA9C2
        , 0xA9C3
        , 0xA9C4
        , 0xA9C5
        , 0xA9C6
        , 0xA9C7
        , 0xA9C8
        , 0xA9C9
        , 0xA9CA
        , 0xA9CB
        , 0xA9CC
        , 0xA9CD
        , 0xA9DE
        , 0xA9DF
        , 0xAA5C
        , 0xAA5D
        , 0xAA5E
        , 0xAA5F
        , 0xAADE
        , 0xAADF
        , 0xAAF0
        , 0xAAF1
        , 0xABEB
        , 0xFD3E
        , 0xFD3F
        , 0xFE10
        , 0xFE11
        , 0xFE12
        , 0xFE13
        , 0xFE14
        , 0xFE15
        , 0xFE16
        , 0xFE17
        , 0xFE18
        , 0xFE19
        , 0xFE30
        , 0xFE31
        , 0xFE32
        , 0xFE33
        , 0xFE34
        , 0xFE35
        , 0xFE36
        , 0xFE37
        , 0xFE38
        , 0xFE39
        , 0xFE3A
        , 0xFE3B
        , 0xFE3C
        , 0xFE3D
        , 0xFE3E
        , 0xFE3F
        , 0xFE40
        , 0xFE41
        , 0xFE42
        , 0xFE43
        , 0xFE44
        , 0xFE45
        , 0xFE46
        , 0xFE47
        , 0xFE48
        , 0xFE49
        , 0xFE4A
        , 0xFE4B
        , 0xFE4C
        , 0xFE4D
        , 0xFE4E
        , 0xFE4F
        , 0xFE50
        , 0xFE51
        , 0xFE52
        , 0xFE54
        , 0xFE55
        , 0xFE56
        , 0xFE57
        , 0xFE58
        , 0xFE59
        , 0xFE5A
        , 0xFE5B
        , 0xFE5C
        , 0xFE5D
        , 0xFE5E
        , 0xFE5F
        , 0xFE60
        , 0xFE61
        , 0xFE63
        , 0xFE68
        , 0xFE6A
        , 0xFE6B
        , 0xFF01
        , 0xFF02
        , 0xFF03
        , 0xFF05
        , 0xFF06
        , 0xFF07
        , 0xFF08
        , 0xFF09
        , 0xFF0A
        , 0xFF0C
        , 0xFF0D
        , 0xFF0E
        , 0xFF0F
        , 0xFF1A
        , 0xFF1B
        , 0xFF1F
        , 0xFF20
        , 0xFF3B
        , 0xFF3C
        , 0xFF3D
        , 0xFF3F
        , 0xFF5B
        , 0xFF5D
        , 0xFF5F
        , 0xFF60
        , 0xFF61
        , 0xFF62
        , 0xFF63
        , 0xFF64
        , 0xFF65
        ]
