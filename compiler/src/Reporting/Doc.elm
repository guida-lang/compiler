module Reporting.Doc exposing
    ( Color(..)
    , Doc
    , a
    , align
    , append
    , args
    , black
    , blue
    , cat
    , commaSep
    , cyan
    , cycle
    , displayIO
    , dullcyan
    , dullred
    , dullyellow
    , empty
    , encode
    , fancyLink
    , fill
    , fillSep
    , fromChars
    , fromInt
    , fromName
    , fromPackage
    , fromVersion
    , green
    ,  hang
       -- , hcat

    , hsep
    , indent
    , intToOrdinal
    , join
    ,  link
       -- , magenta

    , makeLink
    , makeNakedLink
    , moreArgs
    , ordinal
    , plus
    , red
    , reflow
    , reflowLink
    , renderPretty
    , sep
    , stack
    , toAnsi
    , toFancyHint
    , toFancyNote
    , toLine
    , toSimpleHint
    , toSimpleNote
    , toString
    , vcat
    , yellow
    )

import Data.IO as IO exposing (Handle, IO)
import Data.Index as Index
import Data.Name exposing (Name)
import Elm.Package as Pkg
import Elm.Version as V
import Json.EncodeX as E
import List.Extra as List
import Pretty as P
import Pretty.Renderer


type alias Doc =
    P.Doc Style


type alias Style =
    { bold : Bool
    , underline : Bool
    , color : Maybe Color
    }


noStyle : Style
noStyle =
    Style False False Nothing


type Color
    = Red
    | DullRed
    | Yellow
    | DullYellow
    | Green
    | Cyan
    | DullCyan
    | Blue
    | Black


encode : Doc -> E.Value
encode doc =
    let
        _ =
            Debug.log "doc" (Debug.toString (P.pretty 0 doc))
    in
    -- E.array (toJsonHelp noStyle [] (P.renderPretty 1 80 doc))
    E.array []



-- toJsonHelp : Style -> List String -> P.SimpleDoc -> List E.Value
-- toJsonHelp style revChunks simpleDoc =
--     case simpleDoc of
--         P.SFail ->
--             Debug.todo <|
--                 "according to the main implementation, @SFail@ can not appear uncaught in a rendered @SimpleDoc@"
--         P.SEmpty ->
--             [ encodeChunks style revChunks ]
--         P.SChar char rest ->
--             toJsonHelp style ([ char ] :: revChunks) rest
--         P.SText _ string rest ->
--             toJsonHelp style (string :: revChunks) rest
--         P.SLine indent rest ->
--             toJsonHelp style (replicate indent ' ' :: "\n" :: revChunks) rest
--         P.SSGR sgrs rest ->
--             encodeChunks style revChunks :: toJsonHelp (sgrToStyle sgrs style) [] rest
-- DOC


empty : Doc
empty =
    P.empty


align : Doc -> Doc
align =
    P.align


hang : Int -> Doc -> Doc
hang =
    P.hang


fill : Int -> Doc -> Doc
fill _ doc =
    -- TODO fix this
    doc


fillSep : List Doc -> Doc
fillSep =
    P.words


cat : List Doc -> Doc
cat =
    P.join P.empty


sep : List Doc -> Doc
sep =
    P.words


hsep : List Doc -> Doc
hsep =
    P.words


join : Doc -> List Doc -> Doc
join =
    P.join


append : Doc -> Doc -> Doc
append =
    P.append


a : Doc -> Doc -> Doc
a =
    P.a


plus : Doc -> Doc -> Doc
plus doc =
    P.a P.space >> P.a doc


indent : Int -> Doc -> Doc
indent =
    P.indent


black : Doc -> Doc
black =
    P.setTag { noStyle | color = Just Black }


green : Doc -> Doc
green =
    P.setTag { noStyle | color = Just Green }


dullyellow : Doc -> Doc
dullyellow =
    P.setTag { noStyle | color = Just DullYellow }


yellow : Doc -> Doc
yellow =
    P.setTag { noStyle | color = Just Yellow }


red : Doc -> Doc
red =
    P.setTag { noStyle | color = Just Red }


dullred : Doc -> Doc
dullred =
    P.setTag { noStyle | color = Just DullRed }


cyan : Doc -> Doc
cyan =
    P.setTag { noStyle | color = Just Cyan }


dullcyan : Doc -> Doc
dullcyan =
    P.setTag { noStyle | color = Just DullCyan }


blue : Doc -> Doc
blue =
    P.setTag { noStyle | color = Just Blue }


vcat : List Doc -> Doc
vcat =
    P.lines



-- FROM


fromChars : String -> Doc
fromChars =
    P.string


fromName : Name -> Doc
fromName =
    P.string


fromVersion : V.Version -> Doc
fromVersion vsn =
    P.string (V.toChars vsn)


fromPackage : Pkg.Name -> Doc
fromPackage pkg =
    P.string (Pkg.toChars pkg)


fromInt : Int -> Doc
fromInt n =
    P.string (String.fromInt n)



-- TO STRING


toAnsi : Handle -> Doc -> IO ()
toAnsi handle doc =
    IO.hPutStr handle
        (Pretty.Renderer.pretty 80
            { init = ""
            , tagged =
                \{ bold, color, underline } str acc ->
                    let
                        boldChar =
                            if bold then
                                "\u{001B}[1m"

                            else
                                ""

                        colorChar =
                            case color of
                                Just Black ->
                                    "\u{001B}[30m"

                                Just Red ->
                                    "\u{001B}[31m"

                                Just Green ->
                                    "\u{001B}[32m"

                                Just Yellow ->
                                    "\u{001B}[33m"

                                Just Blue ->
                                    "\u{001B}[34m"

                                Just Cyan ->
                                    "\u{001B}[36m"

                                Just DullRed ->
                                    "\u{001B}[91m"

                                Just DullYellow ->
                                    "\u{001B}[93m"

                                Just DullCyan ->
                                    "\u{001B}[96m"

                                Nothing ->
                                    ""

                        underlineChar =
                            if underline then
                                "\u{001B}[4m"

                            else
                                ""
                    in
                    acc ++ boldChar ++ colorChar ++ underlineChar ++ str
            , untagged =
                \str acc ->
                    acc ++ str ++ "\u{001B}[0m"
            , newline =
                \acc ->
                    acc ++ "\n\n"
            , outer = identity
            }
            doc
        )


toString : Doc -> String
toString doc =
    P.pretty 80 doc


toLine : Doc -> String
toLine doc =
    P.pretty (2147483647 // 2) doc



-- FORMATTING


stack : List Doc -> Doc
stack docs =
    P.lines docs


reflow : String -> Doc
reflow paragraph =
    P.words (List.map P.string (String.words paragraph))


commaSep : Doc -> (Doc -> Doc) -> List Doc -> List Doc
commaSep conjunction addStyle names =
    case names of
        [ name ] ->
            [ addStyle name ]

        [ name1, name2 ] ->
            [ addStyle name1, conjunction, addStyle name2 ]

        _ ->
            List.map (addStyle >> P.append (P.char ',')) (List.init names |> Maybe.withDefault [])
                ++ [ conjunction
                   , addStyle (List.last names |> Maybe.withDefault P.empty)
                   ]



-- NOTES


toSimpleNote : String -> Doc
toSimpleNote message =
    toFancyNote (List.map P.string (String.words message))


toFancyNote : List Doc -> Doc
toFancyNote chunks =
    P.words
        ((P.taggedString "Note" { noStyle | underline = True }
            |> P.a (P.char ':')
         )
            :: chunks
        )



-- HINTS


toSimpleHint : String -> Doc
toSimpleHint message =
    toFancyHint (List.map P.string (String.words message))


toFancyHint : List Doc -> Doc
toFancyHint chunks =
    P.words
        ((P.taggedString "Hint" { noStyle | underline = True }
            |> P.a (P.char ':')
         )
            :: chunks
        )



-- LINKS


link : String -> String -> String -> String -> Doc
link word before fileName after =
    P.words
        ((P.taggedString word { noStyle | underline = True }
            |> P.a (P.char ':')
         )
            :: List.map P.string (String.words before)
            ++ P.string (makeLink fileName)
            :: List.map P.string (String.words after)
        )


fancyLink : String -> List Doc -> String -> List Doc -> Doc
fancyLink word before fileName after =
    P.words
        ((P.taggedString word { noStyle | underline = True }
            |> P.a (P.char ':')
         )
            :: before
            ++ P.string (makeLink fileName)
            :: after
        )


makeLink : String -> String
makeLink fileName =
    "<https://elm-lang.org/" ++ V.toChars V.compiler ++ "/" ++ fileName ++ ">"


makeNakedLink : String -> String
makeNakedLink fileName =
    "https://elm-lang.org/" ++ V.toChars V.compiler ++ "/" ++ fileName


reflowLink : String -> String -> String -> Doc
reflowLink before fileName after =
    P.words
        (List.map P.string (String.words before)
            ++ P.string (makeLink fileName)
            :: List.map P.string (String.words after)
        )



-- HELPERS


args : Int -> String
args n =
    String.fromInt n
        ++ (if n == 1 then
                " argument"

            else
                " arguments"
           )


moreArgs : Int -> String
moreArgs n =
    String.fromInt n
        ++ " more"
        ++ (if n == 1 then
                " argument"

            else
                " arguments"
           )


ordinal : Index.ZeroBased -> String
ordinal index =
    intToOrdinal (Index.toHuman index)


intToOrdinal : Int -> String
intToOrdinal number =
    let
        remainder10 =
            modBy 10 number

        remainder100 =
            modBy 100 number

        ending =
            if List.member remainder100 [ 11, 12, 13 ] then
                "th"

            else if remainder10 == 1 then
                "st"

            else if remainder10 == 2 then
                "nd"

            else if remainder10 == 3 then
                "rd"

            else
                "th"
    in
    String.fromInt number ++ ending


cycle : Int -> Name -> List Name -> Doc
cycle indent_ name names =
    let
        toLn n =
            cycleLn
                |> P.setTag { noStyle | color = Just Yellow }
                |> P.a (fromName n)
    in
    P.indent indent_
        (P.lines
            (cycleTop
                :: List.intersperse cycleMid (toLn name :: List.map toLn names)
                ++ [ cycleEnd ]
            )
        )


cycleTop : Doc
cycleTop =
    P.string "┌─────┐"


cycleLn : Doc
cycleLn =
    P.string "│    "


cycleMid : Doc
cycleMid =
    P.string "│     ↓"


cycleEnd : Doc
cycleEnd =
    P.string "└─────┘"


displayIO : Handle -> Doc -> IO ()
displayIO handle doc =
    IO.hPutStr handle (P.pretty 80 doc)


renderPretty : Float -> Int -> Doc -> Doc
renderPretty _ _ _ =
    Debug.todo "renderPretty"
