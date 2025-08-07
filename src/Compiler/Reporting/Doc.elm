module Compiler.Reporting.Doc exposing
    ( Doc
    , plus, append, a
    , align, cat, empty, fill, fillSep, hang
    , hcat, hsep, indent, sep, vcat
    , Color(..)
    , red, cyan, green, blue, black, yellow
    , dullred, dullcyan, dullyellow
    , fromChars, fromName, fromVersion, fromPackage, fromInt
    , toAnsi, toString, toLine
    , encode
    , stack, reflow, commaSep
    , toSimpleNote, toFancyNote, toSimpleHint, toFancyHint
    , link, fancyLink, reflowLink, makeLink, makeNakedLink
    , args, moreArgs, ordinal, intToOrdinal, cycle
    )

{-|

@docs Doc
@docs plus, append, a
@docs align, cat, empty, fill, fillSep, hang
@docs hcat, hsep, indent, sep, vcat
@docs Color
@docs red, cyan, green, blue, black, yellow
@docs dullred, dullcyan, dullyellow
@docs fromChars, fromName, fromVersion, fromPackage, fromInt
@docs toAnsi, toString, toLine
@docs encode
@docs stack, reflow, commaSep
@docs toSimpleNote, toFancyNote, toSimpleHint, toFancyHint
@docs link, fancyLink, reflowLink, makeLink, makeNakedLink
@docs args, moreArgs, ordinal, intToOrdinal, cycle

-}

import Compiler.Data.Index as Index
import Compiler.Data.Name exposing (Name)
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Json.Encode as E
import Maybe.Extra as Maybe
import Prelude
import System.Console.Ansi as Ansi
import System.IO exposing (Handle)
import Task exposing (Task)
import Text.PrettyPrint.ANSI.Leijen as P



-- FROM


fromChars : String -> Doc
fromChars =
    P.text


fromName : Name -> Doc
fromName =
    P.text


fromVersion : V.Version -> Doc
fromVersion vsn =
    P.text (V.toChars vsn)


fromPackage : Pkg.Name -> Doc
fromPackage pkg =
    P.text (Pkg.toChars pkg)


fromInt : Int -> Doc
fromInt n =
    P.text (String.fromInt n)



-- TO STRING


toAnsi : Handle -> Doc -> Task Never ()
toAnsi handle doc =
    P.displayIO handle (P.renderPretty 1 80 doc)


toString : Doc -> String
toString doc =
    P.displayS (P.renderPretty 1 80 (P.plain doc)) ""


toLine : Doc -> String
toLine doc =
    let
        maxBound : number
        maxBound =
            2147483647
    in
    P.displayS (P.renderPretty 1 (maxBound // 2) (P.plain doc)) ""



-- FORMATTING


stack : List Doc -> Doc
stack docs =
    P.vcat (List.intersperse (P.text "") docs)


reflow : String -> Doc
reflow paragraph =
    P.fillSep (List.map P.text (String.words paragraph))


commaSep : Doc -> (Doc -> Doc) -> List Doc -> List Doc
commaSep conjunction addStyle names =
    case names of
        [ name ] ->
            [ addStyle name ]

        [ name1, name2 ] ->
            [ addStyle name1, conjunction, addStyle name2 ]

        _ ->
            List.map (\name -> P.append (addStyle name) (P.text ",")) (Prelude.init names)
                ++ [ conjunction
                   , addStyle (Prelude.last names)
                   ]



-- NOTES


toSimpleNote : String -> Doc
toSimpleNote message =
    toFancyNote (List.map P.text (String.words message))


toFancyNote : List Doc -> Doc
toFancyNote chunks =
    P.fillSep (P.append (P.underline (P.text "Note")) (P.text ":") :: chunks)



-- HINTS


toSimpleHint : String -> Doc
toSimpleHint message =
    toFancyHint (List.map P.text (String.words message))


toFancyHint : List Doc -> Doc
toFancyHint chunks =
    P.fillSep (P.append (P.underline (P.text "Hint")) (P.text ":") :: chunks)



-- LINKS


link : String -> String -> String -> String -> Doc
link word before fileName after =
    P.fillSep <|
        P.append (P.underline (P.text word)) (P.text ":")
            :: List.map P.text (String.words before)
            ++ P.text (makeLink fileName)
            :: List.map P.text (String.words after)


fancyLink : String -> List Doc -> String -> List Doc -> Doc
fancyLink word before fileName after =
    P.fillSep <|
        P.append (P.underline (P.text word)) (P.text ":")
            :: before
            ++ P.text (makeLink fileName)
            :: after


makeLink : String -> String
makeLink fileName =
    "<" ++ makeNakedLink fileName ++ ">"


makeNakedLink : String -> String
makeNakedLink fileName =
    "https://elm-lang.org/" ++ V.toChars V.elmCompiler ++ "/" ++ fileName


reflowLink : String -> String -> String -> Doc
reflowLink before fileName after =
    P.fillSep <|
        List.map P.text (String.words before)
            ++ P.text (makeLink fileName)
            :: List.map P.text (String.words after)



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
        remainder100 : Int
        remainder100 =
            modBy 100 number

        ending : String
        ending =
            if List.member remainder100 [ 11, 12, 13 ] then
                "th"

            else
                let
                    remainder10 : Int
                    remainder10 =
                        modBy 10 number
                in
                if remainder10 == 1 then
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
        toLn : Name -> P.Doc
        toLn n =
            P.append cycleLn (P.dullyellow (fromName n))
    in
    P.indent indent_ <|
        P.vcat <|
            cycleTop
                :: List.intersperse cycleMid (toLn name :: List.map toLn names)
                ++ [ cycleEnd ]


cycleTop : Doc
cycleTop =
    if isWindows then
        P.text "+-----+"

    else
        P.text "┌─────┐"


cycleLn : Doc
cycleLn =
    if isWindows then
        P.text "|    "

    else
        P.text "│    "


cycleMid : Doc
cycleMid =
    if isWindows then
        P.text "|     |"

    else
        P.text "│     ↓"


cycleEnd : Doc
cycleEnd =
    if isWindows then
        P.text "+-<---+"

    else
        P.text "└─────┘"


isWindows : Bool
isWindows =
    -- Info.os == "mingw32"
    False



-- JSON


encode : Doc -> E.Value
encode doc =
    E.array (toJsonHelp noStyle [] (P.renderPretty 1 80 doc))


type Style
    = Style Bool Bool (Maybe Color)


noStyle : Style
noStyle =
    Style False False Nothing


type Color
    = Red
    | RED
    | Magenta
    | MAGENTA
    | Yellow
    | YELLOW
    | Green
    | GREEN
    | Cyan
    | CYAN
    | Blue
    | BLUE
    | Black
    | BLACK
    | White
    | WHITE


toJsonHelp : Style -> List String -> P.SimpleDoc -> List E.Value
toJsonHelp style revChunks simpleDoc =
    case simpleDoc of
        P.SEmpty ->
            [ encodeChunks style revChunks ]

        P.SText string rest ->
            toJsonHelp style (string :: revChunks) rest

        P.SLine indent_ rest ->
            toJsonHelp style (String.repeat indent_ " " :: "\n" :: revChunks) rest

        P.SSGR sgrs rest ->
            encodeChunks style revChunks :: toJsonHelp (sgrToStyle sgrs style) [] rest


sgrToStyle : List Ansi.SGR -> Style -> Style
sgrToStyle sgrs ((Style bold underline color) as style) =
    case sgrs of
        [] ->
            style

        sgr :: rest ->
            sgrToStyle rest <|
                case sgr of
                    Ansi.Reset ->
                        noStyle

                    Ansi.SetConsoleIntensity i ->
                        Style (isBold i) underline color

                    Ansi.SetItalicized _ ->
                        style

                    Ansi.SetUnderlining u ->
                        Style bold (isUnderline u) color

                    Ansi.SetBlinkSpeed _ ->
                        style

                    Ansi.SetVisible _ ->
                        style

                    Ansi.SetSwapForegroundBackground _ ->
                        style

                    Ansi.SetColor l i c ->
                        Style bold underline (toColor l i c)


isBold : Ansi.ConsoleIntensity -> Bool
isBold intensity =
    case intensity of
        Ansi.BoldIntensity ->
            True

        Ansi.FaintIntensity ->
            False

        Ansi.NormalIntensity ->
            False


isUnderline : Ansi.Underlining -> Bool
isUnderline underlining =
    case underlining of
        Ansi.SingleUnderline ->
            True

        Ansi.DoubleUnderline ->
            False

        Ansi.NoUnderline ->
            False


toColor : Ansi.ConsoleLayer -> Ansi.ColorIntensity -> Ansi.Color -> Maybe Color
toColor layer intensity color =
    case layer of
        Ansi.Background ->
            Nothing

        Ansi.Foreground ->
            let
                pick : b -> b -> b
                pick dull vivid =
                    case intensity of
                        Ansi.Dull ->
                            dull

                        Ansi.Vivid ->
                            vivid
            in
            Just <|
                case color of
                    Ansi.Red ->
                        pick Red RED

                    Ansi.Magenta ->
                        pick Magenta MAGENTA

                    Ansi.Yellow ->
                        pick Yellow YELLOW

                    Ansi.Green ->
                        pick Green GREEN

                    Ansi.Cyan ->
                        pick Cyan CYAN

                    Ansi.Blue ->
                        pick Blue BLUE

                    Ansi.White ->
                        pick White WHITE

                    Ansi.Black ->
                        pick Black BLACK


encodeChunks : Style -> List String -> E.Value
encodeChunks (Style bold underline color) revChunks =
    let
        chars : String
        chars =
            String.concat (List.reverse revChunks)
    in
    case ( color, not bold && not underline ) of
        ( Nothing, True ) ->
            E.chars chars

        _ ->
            E.object
                [ ( "bold", E.bool bold )
                , ( "underline", E.bool underline )
                , ( "color", Maybe.unwrap E.null encodeColor color )
                , ( "string", E.chars chars )
                ]


encodeColor : Color -> E.Value
encodeColor color =
    E.string <|
        case color of
            Red ->
                "red"

            RED ->
                "RED"

            Magenta ->
                "magenta"

            MAGENTA ->
                "MAGENTA"

            Yellow ->
                "yellow"

            YELLOW ->
                "YELLOW"

            Green ->
                "green"

            GREEN ->
                "GREEN"

            Cyan ->
                "cyan"

            CYAN ->
                "CYAN"

            Blue ->
                "blue"

            BLUE ->
                "BLUE"

            Black ->
                "black"

            BLACK ->
                "BLACK"

            White ->
                "white"

            WHITE ->
                "WHITE"



-- DOC


type alias Doc =
    P.Doc


a : Doc -> Doc -> Doc
a =
    P.a


plus : Doc -> Doc -> Doc
plus =
    P.plus


append : Doc -> Doc -> Doc
append =
    P.append


align : Doc -> Doc
align =
    P.align


cat : List Doc -> Doc
cat =
    P.cat


empty : Doc
empty =
    P.empty


fill : Int -> Doc -> Doc
fill =
    P.fill


fillSep : List Doc -> Doc
fillSep =
    P.fillSep


hang : Int -> Doc -> Doc
hang =
    P.hang


hcat : List Doc -> Doc
hcat =
    P.hcat


hsep : List Doc -> Doc
hsep =
    P.hsep


indent : Int -> Doc -> Doc
indent =
    P.indent


sep : List Doc -> Doc
sep =
    P.sep


vcat : List Doc -> Doc
vcat =
    P.vcat


red : Doc -> Doc
red =
    P.red


cyan : Doc -> Doc
cyan =
    P.cyan


green : Doc -> Doc
green =
    P.green


blue : Doc -> Doc
blue =
    P.blue


black : Doc -> Doc
black =
    P.black


yellow : Doc -> Doc
yellow =
    P.yellow


dullred : Doc -> Doc
dullred =
    P.dullred


dullcyan : Doc -> Doc
dullcyan =
    P.dullcyan


dullyellow : Doc -> Doc
dullyellow =
    P.dullyellow
