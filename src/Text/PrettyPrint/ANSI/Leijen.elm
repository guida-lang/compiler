module Text.PrettyPrint.ANSI.Leijen exposing
    ( Color(..)
    , Doc
    , SimpleDoc(..)
    , Style
    , a
    , align
    , append
    , black
    , blue
    , cat
    , cyan
    , displayIO
    , displayS
    , dullcyan
    , dullred
    , dullyellow
    , empty
    , fill
    , fillSep
    , green
    , hang
    , hcat
    , hsep
    , indent
    , magenta
    , plain
    , plus
    , red
    , renderPretty
    , sep
    , text
    , underline
    , vcat
    , yellow
    )

import Pretty as P
import Pretty.Renderer as PR
import System.Console.Ansi as Ansi
import System.IO as IO
import Task exposing (Task)


type alias Doc =
    P.Doc Style


type SimpleDoc
    = SEmpty
    | SText String SimpleDoc
    | SLine Int SimpleDoc
    | SSGR (List Ansi.SGR) SimpleDoc


displayIO : IO.Handle -> SimpleDoc -> Task Never ()
displayIO handle simpleDoc =
    IO.hPutStr handle (displayS simpleDoc "")


renderPretty : Float -> Int -> Doc -> SimpleDoc
renderPretty _ w doc =
    PR.pretty w
        { init = { styled = False, newline = False, list = [] }
        , tagged =
            \style str acc ->
                { acc | styled = True, list = SText str :: SSGR (styleToSgrs style) :: acc.list }
        , untagged =
            \str acc ->
                let
                    newAcc : { styled : Bool, newline : Bool, list : List (SimpleDoc -> SimpleDoc) }
                    newAcc =
                        if acc.styled then
                            { acc | styled = False, list = SSGR [ Ansi.Reset ] :: acc.list }

                        else
                            acc
                in
                if newAcc.newline then
                    { newAcc | newline = False, list = SLine (String.length str) :: newAcc.list }

                else
                    { newAcc | list = SText str :: newAcc.list }
        , newline = \acc -> { acc | newline = True }
        , outer = \{ list } -> List.foldl (<|) SEmpty list
        }
        doc


styleToSgrs : Style -> List Ansi.SGR
styleToSgrs style =
    [ if style.bold then
        Just (Ansi.SetConsoleIntensity Ansi.BoldIntensity)

      else
        Nothing
    , if style.underline then
        Just (Ansi.SetUnderlining Ansi.SingleUnderline)

      else
        Nothing
    , style.color
        |> Maybe.map
            (\color ->
                case color of
                    Red ->
                        Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Red

                    Green ->
                        Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Green

                    Cyan ->
                        Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Cyan

                    Magenta ->
                        Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Magenta

                    Blue ->
                        Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Blue

                    Black ->
                        Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Black

                    Yellow ->
                        Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Yellow

                    DullCyan ->
                        Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Cyan

                    DullRed ->
                        Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Red

                    DullYellow ->
                        Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Yellow
            )
    ]
        |> List.filterMap identity


displayS : SimpleDoc -> String -> String
displayS simpleDoc acc =
    case simpleDoc of
        SEmpty ->
            acc

        SText str sd ->
            displayS sd (acc ++ str)

        SLine n sd ->
            displayS sd (acc ++ "\n" ++ String.repeat n " ")

        SSGR (Ansi.Reset :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[0m")

        SSGR ((Ansi.SetUnderlining Ansi.SingleUnderline) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[4m")

        SSGR ((Ansi.SetColor _ Ansi.Dull Ansi.Red) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[31m")

        SSGR ((Ansi.SetColor _ Ansi.Vivid Ansi.Red) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[91m")

        SSGR ((Ansi.SetColor _ Ansi.Dull Ansi.Green) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[32m")

        SSGR ((Ansi.SetColor _ Ansi.Vivid Ansi.Green) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[92m")

        SSGR ((Ansi.SetColor _ Ansi.Dull Ansi.Yellow) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[33m")

        SSGR ((Ansi.SetColor _ Ansi.Vivid Ansi.Yellow) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[93m")

        SSGR ((Ansi.SetColor _ Ansi.Dull Ansi.Cyan) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[36m")

        SSGR ((Ansi.SetColor _ Ansi.Vivid Ansi.Cyan) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[96m")

        SSGR ((Ansi.SetUnderlining Ansi.DoubleUnderline) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[4:2m")

        SSGR ((Ansi.SetUnderlining Ansi.NoUnderline) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[24m")

        SSGR ((Ansi.SetColor _ Ansi.Dull Ansi.Black) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[30m")

        SSGR ((Ansi.SetColor _ Ansi.Dull Ansi.Blue) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[34m")

        SSGR ((Ansi.SetColor _ Ansi.Dull Ansi.Magenta) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[35m")

        SSGR ((Ansi.SetColor _ Ansi.Dull Ansi.White) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[37m")

        SSGR ((Ansi.SetColor _ Ansi.Vivid Ansi.Black) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[90m")

        SSGR ((Ansi.SetColor _ Ansi.Vivid Ansi.Blue) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[94m")

        SSGR ((Ansi.SetColor _ Ansi.Vivid Ansi.Magenta) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[95m")

        SSGR ((Ansi.SetColor _ Ansi.Vivid Ansi.White) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[97m")

        SSGR ((Ansi.SetConsoleIntensity Ansi.BoldIntensity) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[1m")

        SSGR ((Ansi.SetConsoleIntensity Ansi.FaintIntensity) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[2m")

        SSGR ((Ansi.SetConsoleIntensity Ansi.NormalIntensity) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[22m")

        SSGR ((Ansi.SetItalicized True) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[3m")

        SSGR ((Ansi.SetItalicized False) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[23m")

        SSGR ((Ansi.SetBlinkSpeed Ansi.SlowBlink) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[5m")

        SSGR ((Ansi.SetBlinkSpeed Ansi.RapidBlink) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[6m")

        SSGR ((Ansi.SetBlinkSpeed Ansi.NoBlink) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[25m")

        SSGR ((Ansi.SetVisible True) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[28m")

        SSGR ((Ansi.SetVisible False) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[8m")

        SSGR ((Ansi.SetSwapForegroundBackground True) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[7m")

        SSGR ((Ansi.SetSwapForegroundBackground False) :: tail) sd ->
            displayS (SSGR tail sd) (acc ++ "\u{001B}[27m")

        SSGR [] sd ->
            displayS sd acc


text : String -> Doc
text =
    P.string


plain : Doc -> Doc
plain =
    updateStyle (\_ -> defaultStyle)


underline : Doc -> Doc
underline =
    updateStyle (\style -> { style | underline = True })


a : Doc -> Doc -> Doc
a =
    P.a


plus : Doc -> Doc -> Doc
plus doc2 doc1 =
    P.words [ doc1, doc2 ]


append : Doc -> Doc -> Doc
append =
    P.append


align : Doc -> Doc
align =
    P.align


cat : List Doc -> Doc
cat =
    P.group << vcat


empty : Doc
empty =
    P.empty


fill : Int -> Doc -> Doc
fill =
    P.indent


fillSep : List Doc -> Doc
fillSep =
    P.softlines


hang : Int -> Doc -> Doc
hang =
    P.hang


hcat : List Doc -> Doc
hcat docs =
    hcatHelp docs empty


hcatHelp : List Doc -> Doc -> Doc
hcatHelp docs acc =
    case docs of
        [] ->
            acc

        [ doc ] ->
            doc

        doc :: ds ->
            hcatHelp ds (P.append doc acc)


hsep : List Doc -> Doc
hsep =
    P.words


indent : Int -> Doc -> Doc
indent =
    P.indent


sep : List Doc -> Doc
sep =
    P.group << P.lines


vcat : List Doc -> Doc
vcat =
    P.join P.tightline


red : Doc -> Doc
red =
    updateColor Red


cyan : Doc -> Doc
cyan =
    updateColor Cyan


magenta : Doc -> Doc
magenta =
    updateColor Magenta


green : Doc -> Doc
green =
    updateColor Green


blue : Doc -> Doc
blue =
    updateColor Blue


black : Doc -> Doc
black =
    updateColor Black


yellow : Doc -> Doc
yellow =
    updateColor Yellow


dullred : Doc -> Doc
dullred =
    updateColor DullRed


dullcyan : Doc -> Doc
dullcyan =
    updateColor DullCyan


dullyellow : Doc -> Doc
dullyellow =
    updateColor DullYellow



-- STYLE


type alias Style =
    { bold : Bool
    , underline : Bool
    , color : Maybe Color
    }


type Color
    = Red
    | Green
    | Cyan
    | Magenta
    | Blue
    | Black
    | Yellow
    | DullCyan
    | DullRed
    | DullYellow


defaultStyle : Style
defaultStyle =
    Style False False Nothing


updateColor : Color -> Doc -> Doc
updateColor newColor =
    updateStyle (\style -> { style | color = Just newColor })


updateStyle : (Style -> Style) -> Doc -> Doc
updateStyle mapper =
    P.updateTag
        (\_ ->
            Maybe.map mapper
                >> Maybe.withDefault (mapper defaultStyle)
                >> Just
        )
