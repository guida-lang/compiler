module Compiler.Parse.Keyword exposing
    ( alias_
    , as_
    , case_
    , command_
    , effect_
    , else_
    , exposing_
    , if_
    , import_
    , in_
    , infix_
    , k4
    , k5
    , left_
    , let_
    , module_
    , non_
    , of_
    , port_
    , right_
    , subscription_
    , then_
    , type_
    , where_
    )

import Compiler.Parse.Primitives as P exposing (Col, Parser, Row)
import Compiler.Parse.Variable as Var



-- DECLARATIONS


type_ : (Row -> Col -> x) -> Parser x ()
type_ tx =
    k4 't' 'y' 'p' 'e' tx


alias_ : (Row -> Col -> x) -> Parser x ()
alias_ tx =
    k5 'a' 'l' 'i' 'a' 's' tx


port_ : (Row -> Col -> x) -> Parser x ()
port_ tx =
    k4 'p' 'o' 'r' 't' tx



-- IF EXPRESSIONS


if_ : (Row -> Col -> x) -> Parser x ()
if_ tx =
    k2 'i' 'f' tx


then_ : (Row -> Col -> x) -> Parser x ()
then_ tx =
    k4 't' 'h' 'e' 'n' tx


else_ : (Row -> Col -> x) -> Parser x ()
else_ tx =
    k4 'e' 'l' 's' 'e' tx



-- CASE EXPRESSIONS


case_ : (Row -> Col -> x) -> Parser x ()
case_ tx =
    k4 'c' 'a' 's' 'e' tx


of_ : (Row -> Col -> x) -> Parser x ()
of_ tx =
    k2 'o' 'f' tx



-- LET EXPRESSIONS


let_ : (Row -> Col -> x) -> Parser x ()
let_ tx =
    k3 'l' 'e' 't' tx


in_ : (Row -> Col -> x) -> Parser x ()
in_ tx =
    k2 'i' 'n' tx



-- INFIXES


infix_ : (Row -> Col -> x) -> Parser x ()
infix_ tx =
    k5 'i' 'n' 'f' 'i' 'x' tx


left_ : (Row -> Col -> x) -> Parser x ()
left_ tx =
    k4 'l' 'e' 'f' 't' tx


right_ : (Row -> Col -> x) -> Parser x ()
right_ tx =
    k5 'r' 'i' 'g' 'h' 't' tx


non_ : (Row -> Col -> x) -> Parser x ()
non_ tx =
    k3 'n' 'o' 'n' tx



-- IMPORTS


module_ : (Row -> Col -> x) -> Parser x ()
module_ tx =
    k6 'm' 'o' 'd' 'u' 'l' 'e' tx


import_ : (Row -> Col -> x) -> Parser x ()
import_ tx =
    k6 'i' 'm' 'p' 'o' 'r' 't' tx


exposing_ : (Row -> Col -> x) -> Parser x ()
exposing_ tx =
    k8 'e' 'x' 'p' 'o' 's' 'i' 'n' 'g' tx


as_ : (Row -> Col -> x) -> Parser x ()
as_ tx =
    k2 'a' 's' tx



-- EFFECTS


effect_ : (Row -> Col -> x) -> Parser x ()
effect_ tx =
    k6 'e' 'f' 'f' 'e' 'c' 't' tx


where_ : (Row -> Col -> x) -> Parser x ()
where_ tx =
    k5 'w' 'h' 'e' 'r' 'e' tx


command_ : (Row -> Col -> x) -> Parser x ()
command_ tx =
    k7 'c' 'o' 'm' 'm' 'a' 'n' 'd' tx


subscription_ : (Row -> Col -> x) -> Parser x ()
subscription_ toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                pos12 : Int
                pos12 =
                    pos + 12
            in
            if
                (pos12 <= end)
                    && (P.unsafeIndex src pos == 's')
                    && (P.unsafeIndex src (pos + 1) == 'u')
                    && (P.unsafeIndex src (pos + 2) == 'b')
                    && (P.unsafeIndex src (pos + 3) == 's')
                    && (P.unsafeIndex src (pos + 4) == 'c')
                    && (P.unsafeIndex src (pos + 5) == 'r')
                    && (P.unsafeIndex src (pos + 6) == 'i')
                    && (P.unsafeIndex src (pos + 7) == 'p')
                    && (P.unsafeIndex src (pos + 8) == 't')
                    && (P.unsafeIndex src (pos + 9) == 'i')
                    && (P.unsafeIndex src (pos + 10) == 'o')
                    && (P.unsafeIndex src (pos + 11) == 'n')
                    && (Var.getInnerWidth src pos12 end == 0)
            then
                let
                    s : P.State
                    s =
                        P.State src pos12 end indent row (col + 12)
                in
                P.Cok () s

            else
                P.Eerr row col toError



-- KEYWORDS


k2 : Char -> Char -> (Row -> Col -> x) -> Parser x ()
k2 w1 w2 toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                pos2 : Int
                pos2 =
                    pos + 2
            in
            if
                (pos2 <= end)
                    && (P.unsafeIndex src pos == w1)
                    && (P.unsafeIndex src (pos + 1) == w2)
                    && (Var.getInnerWidth src pos2 end == 0)
            then
                let
                    s : P.State
                    s =
                        P.State src pos2 end indent row (col + 2)
                in
                P.Cok () s

            else
                P.Eerr row col toError


k3 : Char -> Char -> Char -> (Row -> Col -> x) -> Parser x ()
k3 w1 w2 w3 toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                pos3 : Int
                pos3 =
                    pos + 3
            in
            if
                (pos3 <= end)
                    && (P.unsafeIndex src pos == w1)
                    && (P.unsafeIndex src (pos + 1) == w2)
                    && (P.unsafeIndex src (pos + 2) == w3)
                    && (Var.getInnerWidth src pos3 end == 0)
            then
                let
                    s : P.State
                    s =
                        P.State src pos3 end indent row (col + 3)
                in
                P.Cok () s

            else
                P.Eerr row col toError


k4 : Char -> Char -> Char -> Char -> (Row -> Col -> x) -> Parser x ()
k4 w1 w2 w3 w4 toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                pos4 : Int
                pos4 =
                    pos + 4
            in
            if
                (pos4 <= end)
                    && (P.unsafeIndex src pos == w1)
                    && (P.unsafeIndex src (pos + 1) == w2)
                    && (P.unsafeIndex src (pos + 2) == w3)
                    && (P.unsafeIndex src (pos + 3) == w4)
                    && (Var.getInnerWidth src pos4 end == 0)
            then
                let
                    s : P.State
                    s =
                        P.State src pos4 end indent row (col + 4)
                in
                P.Cok () s

            else
                P.Eerr row col toError


k5 : Char -> Char -> Char -> Char -> Char -> (Row -> Col -> x) -> Parser x ()
k5 w1 w2 w3 w4 w5 toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                pos5 : Int
                pos5 =
                    pos + 5
            in
            if
                (pos5 <= end)
                    && (P.unsafeIndex src pos == w1)
                    && (P.unsafeIndex src (pos + 1) == w2)
                    && (P.unsafeIndex src (pos + 2) == w3)
                    && (P.unsafeIndex src (pos + 3) == w4)
                    && (P.unsafeIndex src (pos + 4) == w5)
                    && (Var.getInnerWidth src pos5 end == 0)
            then
                let
                    s : P.State
                    s =
                        P.State src pos5 end indent row (col + 5)
                in
                P.Cok () s

            else
                P.Eerr row col toError


k6 : Char -> Char -> Char -> Char -> Char -> Char -> (Row -> Col -> x) -> Parser x ()
k6 w1 w2 w3 w4 w5 w6 toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                pos6 : Int
                pos6 =
                    pos + 6
            in
            if
                (pos6 <= end)
                    && (P.unsafeIndex src pos == w1)
                    && (P.unsafeIndex src (pos + 1) == w2)
                    && (P.unsafeIndex src (pos + 2) == w3)
                    && (P.unsafeIndex src (pos + 3) == w4)
                    && (P.unsafeIndex src (pos + 4) == w5)
                    && (P.unsafeIndex src (pos + 5) == w6)
                    && (Var.getInnerWidth src pos6 end == 0)
            then
                let
                    s : P.State
                    s =
                        P.State src pos6 end indent row (col + 6)
                in
                P.Cok () s

            else
                P.Eerr row col toError


k7 : Char -> Char -> Char -> Char -> Char -> Char -> Char -> (Row -> Col -> x) -> Parser x ()
k7 w1 w2 w3 w4 w5 w6 w7 toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                pos7 : Int
                pos7 =
                    pos + 7
            in
            if
                (pos7 <= end)
                    && (P.unsafeIndex src pos == w1)
                    && (P.unsafeIndex src (pos + 1) == w2)
                    && (P.unsafeIndex src (pos + 2) == w3)
                    && (P.unsafeIndex src (pos + 3) == w4)
                    && (P.unsafeIndex src (pos + 4) == w5)
                    && (P.unsafeIndex src (pos + 5) == w6)
                    && (P.unsafeIndex src (pos + 6) == w7)
                    && (Var.getInnerWidth src pos7 end == 0)
            then
                let
                    s : P.State
                    s =
                        P.State src pos7 end indent row (col + 7)
                in
                P.Cok () s

            else
                P.Eerr row col toError


k8 : Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> (Row -> Col -> x) -> Parser x ()
k8 w1 w2 w3 w4 w5 w6 w7 w8 toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                pos8 : Int
                pos8 =
                    pos + 8
            in
            if
                (pos8 <= end)
                    && (P.unsafeIndex src pos == w1)
                    && (P.unsafeIndex src (pos + 1) == w2)
                    && (P.unsafeIndex src (pos + 2) == w3)
                    && (P.unsafeIndex src (pos + 3) == w4)
                    && (P.unsafeIndex src (pos + 4) == w5)
                    && (P.unsafeIndex src (pos + 5) == w6)
                    && (P.unsafeIndex src (pos + 6) == w7)
                    && (P.unsafeIndex src (pos + 7) == w8)
                    && (Var.getInnerWidth src pos8 end == 0)
            then
                let
                    s : P.State
                    s =
                        P.State src pos8 end indent row (col + 8)
                in
                P.Cok () s

            else
                P.Eerr row col toError
