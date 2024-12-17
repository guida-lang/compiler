module Compiler.Data.Name exposing
    ( CDN_Name
    , array
    , basics
    , bitwise
    , bool
    , char
    , cmd
    , debug
    , debugger
    , dict
    , dollar
    , false
    , float
    , fromManyNames
    , fromPtr
    , fromTypeVariable
    , fromTypeVariableScheme
    , fromVarIndex
    , fromWords
    , getKernel
    , hasDot
    , identity_
    , int
    , isAppendableType
    , isCompappendType
    , isComparableType
    , isKernel
    , isNumberType
    , jsArray
    , list
    , mainModule
    , main_
    , maybe
    , negate
    , node
    , platform
    , program
    , replModule
    , replValueToPrint
    , result
    , router
    , sepBy
    , shader
    , splitDots
    , string
    , sub
    , task
    , toChars
    , toElmString
    , true
    , tuple
    , utils
    , value
    , virtualDom
    )

import Utils.Crash exposing (crash)



-- NAME


type alias CDN_Name =
    String



-- TO


toChars : CDN_Name -> List Char
toChars =
    String.toList


toElmString : CDN_Name -> String
toElmString =
    identity



-- FROM


fromPtr : String -> Int -> Int -> CDN_Name
fromPtr src start end =
    String.slice start end src



-- HAS DOT


hasDot : CDN_Name -> Bool
hasDot =
    String.contains "."


splitDots : CDN_Name -> List String
splitDots =
    String.split "."



-- GET KERNEL


getKernel : CDN_Name -> CDN_Name
getKernel name =
    if isKernel name then
        String.dropLeft (String.length prefixKernel) name

    else
        crash "AssertionFailed"



-- STARTS WITH


isKernel : CDN_Name -> Bool
isKernel =
    String.startsWith prefixKernel


isNumberType : CDN_Name -> Bool
isNumberType =
    String.startsWith prefixNumber


isComparableType : CDN_Name -> Bool
isComparableType =
    String.startsWith prefixComparable


isAppendableType : CDN_Name -> Bool
isAppendableType =
    String.startsWith prefixAppendable


isCompappendType : CDN_Name -> Bool
isCompappendType =
    String.startsWith prefixCompappend


prefixKernel : CDN_Name
prefixKernel =
    "Elm.Kernel."


prefixNumber : CDN_Name
prefixNumber =
    "number"


prefixComparable : CDN_Name
prefixComparable =
    "comparable"


prefixAppendable : CDN_Name
prefixAppendable =
    "appendable"


prefixCompappend : CDN_Name
prefixCompappend =
    "compappend"



-- FROM VAR INDEX


fromVarIndex : Int -> CDN_Name
fromVarIndex n =
    writeDigitsAtEnd "_v" n


writeDigitsAtEnd : String -> Int -> String
writeDigitsAtEnd prefix n =
    prefix ++ String.fromInt n



-- FROM TYPE VARIABLE


fromTypeVariable : CDN_Name -> Int -> CDN_Name
fromTypeVariable name index =
    if index <= 0 then
        name

    else
        name
            |> String.toList
            |> List.reverse
            |> List.head
            |> Maybe.map
                (\lastChar ->
                    if Char.isDigit lastChar then
                        writeDigitsAtEnd (name ++ "_") index

                    else
                        writeDigitsAtEnd name index
                )
            |> Maybe.withDefault name



-- FROM TYPE VARIABLE SCHEME


fromTypeVariableScheme : Int -> CDN_Name
fromTypeVariableScheme scheme =
    if scheme < 26 then
        (0x61 + scheme)
            |> Char.fromCode
            |> String.fromChar

    else
        -- do
        --     let (extra, letter) = List.quotRem scheme 26
        --     let size = 1 + getIndexSize extra
        --     mba <- newByteArray size
        --     writeWord8 mba 0 (0x61 + Word.fromInt letter)
        --     writeDigitsAtEnd mba size extra
        --     freeze mba
        let
            letter : Int
            letter =
                remainderBy 26 scheme

            extra : Int
            extra =
                max 0 (scheme - letter)
        in
        writeDigitsAtEnd
            ((0x61 + letter)
                |> Char.fromCode
                |> String.fromChar
            )
            extra



-- FROM MANY NAMES
--
-- Creating a unique name by combining all the subnames can create names
-- longer than 256 bytes relatively easily. So instead, the first given name
-- (e.g. foo) is prefixed chars that are valid in JS but not Elm (e.g. _M$foo)
--
-- This should be a unique name since 0.19 disallows shadowing. It would not
-- be possible for multiple top-level cycles to include values with the same
-- name, so the important thing is to make the cycle name distinct from the
-- normal name. Same logic for destructuring patterns like (x,y)


fromManyNames : List CDN_Name -> CDN_Name
fromManyNames names =
    case names of
        [] ->
            blank

        -- NOTE: this case is needed for (let _ = Debug.log "x" x in ...)
        -- but maybe unused patterns should be stripped out instead
        firstName :: _ ->
            blank ++ firstName


blank : CDN_Name
blank =
    "_M$"



-- FROM WORDS


fromWords : List Char -> CDN_Name
fromWords words =
    String.fromList words



-- writeWords : MBA s -> Int -> List Word.Word8 -> ST s ()
-- writeWords !mba !i words =
--     case words of
--         [] ->
--             ()
--         w :: ws ->
--             do
--                 writeWord8 mba i w
--                 writeWords mba (i + 1) ws
-- SEP BY


sepBy : Char -> CDN_Name -> CDN_Name -> CDN_Name
sepBy sep ba1 ba2 =
    String.join (String.fromChar sep) [ ba1, ba2 ]



-- COMMON NAMES


int : CDN_Name
int =
    "Int"


float : CDN_Name
float =
    "Float"


bool : CDN_Name
bool =
    "Bool"


char : CDN_Name
char =
    "Char"


string : CDN_Name
string =
    "String"


maybe : CDN_Name
maybe =
    "Maybe"


result : CDN_Name
result =
    "Result"


list : CDN_Name
list =
    "List"


array : CDN_Name
array =
    "Array"


dict : CDN_Name
dict =
    "Dict"


tuple : CDN_Name
tuple =
    "Tuple"


jsArray : CDN_Name
jsArray =
    "JsArray"


task : CDN_Name
task =
    "Task"


router : CDN_Name
router =
    "Router"


cmd : CDN_Name
cmd =
    "Cmd"


sub : CDN_Name
sub =
    "Sub"


platform : CDN_Name
platform =
    "Platform"


virtualDom : CDN_Name
virtualDom =
    "VirtualDom"


shader : CDN_Name
shader =
    "Shader"


debug : CDN_Name
debug =
    "Debug"


debugger : CDN_Name
debugger =
    "Debugger"


bitwise : CDN_Name
bitwise =
    "Bitwise"


basics : CDN_Name
basics =
    "Basics"


utils : CDN_Name
utils =
    "Utils"


negate : CDN_Name
negate =
    "negate"


true : CDN_Name
true =
    "True"


false : CDN_Name
false =
    "False"


value : CDN_Name
value =
    "Value"


node : CDN_Name
node =
    "Node"


program : CDN_Name
program =
    "Program"


main_ : CDN_Name
main_ =
    "main"


mainModule : CDN_Name
mainModule =
    "Main"


dollar : CDN_Name
dollar =
    "$"


identity_ : CDN_Name
identity_ =
    "identity"


replModule : CDN_Name
replModule =
    "Elm_Repl"


replValueToPrint : CDN_Name
replValueToPrint =
    "repl_input_value_"
