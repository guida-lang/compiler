module Compiler.Json.Encode exposing
    ( Value(..)
    , array
    , bool
    , chars
    , dict
    , encodeUgly
    , int
    , list
    , name
    , null
    , object
    , string
    , write
    , writeUgly
    )

import Data.Map as Dict exposing (Dict)
import System.IO as IO exposing (IO(..))



-- VALUES


type Value
    = Array (List Value)
    | Object (List ( String, Value ))
    | StringVal String
    | Boolean Bool
    | Integer Int
    | Number Float
    | Null


array : List Value -> Value
array =
    Array


object : List ( String, Value ) -> Value
object =
    Object


string : String -> Value
string str =
    StringVal str


name : String -> Value
name nm =
    StringVal nm


bool : Bool -> Value
bool =
    Boolean


int : Int -> Value
int =
    Integer


null : Value
null =
    Null


dict : (k -> k -> Order) -> (k -> String) -> (v -> Value) -> Dict c k v -> Value
dict keyComparison encodeKey encodeValue pairs =
    Object
        (Dict.toList keyComparison pairs
            |> List.map (\( k, v ) -> ( encodeKey k, encodeValue v ))
        )


list : (a -> Value) -> List a -> Value
list encodeEntry entries =
    Array (List.map encodeEntry entries)



-- CHARS


chars : String -> Value
chars chrs =
    StringVal (escape chrs)


escape : String -> String
escape chrs =
    case String.toList chrs of
        [] ->
            ""

        c :: cs ->
            let
                escapedChar : String
                escapedChar =
                    case c of
                        '\u{000D}' ->
                            "\\r"

                        '\n' ->
                            "\\n"

                        '"' ->
                            "\\\""

                        '\\' ->
                            "\\\\"

                        _ ->
                            String.fromChar c
            in
            escapedChar ++ escape (String.fromList cs)



-- WRITE TO FILE


write : String -> Value -> IO ()
write path value =
    fileWriteBuilder path (encode value ++ "\n")


writeUgly : String -> Value -> IO ()
writeUgly path value =
    fileWriteBuilder path (encodeUgly value)


{-| FIXME Builder.File.writeBuilder
-}
fileWriteBuilder : String -> String -> IO ()
fileWriteBuilder path value =
    IO (\s -> ( s, IO.WriteString IO.pure path value ))



-- ENCODE UGLY


encodeUgly : Value -> String
encodeUgly value =
    case value of
        Array [] ->
            "[]"

        Array entries ->
            "[" ++ String.join "," (List.map encodeUgly entries) ++ "]"

        Object [] ->
            "{}"

        Object entries ->
            "{" ++ String.join "," (List.map encodeEntryUgly entries) ++ "}"

        StringVal builder ->
            "\"" ++ builder ++ "\""

        Boolean boolean ->
            if boolean then
                "true"

            else
                "false"

        Integer n ->
            String.fromInt n

        Number scientific ->
            String.fromFloat scientific

        Null ->
            "null"


encodeEntryUgly : ( String, Value ) -> String
encodeEntryUgly ( key, entry ) =
    "\"" ++ key ++ "\":" ++ encodeUgly entry



-- ENCODE


encode : Value -> String
encode value =
    encodeHelp "" value


encodeHelp : String -> Value -> String
encodeHelp indent value =
    case value of
        Array [] ->
            "[]"

        Array (first :: rest) ->
            encodeArray indent first rest

        Object [] ->
            "{}"

        Object (first :: rest) ->
            encodeObject indent first rest

        StringVal builder ->
            "\"" ++ builder ++ "\""

        Boolean boolean ->
            if boolean then
                "true"

            else
                "false"

        Integer n ->
            String.fromInt n

        Number scientific ->
            String.fromFloat scientific

        Null ->
            "null"



-- ENCODE ARRAY


encodeArray : String -> Value -> List Value -> String
encodeArray indent first rest =
    let
        newIndent : String
        newIndent =
            indent ++ "    "

        closer : String
        closer =
            "\n" ++ indent ++ "]"

        addValue : Value -> String -> String
        addValue field builder =
            ",\n" ++ newIndent ++ encodeHelp newIndent field ++ builder
    in
    "[\n" ++ newIndent ++ encodeHelp newIndent first ++ List.foldr addValue closer rest



-- ENCODE OBJECT


encodeObject : String -> ( String, Value ) -> List ( String, Value ) -> String
encodeObject indent first rest =
    let
        newIndent : String
        newIndent =
            indent ++ "    "

        closer : String
        closer =
            "\n" ++ indent ++ "}"

        addValue : ( String, Value ) -> String -> String
        addValue field builder =
            ",\n" ++ newIndent ++ encodeField newIndent field ++ builder
    in
    "{\n" ++ newIndent ++ encodeField newIndent first ++ List.foldr addValue closer rest


encodeField : String -> ( String, Value ) -> String
encodeField indent ( key, value ) =
    "\"" ++ key ++ "\": " ++ encodeHelp indent value
