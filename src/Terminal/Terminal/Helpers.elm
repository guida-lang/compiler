module Terminal.Terminal.Helpers exposing
    ( filePath
    , guidaOrElmFile
    , package
    , parseFilePath
    , parseGuidaOrElmFile
    , parsePackage
    , parseVersion
    , version
    )

import Builder.Deps.Registry as Registry
import Builder.Stuff as Stuff
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Parse.Primitives as P
import Compiler.Reporting.Suggest as Suggest
import Data.Map as Dict
import Task exposing (Task)
import Terminal.Terminal.Internal exposing (Parser(..))
import Utils.Main as Utils exposing (FilePath)
import Utils.Task.Extra as Task



-- VERSION


version : Parser
version =
    Parser
        { singular = "version"
        , plural = "versions"
        , suggest = suggestVersion
        , examples = Task.pure << exampleVersions
        }


parseVersion : String -> Maybe V.Version
parseVersion chars =
    case P.fromByteString V.parser Tuple.pair chars of
        Ok vsn ->
            Just vsn

        Err _ ->
            Nothing


suggestVersion : String -> Task Never (List String)
suggestVersion _ =
    Task.pure []


exampleVersions : String -> List String
exampleVersions chars =
    let
        chunks : List String
        chunks =
            String.split "." chars

        isNumber : String -> Bool
        isNumber cs =
            not (String.isEmpty cs) && String.all Char.isDigit cs
    in
    if List.all isNumber chunks then
        case chunks of
            [ x ] ->
                [ x ++ ".0.0" ]

            [ x, y ] ->
                [ x ++ "." ++ y ++ ".0" ]

            x :: y :: z :: _ ->
                [ x ++ "." ++ y ++ "." ++ z ]

            _ ->
                [ "1.0.0", "2.0.3" ]

    else
        [ "1.0.0", "2.0.3" ]



-- GUIDA OR ELM FILE


guidaOrElmFile : Parser
guidaOrElmFile =
    Parser
        { singular = "guida or elm file"
        , plural = "guida or elm files"
        , suggest = \_ -> Task.pure []
        , examples = exampleGuidaOrElmFiles
        }


parseGuidaOrElmFile : String -> Maybe FilePath
parseGuidaOrElmFile chars =
    case Utils.fpTakeExtension chars of
        ".guida" ->
            Just chars

        ".elm" ->
            Just chars

        _ ->
            Nothing


exampleGuidaOrElmFiles : String -> Task Never (List String)
exampleGuidaOrElmFiles _ =
    Task.pure [ "Main.guida", "src/Main.guida", "Main.elm" ]



-- FILE PATH


filePath : Parser
filePath =
    Parser
        { singular = "file path"
        , plural = "file paths"
        , suggest = \_ -> Task.pure []
        , examples = exampleFilePaths
        }


parseFilePath : String -> Maybe FilePath
parseFilePath =
    Just


exampleFilePaths : String -> Task Never (List String)
exampleFilePaths _ =
    Task.pure [ "Main.elm", "src" ]



-- PACKAGE


package : Parser
package =
    Parser
        { singular = "package"
        , plural = "packages"
        , suggest = suggestPackages
        , examples = examplePackages
        }


parsePackage : String -> Maybe Pkg.Name
parsePackage chars =
    case P.fromByteString Pkg.parser Tuple.pair chars of
        Ok pkg ->
            Just pkg

        Err _ ->
            Nothing


suggestPackages : String -> Task Never (List String)
suggestPackages given =
    Stuff.getPackageCache
        |> Task.bind
            (\cache ->
                Registry.read cache
                    |> Task.fmap
                        (\maybeRegistry ->
                            case maybeRegistry of
                                Nothing ->
                                    []

                                Just (Registry.Registry _ versions) ->
                                    List.filter (String.startsWith given) <|
                                        List.map Pkg.toChars (Dict.keys compare versions)
                        )
            )


examplePackages : String -> Task Never (List String)
examplePackages given =
    Stuff.getPackageCache
        |> Task.bind
            (\cache ->
                Registry.read cache
                    |> Task.fmap
                        (\maybeRegistry ->
                            case maybeRegistry of
                                Nothing ->
                                    [ "elm/json"
                                    , "elm/http"
                                    , "elm/random"
                                    ]

                                Just (Registry.Registry _ versions) ->
                                    List.map Pkg.toChars <|
                                        List.take 4 <|
                                            Suggest.sort given Pkg.toChars (Dict.keys compare versions)
                        )
            )
