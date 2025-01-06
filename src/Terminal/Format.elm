module Terminal.Format exposing
    ( Flags(..)
    , run
    )

import Builder.File as File
import Compiler.AST.Source as Src
import Compiler.Parse.Module as Parse
import Compiler.Reporting.Annotation as A
import List.Extra as List
import System.IO as IO exposing (IO)
import Utils.Main as Utils exposing (FilePath)


type Flags
    = Flags (Maybe FilePath)


run : List String -> Flags -> IO ()
run paths flags =
    runHelp paths flags


runHelp : List String -> Flags -> IO ()
runHelp paths ((Flags maybeOutput) as flags) =
    case paths of
        [] ->
            IO.pure ()

        path :: remainingPaths ->
            File.readUtf8 path
                |> IO.bind
                    (\source ->
                        let
                            projectType =
                                -- FIXME
                                Parse.Package ( "elm", "core" )
                        in
                        case Parse.fromByteString projectType source of
                            Ok modul ->
                                File.writeUtf8 (Maybe.withDefault path maybeOutput) (formatModule (Debug.log "modul" modul))

                            Err err ->
                                -- FIXME
                                IO.pure ()
                    )
                |> IO.bind (\_ -> runHelp remainingPaths flags)


formatModule : Src.Module -> String
formatModule modul =
    -- FIXME
    formatModuleHeader modul
        ++ formatModuleImports modul
        ++ formatModuleBody modul



-- HEADER


formatModuleHeader : Src.Module -> String
formatModuleHeader ((Src.Module _ (A.At _ exports) _ _ _ _ _ _ effects) as modul) =
    formatModuleHeaderEffects (Src.getName modul) effects ++ formatExposing exports ++ "\n\n"


formatModuleHeaderEffects : String -> Src.Effects -> String
formatModuleHeaderEffects name effects =
    case effects of
        Src.NoEffects ->
            "module " ++ name

        Src.Ports _ ->
            "port module " ++ name

        Src.Manager _ manager ->
            -- FIXME effect module MyThing where { command = MyCmd }
            "effect module " ++ name ++ " where { ... }"



-- EXPOSING


formatExposing : Src.Exposing -> String
formatExposing exports =
    case exports of
        Src.Open ->
            " exposing (..)"

        Src.Explicit [] ->
            ""

        Src.Explicit (firstExposed :: restExposed) ->
            if exposedInSingleLine firstExposed (List.last restExposed) then
                " exposing (" ++ String.join ", " (List.map formatExposed (firstExposed :: restExposed)) ++ ")"

            else
                " exposing\n    ( " ++ String.join "\n    , " (List.map formatExposed (firstExposed :: restExposed)) ++ "\n)"


exposedInSingleLine : Src.Exposed -> Maybe Src.Exposed -> Bool
exposedInSingleLine firstExposed maybeLastExposed =
    let
        (A.Region (A.Position firstRow _) _) =
            exposedRegion firstExposed
    in
    case Maybe.map exposedRegion maybeLastExposed of
        Just (A.Region _ (A.Position lastRow _)) ->
            firstRow == lastRow

        Nothing ->
            True


exposedRegion : Src.Exposed -> A.Region
exposedRegion exposed =
    case exposed of
        Src.Lower (A.At region _) ->
            region

        Src.Upper (A.At region _) _ ->
            region

        Src.Operator region _ ->
            region


formatExposed : Src.Exposed -> String
formatExposed exposed =
    case exposed of
        Src.Lower (A.At _ name) ->
            name

        Src.Upper (A.At _ name) (Src.Public _) ->
            name ++ "(..)"

        Src.Upper (A.At _ name) Src.Private ->
            name

        Src.Operator _ name ->
            "(" ++ name ++ ")"



-- IMPORTS


formatModuleImports : Src.Module -> String
formatModuleImports (Src.Module _ _ _ imports _ _ _ _ _) =
    case imports of
        [] ->
            ""

        _ ->
            String.join "\n" (List.map formatModuleImport imports)
                ++ "\n\n"


formatModuleImport : Src.Import -> String
formatModuleImport (Src.Import (A.At _ name) maybeAlias exports) =
    let
        alias =
            maybeAlias
                |> Maybe.map ((++) " as ")
                |> Maybe.withDefault ""
    in
    "import " ++ name ++ alias ++ formatExposing exports



-- BODY


formatModuleBody : Src.Module -> String
formatModuleBody (Src.Module _ _ _ _ values unions aliases binops effects) =
    List.map formatModuleValue values
        |> String.join "\n\n"


formatModuleValue : A.Located Src.Value -> String
formatModuleValue (A.At _ (Src.Value (A.At _ name) srcArgs body maybeType)) =
    let
        signature =
            maybeType
                |> Maybe.map (\tipe -> name ++ " : " ++ formatModuleType tipe ++ "\n")
                |> Maybe.withDefault ""
    in
    signature
        ++ name
        ++ String.join " " (List.map (\(A.At _ srcArg) -> Debug.toString srcArg) srcArgs)
        ++ " =\n    "
        ++ Debug.toString body


formatModuleType : Src.Type -> String
formatModuleType (A.At _ type_) =
    case Debug.log "type_" type_ of
        Src.TLambda arg result ->
            formatModuleType arg ++ " -> " ++ formatModuleType result

        Src.TVar name ->
            name

        Src.TType _ name args ->
            name ++ " " ++ String.join " " (List.map formatModuleType args)

        Src.TTypeQual _ home name args ->
            Debug.todo "formatModuleType TTypeQual"

        Src.TRecord fields ext ->
            Debug.todo "formatModuleType TRecord"

        Src.TUnit ->
            "()"

        Src.TTuple a b cs ->
            Debug.todo "formatModuleType TTuple"
