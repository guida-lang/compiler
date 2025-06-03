module Common.Format exposing (format)

import Compiler.AST.Source as Src
import Compiler.AST.Utils.Binop as Binop
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Json.String as Json
import Compiler.Parse.Declaration as Decl
import Compiler.Parse.Module as M
import Compiler.Parse.Primitives as P
import Compiler.Parse.SyntaxVersion exposing (SyntaxVersion)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E


format : SyntaxVersion -> M.ProjectType -> String -> Result E.Module String
format syntaxVersion projectType src =
    P.fromByteString (M.chompModule syntaxVersion projectType) E.ModuleBadEnd src
        |> Result.map formatModule


formatModule : M.Module -> String
formatModule modul =
    preExposing modul
        ++ " exposing"
        ++ formatExports modul
        ++ "\n"
        ++ formatHeaderDocs modul
        ++ "\n"
        ++ formatImports modul
        ++ formatInfixes modul
        ++ "\n"
        ++ formatDeclarations modul
        ++ "\n"


preExposing : M.Module -> String
preExposing modul =
    case Maybe.map (\{ name, effects } -> ( name, effects )) modul.header of
        Just ( A.At _ name, M.NoEffects _ ) ->
            "module " ++ name

        Just ( A.At _ name, M.Ports _ ) ->
            "port module " ++ name

        Just ( A.At _ name, M.Manager _ manager ) ->
            "effect module " ++ name ++ " where " ++ formatManager manager

        Nothing ->
            "module " ++ Name.mainModule


formatManager : Src.Manager -> String
formatManager manager =
    case manager of
        Src.Cmd (A.At _ cmdType) ->
            "{ command = " ++ cmdType ++ " }"

        Src.Sub (A.At _ subType) ->
            "{ subscription = " ++ subType ++ " }"

        Src.Fx (A.At _ cmdType) (A.At _ subType) ->
            "{ command = " ++ cmdType ++ ", subscription = " ++ subType ++ " }"


formatExports : M.Module -> String
formatExports modul =
    case Maybe.map .exports modul.header of
        Just (A.At (A.Region (A.Position startRow _) (A.Position endRow _)) (Src.Explicit exposedList)) ->
            -- FIXME Src.YesDocs comment _
            -- let
            --     _ =
            --         Debug.log "parseOverview" (Docs.parseOverview comment)
            -- in
            let
                exposed : List String
                exposed =
                    exposedList
                        |> List.sortBy
                            (\exposedValue ->
                                case exposedValue of
                                    Src.Operator _ name ->
                                        ( 1, name )

                                    Src.Upper (A.At _ name) _ ->
                                        ( 2, name )

                                    Src.Lower (A.At _ name) ->
                                        ( 3, name )
                            )
                        |> List.map
                            (\exposedValue ->
                                case exposedValue of
                                    Src.Operator _ name ->
                                        "(" ++ name ++ ")"

                                    Src.Upper (A.At _ name) (Src.Public _) ->
                                        name ++ "(..)"

                                    Src.Upper (A.At _ name) Src.Private ->
                                        name

                                    Src.Lower (A.At _ name) ->
                                        name
                            )
            in
            if endRow > startRow then
                indent ("\n( " ++ String.join "\n, " exposed ++ "\n)")

            else
                " (" ++ String.join ", " exposed ++ ")"

        _ ->
            " (..)"


formatHeaderDocs : M.Module -> String
formatHeaderDocs modul =
    case Maybe.map .docs modul.header of
        Just (Ok (Src.Comment comment)) ->
            -- FIXME format comment
            "\n{-|" ++ Json.fromComment comment ++ "-}\n"

        _ ->
            ""


formatImports : M.Module -> String
formatImports modul =
    case modul.imports of
        [] ->
            ""

        imports ->
            String.join "\n" (List.map formatImport imports) ++ "\n\n"


formatImport : Src.Import -> String
formatImport ((Src.Import _ maybeAlias exposing_) as import_) =
    let
        formattedAlias : String
        formattedAlias =
            case maybeAlias of
                Just alias_ ->
                    " as " ++ alias_

                Nothing ->
                    ""

        formattedExposing : String
        formattedExposing =
            case exposing_ of
                Src.Explicit [] ->
                    ""

                Src.Explicit exposedList ->
                    let
                        exposed : List String
                        exposed =
                            exposedList
                                |> List.sortBy
                                    (\exposedValue ->
                                        case exposedValue of
                                            Src.Operator _ name ->
                                                ( 1, name )

                                            Src.Upper (A.At _ name) _ ->
                                                ( 2, name )

                                            Src.Lower (A.At _ name) ->
                                                ( 3, name )
                                    )
                                |> List.map
                                    (\exposedValue ->
                                        case exposedValue of
                                            Src.Operator _ name ->
                                                "(" ++ name ++ ")"

                                            Src.Upper (A.At _ name) (Src.Public _) ->
                                                name ++ "(..)"

                                            Src.Upper (A.At _ name) Src.Private ->
                                                name

                                            Src.Lower (A.At _ name) ->
                                                name
                                    )

                        exposedRows : List Int
                        exposedRows =
                            List.map
                                (\exposedValue ->
                                    case exposedValue of
                                        Src.Operator (A.Region (A.Position startRow _) _) _ ->
                                            startRow

                                        Src.Upper (A.At (A.Region (A.Position startRow _) _) _) _ ->
                                            startRow

                                        Src.Lower (A.At (A.Region (A.Position startRow _) _) _) ->
                                            startRow
                                )
                                exposedList

                        multiLineExposing : Bool
                        multiLineExposing =
                            Maybe.map2 (<) (List.minimum exposedRows) (List.maximum exposedRows)
                                |> Maybe.withDefault False
                    in
                    if multiLineExposing then
                        indent
                            ("\nexposing\n"
                                ++ indent ("( " ++ String.join "\n, " exposed ++ "\n)")
                            )

                    else
                        " exposing (" ++ String.join ", " exposed ++ ")"

                Src.Open ->
                    " exposing (..)"
    in
    "import "
        ++ Src.getImportName import_
        ++ formattedAlias
        ++ formattedExposing


formatInfixes : M.Module -> String
formatInfixes modul =
    case modul.infixes of
        [] ->
            ""

        infixes ->
            "\n" ++ String.join "\n" (List.map formatInfix infixes) ++ "\n\n"


formatInfix : A.Located Src.Infix -> String
formatInfix (A.At _ (Src.Infix op associativity precedence name)) =
    let
        associativityText : String
        associativityText =
            case associativity of
                Binop.Left ->
                    "left"

                Binop.Non ->
                    "non"

                Binop.Right ->
                    "right"
    in
    "infix " ++ associativityText ++ " " ++ String.fromInt precedence ++ " (" ++ op ++ ") = " ++ name


formatDeclarations : M.Module -> String
formatDeclarations modul =
    modul.decls
        |> List.map formatDeclaration
        |> String.join "\n\n\n"


formatDeclaration : Decl.Decl -> String
formatDeclaration decl =
    case decl of
        Decl.Value maybeDocs (A.At _ (Src.Value (A.At _ name) srcArgs body maybeType)) ->
            formatMaybeDocs maybeDocs
                ++ formatMaybeType name maybeType
                ++ String.join " " (name :: List.map (formatPattern True) srcArgs ++ [ "=" ])
                ++ "\n"
                ++ indent (formatExpr False body)

        Decl.Union maybeDocs (A.At _ (Src.Union (A.At _ name) args constructors)) ->
            formatMaybeDocs maybeDocs
                ++ "type "
                ++ name
                ++ String.concat (List.map (\(A.At _ arg) -> " " ++ arg) args)
                ++ "\n"
                ++ indent ("= " ++ String.join "\n| " (List.map (\( A.At _ cname, types ) -> String.join " " (cname :: List.map (formatType False) types)) constructors))

        Decl.Alias maybeDocs (A.At _ (Src.Alias (A.At _ name) args tipe)) ->
            formatMaybeDocs maybeDocs
                ++ "type alias "
                ++ name
                ++ String.concat (List.map (\(A.At _ arg) -> " " ++ arg) args)
                ++ " =\n"
                ++ indent (formatType False tipe)

        Decl.Port maybeDocs (Src.Port (A.At _ name) tipe) ->
            formatMaybeDocs maybeDocs
                ++ "port "
                ++ name
                ++ " : "
                ++ formatType False tipe


formatMaybeDocs : Maybe Src.Comment -> String
formatMaybeDocs maybeComment =
    case maybeComment of
        Just (Src.Comment comment) ->
            -- FIXME format comment
            "{-|" ++ Json.fromComment comment ++ "-}\n"

        Nothing ->
            ""


formatMaybeType : Name -> Maybe Src.Type -> String
formatMaybeType name maybeType =
    case maybeType of
        Just type_ ->
            name ++ " : " ++ formatType False type_ ++ "\n"

        Nothing ->
            ""


formatType : Bool -> Src.Type -> String
formatType groupingRequired (A.At (A.Region (A.Position startRow _) (A.Position endRow _)) type_) =
    case type_ of
        Src.TLambda arg result ->
            withGrouping groupingRequired (formatType True arg ++ " -> " ++ formatType False result)

        Src.TVar name ->
            name

        Src.TType _ name args ->
            withGrouping (groupingRequired && not (List.isEmpty args)) (String.join " " (name :: List.map (formatType True) args))

        Src.TTypeQual _ home name args ->
            withGrouping (groupingRequired && not (List.isEmpty args)) (String.join " " ((home ++ "." ++ name) :: List.map (formatType False) args))

        Src.TRecord [] Nothing ->
            "{}"

        Src.TRecord fields (Just (A.At _ ext)) ->
            "{ "
                ++ ext
                ++ " | "
                ++ String.join ", "
                    (List.map
                        (\( A.At _ fieldName, fieldType ) ->
                            fieldName ++ " : " ++ formatType False fieldType
                        )
                        fields
                    )
                ++ " }"

        Src.TRecord fields Nothing ->
            if endRow > startRow then
                "{ "
                    ++ String.join "\n, "
                        (List.map
                            (\( A.At _ fieldName, fieldType ) ->
                                fieldName ++ " : " ++ formatType False fieldType
                            )
                            fields
                        )
                    ++ "\n}"

            else
                "{ "
                    ++ String.join ", "
                        (List.map
                            (\( A.At _ fieldName, fieldType ) ->
                                fieldName ++ " : " ++ formatType False fieldType
                            )
                            fields
                        )
                    ++ " }"

        Src.TUnit ->
            "()"

        Src.TTuple a b cs ->
            "( " ++ String.join ", " (List.map (formatType False) (a :: b :: cs)) ++ " )"


formatPattern : Bool -> Src.Pattern -> String
formatPattern groupingRequired (A.At _ pattern) =
    case pattern of
        Src.PAnything name ->
            "_" ++ name

        Src.PVar name ->
            name

        Src.PRecord [] ->
            "{}"

        Src.PRecord fields ->
            "{ " ++ String.join ", " (List.map (\(A.At _ name) -> name) fields) ++ " }"

        Src.PAlias aliasPattern (A.At _ name) ->
            withGrouping groupingRequired (formatPattern groupingRequired aliasPattern ++ " as " ++ name)

        Src.PUnit ->
            "()"

        Src.PTuple a b cs ->
            "( " ++ String.join ", " (List.map (formatPattern False) (a :: b :: cs)) ++ " )"

        Src.PCtor _ name patterns ->
            withGrouping (groupingRequired && not (List.isEmpty patterns))
                (String.join " " (name :: List.map (formatPattern False) patterns))

        Src.PCtorQual _ home name patterns ->
            withGrouping (groupingRequired && not (List.isEmpty patterns))
                (String.join " " ((home ++ "." ++ name) :: List.map (formatPattern False) patterns))

        Src.PList [] ->
            "[]"

        Src.PList patterns ->
            "[ " ++ String.join ", " (List.map (formatPattern False) patterns) ++ " ]"

        Src.PCons hd tl ->
            withGrouping groupingRequired (formatPattern False hd ++ " :: " ++ formatPattern groupingRequired tl)

        Src.PChr chr ->
            "'" ++ chr ++ "'"

        Src.PStr str ->
            "\"" ++ str ++ "\""

        Src.PInt int ->
            String.fromInt int


formatExpr : Bool -> Src.Expr -> String
formatExpr groupingRequired (A.At (A.Region (A.Position startRow _) (A.Position endRow _)) expr) =
    case expr of
        Src.Chr chr ->
            "'" ++ chr ++ "'"

        Src.Str str ->
            "\"" ++ str ++ "\""

        Src.Int int ->
            String.fromInt int

        Src.Float float ->
            String.fromFloat float

        Src.Var _ name ->
            name

        Src.VarQual _ prefix name ->
            prefix ++ "." ++ name

        Src.List [] ->
            "[]"

        Src.List list ->
            if endRow > startRow then
                "[ " ++ String.join "\n, " (List.map (formatExpr False) list) ++ "\n]"

            else
                "[ " ++ String.join ", " (List.map (formatExpr False) list) ++ " ]"

        Src.Op op ->
            "(" ++ op ++ ")"

        Src.Negate subExpr ->
            "-" ++ formatExpr False subExpr

        Src.Binops ops final ->
            if endRow > startRow then
                String.join " "
                    (List.map
                        (\( opExpr, A.At _ op ) ->
                            formatExpr False opExpr ++ "\n" ++ indent op
                        )
                        ops
                        ++ [ formatExpr False final ]
                    )

            else
                String.join " "
                    (List.map
                        (\( opExpr, A.At _ op ) ->
                            formatExpr False opExpr ++ " " ++ op
                        )
                        ops
                        ++ [ formatExpr False final ]
                    )

        Src.Lambda srcArgs body ->
            withGrouping groupingRequired
                ("\\"
                    ++ String.join " " (List.map (formatPattern False) srcArgs)
                    ++ " -> "
                    ++ formatExpr False body
                )

        Src.Call func args ->
            withGrouping groupingRequired
                (List.map (formatExpr True) (func :: args)
                    |> String.join " "
                )

        Src.If branches finally ->
            String.join "\n\n"
                (List.map
                    (\( condition, body ) ->
                        "if "
                            ++ formatExpr False condition
                            ++ " then\n"
                            ++ indent (formatExpr False body)
                    )
                    branches
                    ++ [ "else\n" ++ indent (formatExpr False finally) ]
                )

        Src.Let defs letExpr ->
            "let\n"
                ++ indent (String.join "\n\n" (List.map formatDef defs))
                ++ "\nin\n"
                ++ formatExpr False letExpr

        Src.Case caseExpr branches ->
            "case "
                ++ formatExpr False caseExpr
                ++ " of\n"
                ++ indent
                    (String.join "\n\n"
                        (List.map
                            (\( pattern, branchExpr ) ->
                                formatPattern False pattern
                                    ++ " ->\n"
                                    ++ indent (formatExpr False branchExpr)
                            )
                            branches
                        )
                    )

        Src.Accessor field ->
            "." ++ field

        Src.Access record (A.At _ field) ->
            formatExpr False record ++ "." ++ field

        Src.Update name fields ->
            "{ "
                ++ formatExpr False name
                ++ " | "
                ++ String.join ", "
                    (List.map (\( A.At _ fieldName, value ) -> fieldName ++ " = " ++ formatExpr False value)
                        fields
                    )
                ++ " }"

        Src.Record [] ->
            "{}"

        Src.Record fields ->
            "{ "
                ++ String.join ", "
                    (List.map (\( A.At _ name, value ) -> name ++ " = " ++ formatExpr False value)
                        fields
                    )
                ++ " }"

        Src.Unit ->
            "()"

        Src.Tuple a b cs ->
            "( " ++ String.join ", " (List.map (formatExpr False) (a :: b :: cs)) ++ " )"

        Src.Shader src _ ->
            "[glsl|" ++ Shader.toJsStringBuilder src ++ "|]"


formatDef : A.Located Src.Def -> String
formatDef (A.At _ def) =
    case def of
        Src.Define (A.At _ name) srcArgs body maybeType ->
            formatMaybeType name maybeType
                ++ String.join " " (name :: List.map (formatPattern False) srcArgs ++ [ "=" ])
                ++ "\n"
                ++ indent (formatExpr False body)

        Src.Destruct pattern body ->
            formatPattern False pattern
                ++ " =\n"
                ++ indent (formatExpr False body)


indent : String -> String
indent src =
    src
        |> String.split "\n"
        |> List.map
            (\line ->
                if String.trim line == "" then
                    ""

                else
                    String.repeat 4 " " ++ String.trimRight line
            )
        |> String.join "\n"


withGrouping : Bool -> String -> String
withGrouping required str =
    if required then
        "(" ++ str ++ ")"

    else
        str
