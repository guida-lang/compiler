module Compiler.Reporting.Warning exposing
    ( Context(..)
    , Warning(..)
    , toReport
    )

import Compiler.AST.Utils.Type as Type
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Compiler.Reporting.Report exposing (Report(..))
import Types as T



-- ALL POSSIBLE WARNINGS


type Warning
    = UnusedImport T.CRA_Region T.CDN_Name
    | UnusedVariable T.CRA_Region Context T.CDN_Name
    | MissingTypeAnnotation T.CRA_Region T.CDN_Name T.CASTC_Type


type Context
    = Def
    | Pattern



-- TO REPORT


toReport : L.Localizer -> Code.Source -> Warning -> Report
toReport localizer source warning =
    case warning of
        UnusedImport region moduleName ->
            Report "unused import" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow ("Nothing from the `" ++ moduleName ++ "` module is used in this file.")
                    , D.fromChars "I recommend removing unused imports."
                    )

        UnusedVariable region context name ->
            let
                title : String
                title =
                    defOrPat context "unused definition" "unused variable"
            in
            Report title region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow ("You are not using `" ++ name ++ "` anywhere.")
                    , D.stack
                        [ D.reflow <|
                            "Is there a typo? Maybe you intended to use `"
                                ++ name
                                ++ "` somewhere but typed another name instead?"
                        , D.reflow <|
                            defOrPat context
                                "If you are sure there is no typo, remove the definition. This way future readers will not have to wonder why it is there!"
                                ("If you are sure there is no typo, replace `"
                                    ++ name
                                    ++ "` with _ so future readers will not have to wonder why it is there!"
                                )
                        ]
                    )

        MissingTypeAnnotation region name inferredType ->
            Report "missing type annotation" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        case Type.deepDealias inferredType of
                            T.CASTC_TLambda _ _ ->
                                "The `" ++ name ++ "` function has no type annotation."

                            _ ->
                                "The `" ++ name ++ "` definition has no type annotation."
                    , D.stack
                        [ D.fromChars "I inferred the type annotation myself though! You can copy it into your code:"
                        , D.green <|
                            D.hang 4 <|
                                D.sep
                                    [ D.fromName name |> D.a (D.fromChars " :")
                                    , RT.canToDoc localizer RT.None inferredType
                                    ]
                        ]
                    )


defOrPat : Context -> a -> a -> a
defOrPat context def pat =
    case context of
        Def ->
            def

        Pattern ->
            pat
