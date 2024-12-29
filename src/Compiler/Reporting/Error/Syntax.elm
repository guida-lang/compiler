module Compiler.Reporting.Error.Syntax exposing
    ( toReport
    , toSpaceReport
    )

import Compiler.Parse.Symbol
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Report as Report
import Hex
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T



-- TO REPORT


toReport : Code.Source -> T.CRES_Error -> Report.Report
toReport source err =
    case err of
        T.CRES_ModuleNameUnspecified name ->
            let
                region : T.CRA_Region
                region =
                    toRegion 1 1
            in
            Report.Report "MODULE NAME MISSING" region [] <|
                D.stack
                    [ D.reflow "I need the module name to be declared at the top of this file, like this:"
                    , D.indent 4 <|
                        D.fillSep <|
                            [ D.cyan (D.fromChars "module")
                            , D.fromName name
                            , D.cyan (D.fromChars "exposing")
                            , D.fromChars "(..)"
                            ]
                    , D.reflow <|
                        "Try adding that as the first line of your file!"
                    , D.toSimpleNote <|
                        "It is best to replace (..) with an explicit list of types and functions you want to expose. When you know a value is only used within this module, you can refactor without worrying about uses elsewhere. Limiting exposed values can also speed up compilation because I can skip a bunch of work if I see that the exposed API has not changed."
                    ]

        T.CRES_ModuleNameMismatch expectedName (T.CRA_At region actualName) ->
            Report.Report "MODULE NAME MISMATCH" region [ expectedName ] <|
                Code.toSnippet source region Nothing <|
                    ( D.fromChars "It looks like this module name is out of sync:"
                    , D.stack
                        [ D.reflow <|
                            "I need it to match the file path, so I was expecting to see `"
                                ++ expectedName
                                ++ "` here. Make the following change, and you should be all set!"
                        , D.indent 4 <|
                            (D.dullyellow (D.fromName actualName)
                                |> D.a (D.fromChars " -> ")
                                |> D.a (D.green (D.fromName expectedName))
                            )
                        , D.toSimpleNote <|
                            "I require that module names correspond to file paths. This makes it much easier to explore unfamiliar codebases! So if you want to keep the current module name, try renaming the file instead."
                        ]
                    )

        T.CRES_UnexpectedPort region ->
            Report.Report "UNEXPECTED PORTS" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "You are declaring ports in a normal module."
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Switch"
                            , D.fromChars "this"
                            , D.fromChars "to"
                            , D.fromChars "say"
                            , D.cyan (D.fromChars "port module")
                            , D.fromChars "instead,"
                            , D.fromChars "marking"
                            , D.fromChars "that"
                            , D.fromChars "this"
                            , D.fromChars "module"
                            , D.fromChars "contains"
                            , D.fromChars "port"
                            , D.fromChars "declarations."
                            ]
                        , D.link "Note"
                            "Ports are not a traditional FFI for calling JS functions directly. They need a different mindset! Read"
                            "ports"
                            "to learn the syntax and how to use it effectively."
                        ]
                    )

        T.CRES_NoPorts region ->
            Report.Report "NO PORTS" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "This module does not declare any ports, but it says it will:"
                    , D.fillSep
                        [ D.fromChars "Switch"
                        , D.fromChars "this"
                        , D.fromChars "to"
                        , D.cyan (D.fromChars "module")
                        , D.fromChars "and"
                        , D.fromChars "you"
                        , D.fromChars "should"
                        , D.fromChars "be"
                        , D.fromChars "all"
                        , D.fromChars "set!"
                        ]
                    )

        T.CRES_NoPortsInPackage (T.CRA_At region _) ->
            Report.Report "PACKAGES CANNOT HAVE PORTS" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "Packages cannot declare any ports, so I am getting stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "Remove this port declaration."
                        , noteForPortsInPackage
                        ]
                    )

        T.CRES_NoPortModulesInPackage region ->
            Report.Report "PACKAGES CANNOT HAVE PORTS" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "Packages cannot declare any ports, so I am getting stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.fromChars "Remove"
                            , D.fromChars "the"
                            , D.cyan (D.fromChars "port")
                            , D.fromChars "keyword"
                            , D.fromChars "and"
                            , D.fromChars "I"
                            , D.fromChars "should"
                            , D.fromChars "be"
                            , D.fromChars "able"
                            , D.fromChars "to"
                            , D.fromChars "continue."
                            ]
                        , noteForPortsInPackage
                        ]
                    )

        T.CRES_NoEffectsOutsideKernel region ->
            Report.Report "INVALID EFFECT MODULE" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "It is not possible to declare an `effect module` outside the @elm organization, so I am getting stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "Switch to a normal module declaration."
                        , D.toSimpleNote <|
                            "Effect modules are designed to allow certain core functionality to be defined separately from the compiler. So the @elm organization has access to this so that certain changes, extensions, and fixes can be introduced without needing to release new Elm binaries. For example, we want to make it possible to test effects, but this may require changes to the design of effect modules. By only having them defined in the @elm organization, that kind of design work can proceed much more smoothly."
                        ]
                    )

        T.CRES_ParseError modul ->
            toParseErrorReport source modul


noteForPortsInPackage : D.Doc
noteForPortsInPackage =
    D.stack
        [ D.toSimpleNote <|
            "One of the major goals of the package ecosystem is to be completely written in Elm. This means when you install an Elm package, you can be sure you are safe from security issues on install and that you are not going to get any runtime exceptions coming from your new dependency. This design also sets the ecosystem up to target other platforms more easily (like mobile phones, WebAssembly, etc.) since no community code explicitly depends on JavaScript even existing."
        , D.reflow <|
            "Given that overall goal, allowing ports in packages would lead to some pretty surprising behavior. If ports were allowed in packages, you could install a package but not realize that it brings in an indirect dependency that defines a port. Now you have a program that does not work and the fix is to realize that some JavaScript needs to be added for a dependency you did not even know about. That would be extremely frustrating! \"So why not allow the package author to include the necessary JS code as well?\" Now we are back in conflict with our overall goal to keep all community packages free from runtime exceptions."
        ]


toParseErrorReport : Code.Source -> T.CRES_Module -> Report.Report
toParseErrorReport source modul =
    case modul of
        T.CRES_ModuleSpace space row col ->
            toSpaceReport source space row col

        T.CRES_ModuleBadEnd row col ->
            if col == 1 then
                toDeclStartReport source row col

            else
                toWeirdEndReport source row col

        T.CRES_ModuleProblem row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED MODULE DECLARATION" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I am parsing an `module` declaration, but I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "Here are some examples of valid `module` declarations:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep
                                    [ D.cyan (D.fromChars "module")
                                    , D.fromChars "Main"
                                    , D.cyan (D.fromChars "exposing")
                                    , D.fromChars "(..)"
                                    ]
                                , D.fillSep
                                    [ D.cyan (D.fromChars "module")
                                    , D.fromChars "Dict"
                                    , D.cyan (D.fromChars "exposing")
                                    , D.fromChars "(Dict, empty, get)"
                                    ]
                                ]
                        , D.reflow <|
                            "I generally recommend using an explicit exposing list. I can skip compiling a bunch of files when the public interface of a module stays the same, so exposing fewer values can help improve compile times!"
                        ]
                    )

        T.CRES_ModuleName row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING MODULE NAME" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was parsing an `module` declaration until I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see the module name next, like in these examples:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep
                                    [ D.cyan (D.fromChars "module")
                                    , D.fromChars "Dict"
                                    , D.cyan (D.fromChars "exposing")
                                    , D.fromChars "(..)"
                                    ]
                                , D.fillSep
                                    [ D.cyan (D.fromChars "module")
                                    , D.fromChars "Maybe"
                                    , D.cyan (D.fromChars "exposing")
                                    , D.fromChars "(..)"
                                    ]
                                , D.fillSep
                                    [ D.cyan (D.fromChars "module")
                                    , D.fromChars "Html.Attributes"
                                    , D.cyan (D.fromChars "exposing")
                                    , D.fromChars "(..)"
                                    ]
                                , D.fillSep
                                    [ D.cyan (D.fromChars "module")
                                    , D.fromChars "Json.Decode"
                                    , D.cyan (D.fromChars "exposing")
                                    , D.fromChars "(..)"
                                    ]
                                ]
                        , D.reflow <|
                            "Notice that the module names all start with capital letters. That is required!"
                        ]
                    )

        T.CRES_ModuleExposing exposing_ row col ->
            toExposingReport source exposing_ row col

        T.CRES_PortModuleProblem row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PORT MODULE DECLARATION" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I am parsing an `port module` declaration, but I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "Here are some examples of valid `port module` declarations:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep
                                    [ D.cyan (D.fromChars "port")
                                    , D.cyan (D.fromChars "module")
                                    , D.fromChars "WebSockets"
                                    , D.cyan (D.fromChars "exposing")
                                    , D.fromChars "(send, listen, keepAlive)"
                                    ]
                                , D.fillSep
                                    [ D.cyan (D.fromChars "port")
                                    , D.cyan (D.fromChars "module")
                                    , D.fromChars "Maps"
                                    , D.cyan (D.fromChars "exposing")
                                    , D.fromChars "(Location, goto)"
                                    ]
                                ]
                        , D.link "Note" "Read" "ports" "for more help."
                        ]
                    )

        T.CRES_PortModuleName row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING MODULE NAME" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was parsing an `module` declaration until I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see the module name next, like in these examples:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep
                                    [ D.cyan (D.fromChars "port")
                                    , D.cyan (D.fromChars "module")
                                    , D.fromChars "WebSockets"
                                    , D.cyan (D.fromChars "exposing")
                                    , D.fromChars "(send, listen, keepAlive)"
                                    ]
                                , D.fillSep
                                    [ D.cyan (D.fromChars "port")
                                    , D.cyan (D.fromChars "module")
                                    , D.fromChars "Maps"
                                    , D.cyan (D.fromChars "exposing")
                                    , D.fromChars "(Location, goto)"
                                    ]
                                ]
                        , D.reflow <|
                            "Notice that the module names start with capital letters. That is required!"
                        ]
                    )

        T.CRES_PortModuleExposing exposing_ row col ->
            toExposingReport source exposing_ row col

        T.CRES_Effect row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "BAD MODULE DECLARATION" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I cannot parse this module declaration:"
                    , D.reflow <|
                        "This type of module is reserved for the @elm organization. It is used to define certain effects, avoiding building them into the compiler."
                    )

        T.CRES_FreshLine row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col

                toBadFirstLineReport : String -> Report.Report
                toBadFirstLineReport keyword =
                    Report.Report "TOO MUCH INDENTATION" region [] <|
                        Code.toSnippet source region Nothing <|
                            ( D.reflow <|
                                "This `"
                                    ++ keyword
                                    ++ "` should not have any spaces before it:"
                            , D.reflow <|
                                "Delete the spaces before `"
                                    ++ keyword
                                    ++ "` until there are none left!"
                            )
            in
            case Code.whatIsNext source row col of
                Code.Keyword "module" ->
                    toBadFirstLineReport "module"

                Code.Keyword "import" ->
                    toBadFirstLineReport "import"

                Code.Keyword "type" ->
                    toBadFirstLineReport "type"

                Code.Keyword "port" ->
                    toBadFirstLineReport "port"

                _ ->
                    Report.Report "SYNTAX PROBLEM" region [] <|
                        Code.toSnippet source region Nothing <|
                            ( D.reflow <|
                                "I got stuck here:"
                            , D.stack
                                [ D.reflow <|
                                    "I am not sure what is going on, but I recommend starting an Elm file with the following lines:"
                                , D.indent 4 <|
                                    D.vcat <|
                                        [ D.fillSep [ D.cyan (D.fromChars "import"), D.fromChars "Html" ]
                                        , D.fromChars ""
                                        , D.fromChars "main ="
                                        , D.fromChars "  Html.text "
                                            |> D.a (D.dullyellow (D.fromChars "\"Hello!\""))
                                        ]
                                , D.reflow <|
                                    "You should be able to copy those lines directly into your file. Check out the examples at <https://elm-lang.org/examples> for more help getting started!"
                                , D.toSimpleNote <|
                                    "This can also happen when something is indented too much!"
                                ]
                            )

        T.CRES_ImportStart row col ->
            toImportReport source row col

        T.CRES_ImportName row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING IMPORT NAME" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was parsing an `import` until I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see a module name next, like in these examples:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep
                                    [ D.cyan (D.fromChars "import")
                                    , D.fromChars "Dict"
                                    ]
                                , D.fillSep
                                    [ D.cyan (D.fromChars "import")
                                    , D.fromChars "Maybe"
                                    ]
                                , D.fillSep
                                    [ D.cyan (D.fromChars "import")
                                    , D.fromChars "Html.Attributes"
                                    , D.cyan (D.fromChars "as")
                                    , D.fromChars "A"
                                    ]
                                , D.fillSep
                                    [ D.cyan (D.fromChars "import")
                                    , D.fromChars "Json.Decode"
                                    , D.cyan (D.fromChars "exposing")
                                    , D.fromChars "(..)"
                                    ]
                                ]
                        , D.reflow <|
                            "Notice that the module names all start with capital letters. That is required!"
                        , D.reflowLink "Read" "imports" "to learn more."
                        ]
                    )

        T.CRES_ImportAs row col ->
            toImportReport source row col

        T.CRES_ImportAlias row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING IMPORT ALIAS" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was parsing an `import` until I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see an alias next, like in these examples:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep
                                    [ D.cyan <| D.fromChars "import"
                                    , D.fromChars "Html.Attributes"
                                    , D.cyan <| D.fromChars "as"
                                    , D.fromChars "Attr"
                                    ]
                                , D.fillSep
                                    [ D.cyan <| D.fromChars "import"
                                    , D.fromChars "WebGL.Texture"
                                    , D.cyan <| D.fromChars "as"
                                    , D.fromChars "Texture"
                                    ]
                                , D.fillSep
                                    [ D.cyan <| D.fromChars "import"
                                    , D.fromChars "Json.Decode"
                                    , D.cyan <| D.fromChars "as"
                                    , D.fromChars "D"
                                    ]
                                ]
                        , D.reflow <|
                            "Notice that the alias always starts with a capital letter. That is required!"
                        , D.reflowLink "Read" "imports" "to learn more."
                        ]
                    )

        T.CRES_ImportExposing row col ->
            toImportReport source row col

        T.CRES_ImportExposingList exposing_ row col ->
            toExposingReport source exposing_ row col

        T.CRES_ImportEnd row col ->
            toImportReport source row col

        T.CRES_ImportIndentName row col ->
            toImportReport source row col

        T.CRES_ImportIndentAlias row col ->
            toImportReport source row col

        T.CRES_ImportIndentExposingList row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IMPORT" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was parsing an `import` until I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see the list of exposed values next. For example, here are two ways to expose values from the `Html` module:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep
                                    [ D.cyan <| D.fromChars "import"
                                    , D.fromChars "Html"
                                    , D.cyan <| D.fromChars "exposing"
                                    , D.fromChars "(..)"
                                    ]
                                , D.fillSep
                                    [ D.cyan <| D.fromChars "import"
                                    , D.fromChars "Html"
                                    , D.cyan <| D.fromChars "exposing"
                                    , D.fromChars "(Html, div, text)"
                                    ]
                                ]
                        , D.reflow <|
                            "I generally recommend the second style. It is more explicit, making it much easier to figure out where values are coming from in large projects!"
                        ]
                    )

        T.CRES_Infix row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "BAD INFIX" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "Something went wrong in this infix operator declaration:"
                    , D.reflow <|
                        "This feature is used by the @elm organization to define the languages built-in operators."
                    )

        T.CRES_Declarations decl _ _ ->
            toDeclarationsReport source decl



-- WEIRD END


toWeirdEndReport : Code.Source -> T.CPP_Row -> T.CPP_Col -> Report.Report
toWeirdEndReport source row col =
    case Code.whatIsNext source row col of
        Code.Keyword keyword ->
            let
                region : T.CRA_Region
                region =
                    toKeywordRegion row col keyword
            in
            Report.Report "RESERVED WORD" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I got stuck on this reserved word:"
                    , D.reflow <|
                        ("The name `" ++ keyword ++ "` is reserved, so try using a different name?")
                    )

        Code.Operator op ->
            let
                region : T.CRA_Region
                region =
                    toKeywordRegion row col op
            in
            Report.Report "UNEXPECTED SYMBOL" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I ran into an unexpected symbol:"
                    , D.reflow <|
                        ("I was not expecting to see a " ++ op ++ " here. Try deleting it? Maybe I can give a better hint from there?")
                    )

        Code.Close term bracket ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report ("UNEXPECTED " ++ String.toUpper term) region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow ("I ran into an unexpected " ++ term ++ ":")
                    , D.reflow ("This " ++ String.fromChar bracket ++ " does not match up with an earlier open " ++ term ++ ". Try deleting it?")
                    )

        Code.Lower c cs ->
            let
                region : T.CRA_Region
                region =
                    toKeywordRegion row col (String.cons c cs)
            in
            Report.Report "UNEXPECTED NAME" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I got stuck on this name:"
                    , D.reflow "It is confusing me a lot! Normally I can give fairly specific hints, but something is really tripping me up this time."
                    )

        Code.Upper c cs ->
            let
                region : T.CRA_Region
                region =
                    toKeywordRegion row col (String.fromChar c ++ cs)
            in
            Report.Report "UNEXPECTED NAME" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I got stuck on this name:"
                    , D.reflow "It is confusing me a lot! Normally I can give fairly specific hints, but something is really tripping me up this time."
                    )

        Code.Other maybeChar ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            case maybeChar of
                Just ';' ->
                    Report.Report "UNEXPECTED SEMICOLON" region [] <|
                        Code.toSnippet source region Nothing <|
                            ( D.reflow "I got stuck on this semicolon:"
                            , D.stack
                                [ D.reflow "Try removing it?"
                                , D.toSimpleNote "Some languages require semicolons at the end of each statement. These are often called C-like languages, and they usually share a lot of language design choices. (E.g. side-effects, for loops, etc.) Elm manages effects with commands and subscriptions instead, so there is no special syntax for \"statements\" and therefore no need to use semicolons to separate them. I think this will make more sense as you work through <https://guide.elm-lang.org> though!"
                                ]
                            )

                Just ',' ->
                    Report.Report "UNEXPECTED COMMA" region [] <|
                        Code.toSnippet source region Nothing <|
                            ( D.reflow "I got stuck on this comma:"
                            , D.stack
                                [ D.reflow "I do not think I am parsing a list or tuple right now. Try deleting the comma?"
                                , D.toSimpleNote "If this is supposed to be part of a list, the problem may be a bit earlier. Perhaps the opening [ is missing? Or perhaps some value in the list has an extra closing ] that is making me think the list ended earlier? The same kinds of things could be going wrong if this is supposed to be a tuple."
                                ]
                            )

                Just '`' ->
                    Report.Report "UNEXPECTED CHARACTER" region [] <|
                        Code.toSnippet source region Nothing <|
                            ( D.reflow "I got stuck on this character:"
                            , D.stack
                                [ D.reflow "It is not used for anything in Elm syntax. It is used for multi-line strings in some languages though, so if you want a string that spans multiple lines, you can use Elm's multi-line string syntax like this:"
                                , D.dullyellow <|
                                    D.indent 4 <|
                                        D.vcat
                                            [ D.fromChars "\"\"\""
                                            , D.fromChars "# Multi-line Strings"
                                            , D.fromChars ""
                                            , D.fromChars "- start with triple double quotes"
                                            , D.fromChars "- write whatever you want"
                                            , D.fromChars "- no need to escape newlines or double quotes"
                                            , D.fromChars "- end with triple double quotes"
                                            , D.fromChars "\"\"\""
                                            ]
                                , D.reflow "Otherwise I do not know what is going on! Try removing the character?"
                                ]
                            )

                Just '$' ->
                    Report.Report "UNEXPECTED SYMBOL" region [] <|
                        Code.toSnippet source region Nothing <|
                            ( D.reflow "I got stuck on this dollar sign:"
                            , D.reflow "It is not used for anything in Elm syntax. Are you coming from a language where dollar signs can be used in variable names? If so, try a name that (1) starts with a letter and (2) only contains letters, numbers, and underscores."
                            )

                Just c ->
                    if List.member c [ '#', '@', '!', '%', '~' ] then
                        Report.Report "UNEXPECTED SYMBOL" region [] <|
                            Code.toSnippet source region Nothing <|
                                ( D.reflow "I got stuck on this symbol:"
                                , D.reflow "It is not used for anything in Elm syntax. Try removing it?"
                                )

                    else
                        toWeirdEndSyntaxProblemReport source region

                _ ->
                    toWeirdEndSyntaxProblemReport source region


toWeirdEndSyntaxProblemReport : Code.Source -> T.CRA_Region -> Report.Report
toWeirdEndSyntaxProblemReport source region =
    Report.Report "SYNTAX PROBLEM" region [] <|
        Code.toSnippet source region Nothing <|
            ( D.reflow "I got stuck here:"
            , D.reflow "Whatever I am running into is confusing me a lot! Normally I can give fairly specific hints, but something is really tripping me up this time."
            )



-- IMPORTS


toImportReport : Code.Source -> T.CPP_Row -> T.CPP_Col -> Report.Report
toImportReport source row col =
    let
        region : T.CRA_Region
        region =
            toRegion row col
    in
    Report.Report "UNFINISHED IMPORT" region [] <|
        Code.toSnippet source region Nothing <|
            ( D.reflow "I am partway through parsing an import, but I got stuck here:"
            , D.stack
                [ D.reflow "Here are some examples of valid `import` declarations:"
                , D.indent 4 <|
                    D.vcat
                        [ D.fillSep
                            [ D.cyan <| D.fromChars "import"
                            , D.fromChars "Html"
                            ]
                        , D.fillSep
                            [ D.cyan <| D.fromChars "import"
                            , D.fromChars "Html"
                            , D.cyan <| D.fromChars "as"
                            , D.fromChars "H"
                            ]
                        , D.fillSep
                            [ D.cyan <| D.fromChars "import"
                            , D.fromChars "Html"
                            , D.cyan <| D.fromChars "as"
                            , D.fromChars "H"
                            , D.cyan <| D.fromChars "exposing"
                            , D.fromChars "(..)"
                            ]
                        , D.fillSep
                            [ D.cyan <| D.fromChars "import"
                            , D.fromChars "Html"
                            , D.cyan <| D.fromChars "exposing"
                            , D.fromChars "(Html, div, text)"
                            ]
                        ]
                , D.reflow "You are probably trying to import a different module, but try to make it look like one of these examples!"
                , D.reflowLink "Read" "imports" "to learn more."
                ]
            )



-- EXPOSING


toExposingReport : Code.Source -> T.CRES_Exposing -> T.CPP_Row -> T.CPP_Col -> Report.Report
toExposingReport source exposing_ startRow startCol =
    case exposing_ of
        T.CRES_ExposingSpace space row col ->
            toSpaceReport source space row col

        T.CRES_ExposingStart row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN EXPOSING" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I want to parse exposed values, but I am getting stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Exposed"
                            , D.fromChars "values"
                            , D.fromChars "are"
                            , D.fromChars "always"
                            , D.fromChars "surrounded"
                            , D.fromChars "by"
                            , D.fromChars "parentheses."
                            , D.fromChars "So"
                            , D.fromChars "try"
                            , D.fromChars "adding"
                            , D.fromChars "a"
                            , D.green (D.fromChars "(")
                            , D.fromChars "here?"
                            ]
                        , D.toSimpleNote "Here are some valid examples of `exposing` for reference:"
                        , D.indent 4 <|
                            D.vcat
                                [ D.fillSep
                                    [ D.cyan <| D.fromChars "import"
                                    , D.fromChars "Html"
                                    , D.cyan <| D.fromChars "exposing"
                                    , D.fromChars "(..)"
                                    ]
                                , D.fillSep
                                    [ D.cyan <| D.fromChars "import"
                                    , D.fromChars "Html"
                                    , D.cyan <| D.fromChars "exposing"
                                    , D.fromChars "(Html, div, text)"
                                    ]
                                ]
                        , D.reflow "If you are getting tripped up, you can just expose everything for now. It should get easier to make an explicit exposing list as you see more examples in the wild."
                        ]
                    )

        T.CRES_ExposingValue row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I got stuck on this reserved word:"
                            , D.reflow ("It looks like you are trying to expose `" ++ keyword ++ "` but that is a reserved word. Is there a typo?")
                            )

                Code.Operator op ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col op
                    in
                    Report.Report "UNEXPECTED SYMBOL" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I got stuck on this symbol:"
                            , D.stack
                                [ D.reflow "If you are trying to expose an operator, add parentheses around it like this:"
                                , D.indent 4 <|
                                    (D.dullyellow (D.fromChars op)
                                        |> D.a (D.fromChars " -> ")
                                        |> D.a
                                            (D.green
                                                (D.fromChars "("
                                                    |> D.a (D.fromChars op)
                                                    |> D.a (D.fromChars ")")
                                                )
                                            )
                                    )
                                ]
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN EXPOSING" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I got stuck while parsing these exposed values:"
                            , D.stack
                                [ D.reflow "I do not have an exact recommendation, so here are some valid examples of `exposing` for reference:"
                                , D.indent 4 <|
                                    D.vcat
                                        [ D.fillSep
                                            [ D.cyan (D.fromChars "import")
                                            , D.fromChars "Html"
                                            , D.cyan (D.fromChars "exposing")
                                            , D.fromChars "(..)"
                                            ]
                                        , D.fillSep
                                            [ D.cyan <| D.fromChars "import"
                                            , D.fromChars "Basics"
                                            , D.cyan <| D.fromChars "exposing"
                                            , D.fromChars "(Int, Float, Bool(..), (+), not, sqrt)"
                                            ]
                                        ]
                                , D.reflow "These examples show how to expose types, variants, operators, and functions. Everything should be some permutation of these examples, just with different names."
                                ]
                            )

        T.CRES_ExposingOperator row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN EXPOSING" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I just saw an open parenthesis, so I was expecting an operator next:"
                    , D.fillSep
                        [ D.fromChars "It"
                        , D.fromChars "is"
                        , D.fromChars "possible"
                        , D.fromChars "to"
                        , D.fromChars "expose"
                        , D.fromChars "operators,"
                        , D.fromChars "so"
                        , D.fromChars "I"
                        , D.fromChars "was"
                        , D.fromChars "expecting"
                        , D.fromChars "to"
                        , D.fromChars "see"
                        , D.fromChars "something"
                        , D.fromChars "like"
                        , D.dullyellow <| D.fromChars "(+)"
                        , D.fromChars "or"
                        , D.dullyellow <| D.fromChars "(|=)"
                        , D.fromChars "or"
                        , D.dullyellow <| D.fromChars "(||)"
                        , D.fromChars "after"
                        , D.fromChars "I"
                        , D.fromChars "saw"
                        , D.fromChars "that"
                        , D.fromChars "open"
                        , D.fromChars "parenthesis."
                        ]
                    )

        T.CRES_ExposingOperatorReserved op row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "RESERVED SYMBOL" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I cannot expose this as an operator:"
                    , case op of
                        T.CPS_BadDot ->
                            D.reflow "Try getting rid of this entry? Maybe I can give you a better hint after that?"

                        T.CPS_BadPipe ->
                            D.fillSep
                                [ D.fromChars "Maybe"
                                , D.fromChars "you"
                                , D.fromChars "want"
                                , D.dullyellow <| D.fromChars "(||)"
                                , D.fromChars "instead?"
                                ]

                        T.CPS_BadArrow ->
                            D.reflow "Try getting rid of this entry? Maybe I can give you a better hint after that?"

                        T.CPS_BadEquals ->
                            D.fillSep
                                [ D.fromChars "Maybe"
                                , D.fromChars "you"
                                , D.fromChars "want"
                                , D.dullyellow <| D.fromChars "(==)"
                                , D.fromChars "instead?"
                                ]

                        T.CPS_BadHasType ->
                            D.fillSep
                                [ D.fromChars "Maybe"
                                , D.fromChars "you"
                                , D.fromChars "want"
                                , D.dullyellow <| D.fromChars "(::)"
                                , D.fromChars "instead?"
                                ]
                    )

        T.CRES_ExposingOperatorRightParen row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN EXPOSING" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "It looks like you are exposing an operator, but I got stuck here:"
                    , D.fillSep
                        [ D.fromChars "I"
                        , D.fromChars "was"
                        , D.fromChars "expecting"
                        , D.fromChars "to"
                        , D.fromChars "see"
                        , D.fromChars "the"
                        , D.fromChars "closing"
                        , D.fromChars "parenthesis"
                        , D.fromChars "immediately"
                        , D.fromChars "after"
                        , D.fromChars "the"
                        , D.fromChars "operator."
                        , D.fromChars "Try"
                        , D.fromChars "adding"
                        , D.fromChars "a"
                        , D.green <| D.fromChars ")"
                        , D.fromChars "right"
                        , D.fromChars "here?"
                        ]
                    )

        T.CRES_ExposingEnd row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED EXPOSING" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was partway through parsing exposed values, but I got stuck here:"
                    , D.reflow "Maybe there is a comma missing before this?"
                    )

        T.CRES_ExposingTypePrivacy row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "PROBLEM EXPOSING CUSTOM TYPE VARIANTS" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "It looks like you are trying to expose the variants of a custom type:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "You"
                            , D.fromChars "need"
                            , D.fromChars "to"
                            , D.fromChars "write"
                            , D.fromChars "something"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "Status(..)"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "Entity(..)"
                            , D.fromChars "though."
                            , D.fromChars "It"
                            , D.fromChars "is"
                            , D.fromChars "all"
                            , D.fromChars "or"
                            , D.fromChars "nothing,"
                            , D.fromChars "otherwise"
                            , D.fromChars "`case`"
                            , D.fromChars "expressions"
                            , D.fromChars "could"
                            , D.fromChars "miss"
                            , D.fromChars "a"
                            , D.fromChars "variant"
                            , D.fromChars "and"
                            , D.fromChars "crash!"
                            ]
                        , D.toSimpleNote <|
                            "It is often best to keep the variants hidden! If someone pattern matches on the variants, it is a MAJOR change if any new variants are added. Suddenly their `case` expressions do not cover all variants! So if you do not need people to pattern match, keep the variants hidden and expose functions to construct values of this type. This way you can add new variants as a MINOR change!"
                        ]
                    )

        T.CRES_ExposingIndentEnd row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED EXPOSING" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was partway through parsing exposed values, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "a"
                            , D.fromChars "closing"
                            , D.fromChars "parenthesis."
                            , D.fromChars "Try"
                            , D.fromChars "adding"
                            , D.fromChars "a"
                            , D.green <| D.fromChars ")"
                            , D.fromChars "right"
                            , D.fromChars "here?"
                            ]
                        , D.toSimpleNote <|
                            "I can get confused when there is not enough indentation, so if you already have a closing parenthesis, it probably just needs some spaces in front of it."
                        ]
                    )

        T.CRES_ExposingIndentValue row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED EXPOSING" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was partway through parsing exposed values, but I got stuck here:"
                    , D.reflow "I was expecting another value to expose."
                    )



-- SPACES


toSpaceReport : Code.Source -> T.CRES_Space -> T.CPP_Row -> T.CPP_Col -> Report.Report
toSpaceReport source space row col =
    case space of
        T.CRES_HasTab ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "NO TABS" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I ran into a tab, but tabs are not allowed in Elm files."
                    , D.reflow "Replace the tab with spaces."
                    )

        T.CRES_EndlessMultiComment ->
            let
                region : T.CRA_Region
                region =
                    toWiderRegion row col 2
            in
            Report.Report "ENDLESS COMMENT" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I cannot find the end of this multi-line comment:"
                    , D.stack
                        -- "{-"
                        [ D.reflow "Add a -} somewhere after this to end the comment."
                        , D.toSimpleHint "Multi-line comments can be nested in Elm, so {- {- -} -} is a comment that happens to contain another comment. Like parentheses and curly braces, the start and end markers must always be balanced. Maybe that is the problem?"
                        ]
                    )



-- DECLARATIONS


toRegion : T.CPP_Row -> T.CPP_Col -> T.CRA_Region
toRegion row col =
    let
        pos : T.CRA_Position
        pos =
            T.CRA_Position row col
    in
    T.CRA_Region pos pos


toWiderRegion : T.CPP_Row -> T.CPP_Col -> Int -> T.CRA_Region
toWiderRegion row col extra =
    T.CRA_Region
        (T.CRA_Position row col)
        (T.CRA_Position row (col + extra))


toKeywordRegion : T.CPP_Row -> T.CPP_Col -> String -> T.CRA_Region
toKeywordRegion row col keyword =
    T.CRA_Region
        (T.CRA_Position row col)
        (T.CRA_Position row (col + String.length keyword))


toDeclarationsReport : Code.Source -> T.CRES_Decl -> Report.Report
toDeclarationsReport source decl =
    case decl of
        T.CRES_DeclStart row col ->
            toDeclStartReport source row col

        T.CRES_DeclSpace space row col ->
            toSpaceReport source space row col

        T.CRES_Port port_ row col ->
            toPortReport source port_ row col

        T.CRES_DeclType declType row col ->
            toDeclTypeReport source declType row col

        T.CRES_DeclDef name declDef row col ->
            toDeclDefReport source name declDef row col

        T.CRES_DeclFreshLineAfterDocComment row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING DECLARATION" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I just saw a doc comment, but then I got stuck here:"
                    , D.reflow "I was expecting to see the corresponding declaration next, starting on a fresh line with no indentation."
                    )


toDeclStartReport : Code.Source -> T.CPP_Row -> T.CPP_Col -> Report.Report
toDeclStartReport source row col =
    case Code.whatIsNext source row col of
        Code.Close term bracket ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report ("STRAY " ++ String.toUpper term) region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow ("I was not expecting to see a " ++ term ++ " here:")
                    , D.reflow ("This " ++ String.fromChar bracket ++ " does not match up with an earlier open " ++ term ++ ". Try deleting it?")
                    )

        Code.Keyword keyword ->
            let
                region : T.CRA_Region
                region =
                    toKeywordRegion row col keyword
            in
            Report.Report "RESERVED WORD" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow ("I was not expecting to run into the `" ++ keyword ++ "` keyword here:")
                    , case keyword of
                        "import" ->
                            D.reflow "It is reserved for declaring imports at the top of your module. If you want another import, try moving it up top with the other imports. If you want to define a value or function, try changing the name to something else!"

                        "case" ->
                            D.stack
                                [ D.reflow "It is reserved for writing `case` expressions. Try using a different name?"
                                , D.toSimpleNote "If you are trying to write a `case` expression, it needs to be part of a definition. So you could write something like this instead:"
                                , D.indent 4 <|
                                    D.vcat
                                        [ D.indent 0 <| D.fillSep [ D.fromChars "getWidth", D.fromChars "maybeWidth", D.fromChars "=" ]
                                        , D.indent 2 <| D.fillSep [ D.cyan (D.fromChars "case"), D.fromChars "maybeWidth", D.cyan (D.fromChars "of") ]
                                        , D.indent 4 <| D.fillSep [ D.blue (D.fromChars "Just"), D.fromChars "width", D.fromChars "->" ]
                                        , D.indent 6 <| D.fillSep [ D.fromChars "width", D.fromChars "+", D.dullyellow (D.fromChars "200") ]
                                        , D.fromChars ""
                                        , D.indent 4 <| D.fillSep [ D.blue (D.fromChars "Nothing"), D.fromChars "->" ]
                                        , D.indent 6 <| D.fillSep [ D.dullyellow (D.fromChars "400") ]
                                        ]
                                , D.reflow "This defines a `getWidth` function that you can use elsewhere in your program."
                                ]

                        "if" ->
                            D.stack
                                [ D.reflow "It is reserved for writing `if` expressions. Try using a different name?"
                                , D.toSimpleNote "If you are trying to write an `if` expression, it needs to be part of a definition. So you could write something like this instead:"
                                , D.indent 4 <|
                                    D.vcat
                                        [ D.fromChars "greet name ="
                                        , D.fillSep
                                            [ D.fromChars " "
                                            , D.cyan <| D.fromChars "if"
                                            , D.fromChars "name"
                                            , D.fromChars "=="
                                            , D.dullyellow <| D.fromChars "\"Abraham Lincoln\""
                                            , D.cyan <| D.fromChars "then"
                                            , D.dullyellow <| D.fromChars "\"Greetings Mr. President.\""
                                            , D.cyan <| D.fromChars "else"
                                            , D.dullyellow <| D.fromChars "\"Hey!\""
                                            ]
                                        ]
                                , D.reflow "This defines a `reviewPowerLevel` function that you can use elsewhere in your program."
                                ]

                        _ ->
                            D.reflow "It is a reserved word. Try changing the name to something else?"
                    )

        Code.Upper c cs ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED CAPITAL LETTER" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "Declarations always start with a lower-case letter, so I am getting stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Try"
                            , D.fromChars "a"
                            , D.fromChars "name"
                            , D.fromChars "like"
                            , D.green (D.fromChars (String.cons (Char.toLower c) cs))
                            , D.fromChars "instead?"
                            ]
                        , D.toSimpleNote "Here are a couple valid declarations for reference:"
                        , D.indent 4 <|
                            D.vcat
                                [ D.fromChars "greet : String -> String"
                                , D.fromChars "greet name ="
                                , D.fromChars "  "
                                    |> D.a (D.dullyellow (D.fromChars "\"Hello \""))
                                    |> D.a (D.fromChars " ++ name ++ ")
                                    |> D.a (D.dullyellow (D.fromChars "\"!\""))
                                , D.fromChars ""
                                , D.cyan (D.fromChars "type" |> D.a (D.fromChars " User = Anonymous | LoggedIn String"))
                                ]
                        , D.reflow "Notice that they always start with a lower-case letter. Capitalization matters!"
                        ]
                    )

        Code.Other (Just char) ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            if List.member char [ '(', '{', '[', '+', '-', '*', '/', '^', '&', '|', '"', '\'', '!', '@', '#', '$', '%' ] then
                Report.Report "UNEXPECTED SYMBOL" region [] <|
                    Code.toSnippet source region Nothing <|
                        ( D.reflow ("I am getting stuck because this line starts with the " ++ String.fromChar char ++ " symbol:")
                        , D.stack
                            [ D.reflow "When a line has no spaces at the beginning, I expect it to be a declaration like one of these:"
                            , D.indent 4 <|
                                D.vcat
                                    [ D.fromChars "greet : String -> String"
                                    , D.fromChars "greet name ="
                                    , D.fromChars "  "
                                        |> D.a (D.dullyellow (D.fromChars "\"Hello \""))
                                        |> D.a (D.fromChars " ++ name ++ ")
                                        |> D.a (D.dullyellow (D.fromChars "\"!\""))
                                    , D.fromChars ""
                                    , D.cyan (D.fromChars "type")
                                        |> D.a (D.fromChars " User = Anonymous | LoggedIn String")
                                    ]
                            , D.reflow "If this is not supposed to be a declaration, try adding some spaces before it?"
                            ]
                        )

            else
                toDeclStartWeirdDeclarationReport source region

        _ ->
            toDeclStartWeirdDeclarationReport source (toRegion row col)


toDeclStartWeirdDeclarationReport : Code.Source -> T.CRA_Region -> Report.Report
toDeclStartWeirdDeclarationReport source region =
    Report.Report "WEIRD DECLARATION" region [] <|
        Code.toSnippet source region Nothing <|
            ( D.reflow "I am trying to parse a declaration, but I am getting stuck here:"
            , D.stack
                [ D.reflow "When a line has no spaces at the beginning, I expect it to be a declaration like one of these:"
                , D.indent 4 <|
                    D.vcat
                        [ D.fromChars "greet : String -> String"
                        , D.fromChars "greet name ="
                        , D.fromChars "  "
                            |> D.a (D.dullyellow (D.fromChars "\"Hello \""))
                            |> D.a (D.fromChars " ++ name ++ ")
                            |> D.a (D.dullyellow (D.fromChars "\"!\""))
                        , D.fromChars ""
                        , D.cyan (D.fromChars "type") |> D.a (D.fromChars " User = Anonymous | LoggedIn String")
                        ]
                , D.reflow "Try to make your declaration look like one of those? Or if this is not supposed to be a declaration, try adding some spaces before it?"
                ]
            )



-- PORT


toPortReport : Code.Source -> T.CRES_Port -> T.CPP_Row -> T.CPP_Col -> Report.Report
toPortReport source port_ startRow startCol =
    case port_ of
        T.CRES_PortSpace space row col ->
            toSpaceReport source space row col

        T.CRES_PortName row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I cannot handle ports with names like this:"
                            , D.reflow ("You are trying to make a port named `" ++ keyword ++ "` but that is a reserved word. Try using some other name?")
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "PORT PROBLEM" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I just saw the start of a `port` declaration, but then I got stuck here:"
                            , D.stack
                                [ D.fillSep
                                    [ D.fromChars "I"
                                    , D.fromChars "was"
                                    , D.fromChars "expecting"
                                    , D.fromChars "to"
                                    , D.fromChars "see"
                                    , D.fromChars "a"
                                    , D.fromChars "name"
                                    , D.fromChars "like"
                                    , D.dullyellow <| D.fromChars "send"
                                    , D.fromChars "or"
                                    , D.dullyellow <| D.fromChars "receive"
                                    , D.fromChars "next."
                                    , D.fromChars "Something"
                                    , D.fromChars "that"
                                    , D.fromChars "starts"
                                    , D.fromChars "with"
                                    , D.fromChars "a"
                                    , D.fromChars "lower-case"
                                    , D.fromChars "letter."
                                    ]
                                , portNote
                                ]
                            )

        T.CRES_PortColon row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "PORT PROBLEM" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I just saw the start of a `port` declaration, but then I got stuck here:"
                    , D.stack
                        [ D.reflow "I was expecting to see a colon next. And then a type that tells me what type of values are going to flow through."
                        , portNote
                        ]
                    )

        T.CRES_PortType tipe row col ->
            toTypeReport source TC_Port tipe row col

        T.CRES_PortIndentName row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PORT" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I just saw the start of a `port` declaration, but then I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "a"
                            , D.fromChars "name"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "send"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "receive"
                            , D.fromChars "next."
                            , D.fromChars "Something"
                            , D.fromChars "that"
                            , D.fromChars "starts"
                            , D.fromChars "with"
                            , D.fromChars "a"
                            , D.fromChars "lower-case"
                            , D.fromChars "letter."
                            ]
                        , portNote
                        ]
                    )

        T.CRES_PortIndentColon row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PORT" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I just saw the start of a `port` declaration, but then I got stuck here:"
                    , D.stack
                        [ D.reflow "I was expecting to see a colon next. And then a type that tells me what type of values are going to flow through."
                        , portNote
                        ]
                    )

        T.CRES_PortIndentType row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PORT" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I just saw the start of a `port` declaration, but then I got stuck here:"
                    , D.stack
                        [ D.reflow "I was expecting to see a type next. Here are examples of outgoing and incoming ports for reference:"
                        , D.indent 4 <|
                            D.vcat
                                [ D.fillSep
                                    [ D.cyan (D.fromChars "port")
                                    , D.fromChars "send"
                                    , D.fromChars ":"
                                    , D.fromChars "String -> Cmd msg"
                                    ]
                                , D.fillSep
                                    [ D.cyan (D.fromChars "port")
                                    , D.fromChars "receive"
                                    , D.fromChars ":"
                                    , D.fromChars "(String -> msg) -> Sub msg"
                                    ]
                                ]
                        , D.reflow "The first line defines a `send` port so you can send strings out to JavaScript. Maybe you send them on a WebSocket or put them into IndexedDB. The second line defines a `receive` port so you can receive strings from JavaScript. Maybe you get receive messages when new WebSocket messages come in or when an entry in IndexedDB changes for some external reason."
                        ]
                    )


portNote : D.Doc
portNote =
    D.stack
        [ D.toSimpleNote "Here are some example `port` declarations for reference:"
        , D.indent 4 <|
            D.vcat
                [ D.fillSep
                    [ D.cyan <| D.fromChars "port"
                    , D.fromChars "send"
                    , D.fromChars ":"
                    , D.fromChars "String -> Cmd msg"
                    ]
                , D.fillSep
                    [ D.cyan <| D.fromChars "port"
                    , D.fromChars "receive"
                    , D.fromChars ":"
                    , D.fromChars "(String -> msg) -> Sub msg"
                    ]
                ]
        , D.reflow "The first line defines a `send` port so you can send strings out to JavaScript. Maybe you send them on a WebSocket or put them into IndexedDB. The second line defines a `receive` port so you can receive strings from JavaScript. Maybe you get receive messages when new WebSocket messages come in or when the IndexedDB is changed for some external reason."
        ]



-- DECL TYPE


toDeclTypeReport : Code.Source -> T.CRES_DeclType -> T.CPP_Row -> T.CPP_Col -> Report.Report
toDeclTypeReport source declType startRow startCol =
    case declType of
        T.CRES_DT_Space space row col ->
            toSpaceReport source space row col

        T.CRES_DT_Name row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING TYPE NAME" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I think I am parsing a type declaration, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "a"
                            , D.fromChars "name"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "Status"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "Style"
                            , D.fromChars "next."
                            , D.fromChars "Just"
                            , D.fromChars "make"
                            , D.fromChars "sure"
                            , D.fromChars "it"
                            , D.fromChars "is"
                            , D.fromChars "a"
                            , D.fromChars "name"
                            , D.fromChars "that"
                            , D.fromChars "starts"
                            , D.fromChars "with"
                            , D.fromChars "a"
                            , D.fromChars "capital"
                            , D.fromChars "letter!"
                            ]
                        , customTypeNote
                        ]
                    )

        T.CRES_DT_Alias typeAlias row col ->
            toTypeAliasReport source typeAlias row col

        T.CRES_DT_Union customType row col ->
            toCustomTypeReport source customType row col

        T.CRES_DT_IndentName row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING TYPE NAME" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I think I am parsing a type declaration, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "a"
                            , D.fromChars "name"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "Status"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "Style"
                            , D.fromChars "next."
                            , D.fromChars "Just"
                            , D.fromChars "make"
                            , D.fromChars "sure"
                            , D.fromChars "it"
                            , D.fromChars "is"
                            , D.fromChars "a"
                            , D.fromChars "name"
                            , D.fromChars "that"
                            , D.fromChars "starts"
                            , D.fromChars "with"
                            , D.fromChars "a"
                            , D.fromChars "capital"
                            , D.fromChars "letter!"
                            ]
                        , customTypeNote
                        ]
                    )


toTypeAliasReport : Code.Source -> T.CRES_TypeAlias -> T.CPP_Row -> T.CPP_Col -> Report.Report
toTypeAliasReport source typeAlias startRow startCol =
    case typeAlias of
        T.CRES_AliasSpace space row col ->
            toSpaceReport source space row col

        T.CRES_AliasName row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING TYPE ALIAS NAME" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a type alias, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "a"
                            , D.fromChars "name"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "Person"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "Point"
                            , D.fromChars "next."
                            , D.fromChars "Just"
                            , D.fromChars "make"
                            , D.fromChars "sure"
                            , D.fromChars "it"
                            , D.fromChars "is"
                            , D.fromChars "a"
                            , D.fromChars "name"
                            , D.fromChars "that"
                            , D.fromChars "starts"
                            , D.fromChars "with"
                            , D.fromChars "a"
                            , D.fromChars "capital"
                            , D.fromChars "letter!"
                            ]
                        , typeAliasNote
                        ]
                    )

        T.CRES_AliasEquals row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I ran into a reserved word unexpectedly while parsing this type alias:"
                            , D.stack
                                [ D.reflow <|
                                    ("It looks like you are trying use `"
                                        ++ keyword
                                        ++ "` as a type variable, but it is a reserved word. Try using a different name?"
                                    )
                                , typeAliasNote
                                ]
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN TYPE ALIAS" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I am partway through parsing a type alias, but I got stuck here:"
                            , D.stack
                                [ D.reflow "I was expecting to see a type variable or an equals sign next."
                                , typeAliasNote
                                ]
                            )

        T.CRES_AliasBody tipe row col ->
            toTypeReport source TC_TypeAlias tipe row col

        T.CRES_AliasIndentEquals row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED TYPE ALIAS" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a type alias, but I got stuck here:"
                    , D.stack
                        [ D.reflow "I was expecting to see a type variable or an equals sign next."
                        , typeAliasNote
                        ]
                    )

        T.CRES_AliasIndentBody row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED TYPE ALIAS" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a type alias, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "a"
                            , D.fromChars "type"
                            , D.fromChars "next."
                            , D.fromChars "Something"
                            , D.fromChars "as"
                            , D.fromChars "simple"
                            , D.fromChars "as"
                            , D.dullyellow <| D.fromChars "Int"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "Float"
                            , D.fromChars "would"
                            , D.fromChars "work!"
                            ]
                        , typeAliasNote
                        ]
                    )


typeAliasNote : D.Doc
typeAliasNote =
    D.stack
        [ D.toSimpleNote "Here is an example of a valid `type alias` for reference:"
        , D.vcat
            [ D.indent 4 <|
                D.fillSep
                    [ D.cyan (D.fromChars "type")
                    , D.cyan (D.fromChars "alias")
                    , D.fromChars "Person"
                    , D.fromChars "="
                    ]
            , D.indent 6 <|
                D.vcat
                    [ D.fromChars "{ name : String"
                    , D.fromChars ", age : Int"
                    , D.fromChars ", height : Float"
                    , D.fromChars "}"
                    ]
            ]
        , D.reflow <|
            "This would let us use `Person` as a shorthand for that record type. Using this shorthand makes type annotations much easier to read, and makes changing code easier if you decide later that there is more to a person than age and height!"
        ]


toCustomTypeReport : Code.Source -> T.CRES_CustomType -> T.CPP_Row -> T.CPP_Col -> Report.Report
toCustomTypeReport source customType startRow startCol =
    case customType of
        T.CRES_CT_Space space row col ->
            toSpaceReport source space row col

        T.CRES_CT_Name row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING TYPE NAME" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I think I am parsing a type declaration, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "a"
                            , D.fromChars "name"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "Status"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "Style"
                            , D.fromChars "next."
                            , D.fromChars "Just"
                            , D.fromChars "make"
                            , D.fromChars "sure"
                            , D.fromChars "it"
                            , D.fromChars "is"
                            , D.fromChars "a"
                            , D.fromChars "name"
                            , D.fromChars "that"
                            , D.fromChars "starts"
                            , D.fromChars "with"
                            , D.fromChars "a"
                            , D.fromChars "capital"
                            , D.fromChars "letter!"
                            ]
                        , customTypeNote
                        ]
                    )

        T.CRES_CT_Equals row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I ran into a reserved word unexpectedly while parsing this custom type:"
                            , D.stack
                                [ D.reflow <|
                                    "It looks like you are trying use `"
                                        ++ keyword
                                        ++ "` as a type variable, but it is a reserved word. Try using a different name?"
                                , customTypeNote
                                ]
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN CUSTOM TYPE" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I am partway through parsing a custom type, but I got stuck here:"
                            , D.stack
                                [ D.reflow "I was expecting to see a type variable or an equals sign next."
                                , customTypeNote
                                ]
                            )

        T.CRES_CT_Bar row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN CUSTOM TYPE" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a custom type, but I got stuck here:"
                    , D.stack
                        [ D.reflow "I was expecting to see a vertical bar like | next."
                        , customTypeNote
                        ]
                    )

        T.CRES_CT_Variant row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN CUSTOM TYPE" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a custom type, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "a"
                            , D.fromChars "variant"
                            , D.fromChars "name"
                            , D.fromChars "next."
                            , D.fromChars "Something"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "Success"
                            , D.fromChars "or"
                            , D.dullyellow (D.fromChars "Sandwich")
                                |> D.a (D.fromChars ".")
                            , D.fromChars "Any"
                            , D.fromChars "name"
                            , D.fromChars "that"
                            , D.fromChars "starts"
                            , D.fromChars "with"
                            , D.fromChars "a"
                            , D.fromChars "capital"
                            , D.fromChars "letter"
                            , D.fromChars "really!"
                            ]
                        , customTypeNote
                        ]
                    )

        T.CRES_CT_VariantArg tipe row col ->
            toTypeReport source TC_CustomType tipe row col

        T.CRES_CT_IndentEquals row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED CUSTOM TYPE" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a custom type, but I got stuck here:"
                    , D.stack
                        [ D.reflow "I was expecting to see a type variable or an equals sign next."
                        , customTypeNote
                        ]
                    )

        T.CRES_CT_IndentBar row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED CUSTOM TYPE" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a custom type, but I got stuck here:"
                    , D.stack
                        [ D.reflow "I was expecting to see a vertical bar like | next."
                        , customTypeNote
                        ]
                    )

        T.CRES_CT_IndentAfterBar row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED CUSTOM TYPE" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a custom type, but I got stuck here:"
                    , D.stack
                        [ D.reflow "I just saw a vertical bar, so I was expecting to see another variant defined next."
                        , customTypeNote
                        ]
                    )

        T.CRES_CT_IndentAfterEquals row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED CUSTOM TYPE" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a custom type, but I got stuck here:"
                    , D.stack
                        [ D.reflow "I just saw an equals sign, so I was expecting to see the first variant defined next."
                        , customTypeNote
                        ]
                    )


customTypeNote : D.Doc
customTypeNote =
    D.stack
        [ D.toSimpleNote "Here is an example of a valid `type` declaration for reference:"
        , D.vcat
            [ D.indent 4 <| D.fillSep [ D.cyan (D.fromChars "type"), D.cyan (D.fromChars "Status") ]
            , D.indent 6 <| D.fillSep [ D.fromChars "=", D.fromChars "Failure" ]
            , D.indent 6 <| D.fillSep [ D.fromChars "|", D.fromChars "Waiting" ]
            , D.indent 6 <| D.fillSep [ D.fromChars "|", D.fromChars "Success", D.fromChars "String" ]
            ]
        , D.reflow <|
            "This defines a new `Status` type with three variants. This could be useful if we are waiting for an HTTP request. Maybe we start with `Waiting` and then switch to `Failure` or `Success \"message from server\"` depending on how things go. Notice that the Success variant has some associated data, allowing us to store a String if the request goes well!"
        ]



-- DECL DEF


toDeclDefReport : Code.Source -> T.CDN_Name -> T.CRES_DeclDef -> T.CPP_Row -> T.CPP_Col -> Report.Report
toDeclDefReport source name declDef startRow startCol =
    case declDef of
        T.CRES_DeclDefSpace space row col ->
            toSpaceReport source space row col

        T.CRES_DeclDefEquals row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.fillSep
                                [ D.fromChars "The"
                                , D.fromChars "name"
                                , D.fromChars "`"
                                    |> D.a (D.cyan (D.fromChars keyword))
                                    |> D.a (D.fromChars "`")
                                , D.fromChars "is"
                                , D.fromChars "reserved"
                                , D.fromChars "in"
                                , D.fromChars "Elm,"
                                , D.fromChars "so"
                                , D.fromChars "it"
                                , D.fromChars "cannot"
                                , D.fromChars "be"
                                , D.fromChars "used"
                                , D.fromChars "as"
                                , D.fromChars "an"
                                , D.fromChars "argument"
                                , D.fromChars "here:"
                                ]
                            , D.stack
                                [ D.reflow "Try renaming it to something else."
                                , case keyword of
                                    "as" ->
                                        D.toFancyNote
                                            [ D.fromChars "This"
                                            , D.fromChars "keyword"
                                            , D.fromChars "is"
                                            , D.fromChars "reserved"
                                            , D.fromChars "for"
                                            , D.fromChars "pattern"
                                            , D.fromChars "matches"
                                            , D.fromChars "like"
                                            , D.fromChars "((x,y)"
                                            , D.cyan <| D.fromChars "as"
                                            , D.fromChars "point)"
                                            , D.fromChars "where"
                                            , D.fromChars "you"
                                            , D.fromChars "want"
                                            , D.fromChars "to"
                                            , D.fromChars "name"
                                            , D.fromChars "a"
                                            , D.fromChars "tuple"
                                            , D.fromChars "and"
                                            , D.fromChars "the"
                                            , D.fromChars "values"
                                            , D.fromChars "it"
                                            , D.fromChars "contains."
                                            ]

                                    _ ->
                                        D.toSimpleNote <|
                                            "The `"
                                                ++ keyword
                                                ++ "` keyword has a special meaning in Elm, so it can only be used in certain situations."
                                ]
                            )

                Code.Operator "->" ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toWiderRegion row col 2
                    in
                    Report.Report "MISSING COLON?" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I was not expecting to see an arrow here:"
                            , D.stack
                                [ D.fillSep
                                    [ D.fromChars "This"
                                    , D.fromChars "usually"
                                    , D.fromChars "means"
                                    , D.fromChars "a"
                                    , D.green <| D.fromChars ":"
                                    , D.fromChars "is"
                                    , D.fromChars "missing"
                                    , D.fromChars "a"
                                    , D.fromChars "bit"
                                    , D.fromChars "earlier"
                                    , D.fromChars "in"
                                    , D.fromChars "a"
                                    , D.fromChars "type"
                                    , D.fromChars "annotation."
                                    , D.fromChars "It"
                                    , D.fromChars "could"
                                    , D.fromChars "be"
                                    , D.fromChars "something"
                                    , D.fromChars "else"
                                    , D.fromChars "though,"
                                    , D.fromChars "so"
                                    , D.fromChars "here"
                                    , D.fromChars "is"
                                    , D.fromChars "a"
                                    , D.fromChars "valid"
                                    , D.fromChars "definition"
                                    , D.fromChars "for"
                                    , D.fromChars "reference:"
                                    ]
                                , D.indent 4 <|
                                    D.vcat
                                        [ D.fromChars "greet : String -> String"
                                        , D.fromChars "greet name ="
                                        , D.fromChars "  "
                                            |> D.a (D.dullyellow (D.fromChars "\"Hello \""))
                                            |> D.a (D.fromChars " ++ name ++ ")
                                            |> D.a (D.dullyellow (D.fromChars "\"!\""))
                                        ]
                                , D.reflow <|
                                    "Try to use that format with your `"
                                        ++ name
                                        ++ "` definition!"
                                ]
                            )

                Code.Operator op ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col op
                    in
                    Report.Report "UNEXPECTED SYMBOL" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I was not expecting to see this symbol here:"
                            , D.stack
                                [ D.reflow "I am not sure what is going wrong exactly, so here is a valid definition (with an optional type annotation) for reference:"
                                , D.indent 4 <|
                                    D.vcat
                                        [ D.fromChars "greet : String -> String"
                                        , D.fromChars "greet name ="
                                        , D.fromChars "  "
                                            |> D.a (D.dullyellow (D.fromChars "\"Hello \""))
                                            |> D.a (D.fromChars " ++ name ++ ")
                                            |> D.a (D.dullyellow (D.fromChars "\"!\""))
                                        ]
                                , D.reflow <|
                                    "Try to use that format with your `"
                                        ++ name
                                        ++ "` definition!"
                                ]
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN DEFINITION" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I got stuck while parsing the `"
                                    ++ name
                                    ++ "` definition:"
                            , D.stack
                                [ D.reflow "I am not sure what is going wrong exactly, so here is a valid definition (with an optional type annotation) for reference:"
                                , D.indent 4 <|
                                    D.vcat
                                        [ D.fromChars "greet : String -> String"
                                        , D.fromChars "greet name ="
                                        , D.fromChars "  "
                                            |> D.a (D.dullyellow (D.fromChars "\"Hello \""))
                                            |> D.a (D.fromChars " ++ name ++ ")
                                            |> D.a (D.dullyellow (D.fromChars "\"!\""))
                                        ]
                                , D.reflow "Try to use that format!"
                                ]
                            )

        T.CRES_DeclDefType tipe row col ->
            toTypeReport source (TC_Annotation name) tipe row col

        T.CRES_DeclDefArg pattern row col ->
            toPatternReport source PArg pattern row col

        T.CRES_DeclDefBody expr row col ->
            toExprReport source (InDef name startRow startCol) expr row col

        T.CRES_DeclDefNameRepeat row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING DEFINITION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw the type annotation for `"
                            ++ name
                            ++ "` so I was expecting to see its definition here:"
                    , D.stack
                        [ D.reflow "Type annotations always appear directly above the relevant definition, without anything else in between. (Not even doc comments!)"
                        , declDefNote
                        ]
                    )

        T.CRES_DeclDefNameMatch defName row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "NAME MISMATCH" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw a type annotation for `"
                            ++ name
                            ++ "`, but it is followed by a definition for `"
                            ++ defName
                            ++ "`:"
                    , D.stack
                        [ D.reflow "These names do not match! Is there a typo?"
                        , D.indent 4 <|
                            D.fillSep
                                [ D.dullyellow (D.fromName defName)
                                , D.fromChars "->"
                                , D.green (D.fromName name)
                                ]
                        ]
                    )

        T.CRES_DeclDefIndentType row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck while parsing the `"
                            ++ name
                            ++ "` type annotation:"
                    , D.stack
                        [ D.reflow "I just saw a colon, so I am expecting to see a type next."
                        , declDefNote
                        ]
                    )

        T.CRES_DeclDefIndentEquals row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck while parsing the `"
                            ++ name
                            ++ "` definition:"
                    , D.stack
                        [ D.reflow "I was expecting to see an argument or an equals sign next."
                        , declDefNote
                        ]
                    )

        T.CRES_DeclDefIndentBody row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck while parsing the `"
                            ++ name
                            ++ "` definition:"
                    , D.stack
                        [ D.reflow "I was expecting to see an expression next. What is it equal to?"
                        , declDefNote
                        ]
                    )


declDefNote : D.Doc
declDefNote =
    D.stack
        [ D.reflow "Here is a valid definition (with a type annotation) for reference:"
        , D.indent 4 <|
            D.vcat
                [ D.fromChars "greet : String -> String"
                , D.fromChars "greet name ="
                , D.fromChars "  "
                    |> D.a (D.dullyellow (D.fromChars "\"Hello \""))
                    |> D.a (D.fromChars " ++ name ++ ")
                    |> D.a (D.dullyellow (D.fromChars "\"!\""))
                ]
        , D.reflow "The top line (called a \"type annotation\") is optional. You can leave it off if you want. As you get more comfortable with Elm and as your project grows, it becomes more and more valuable to add them though! They work great as compiler-verified documentation, and they often improve error messages!"
        ]



-- CONTEXT


type Context
    = InNode Node T.CPP_Row T.CPP_Col Context
    | InDef T.CDN_Name T.CPP_Row T.CPP_Col
    | InDestruct T.CPP_Row T.CPP_Col


type Node
    = NRecord
    | NParens
    | NList
    | NFunc
    | NCond
    | NThen
    | NElse
    | NCase
    | NBranch


getDefName : Context -> Maybe T.CDN_Name
getDefName context =
    case context of
        InDestruct _ _ ->
            Nothing

        InDef name _ _ ->
            Just name

        InNode _ _ _ c ->
            getDefName c


isWithin : Node -> Context -> Bool
isWithin desiredNode context =
    case context of
        InDestruct _ _ ->
            False

        InDef _ _ _ ->
            False

        InNode actualNode _ _ _ ->
            desiredNode == actualNode



-- EXPR REPORTS


toExprReport : Code.Source -> Context -> T.CRES_Expr -> T.CPP_Row -> T.CPP_Col -> Report.Report
toExprReport source context expr startRow startCol =
    case expr of
        T.CRES_Let let_ row col ->
            toLetReport source context let_ row col

        T.CRES_Case case_ row col ->
            toCaseReport source context case_ row col

        T.CRES_If if_ row col ->
            toIfReport source context if_ row col

        T.CRES_List list row col ->
            toListReport source context list row col

        T.CRES_Record record row col ->
            toRecordReport source context record row col

        T.CRES_Tuple tuple row col ->
            toTupleReport source context tuple row col

        T.CRES_Func func row col ->
            toFuncReport source context func row col

        T.CRES_Dot row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING RECORD ACCESSOR" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I was expecting to see a record accessor here:"
                    , D.fillSep
                        [ D.fromChars "Something"
                        , D.fromChars "like"
                        , D.dullyellow <| D.fromChars ".name"
                        , D.fromChars "or"
                        , D.dullyellow <| D.fromChars ".price"
                        , D.fromChars "that"
                        , D.fromChars "accesses"
                        , D.fromChars "a"
                        , D.fromChars "value"
                        , D.fromChars "from"
                        , D.fromChars "a"
                        , D.fromChars "record."
                        ]
                    )

        T.CRES_Access row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING RECORD ACCESSOR" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I am trying to parse a record accessor here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Something"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars ".name"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars ".price"
                            , D.fromChars "that"
                            , D.fromChars "accesses"
                            , D.fromChars "a"
                            , D.fromChars "value"
                            , D.fromChars "from"
                            , D.fromChars "a"
                            , D.fromChars "record."
                            ]
                        , D.toSimpleNote "Record field names must start with a lower case letter!"
                        ]
                    )

        T.CRES_OperatorRight op row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col

                isMath : Bool
                isMath =
                    List.member op [ "-", "+", "*", "/", "^" ]
            in
            Report.Report "MISSING EXPRESSION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw a "
                            ++ op
                            ++ " "
                            ++ (if isMath then
                                    "sign"

                                else
                                    "operator"
                               )
                            ++ ", so I am getting stuck here:"
                    , if isMath then
                        D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "an"
                            , D.fromChars "expression"
                            , D.fromChars "next."
                            , D.fromChars "Something"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "42"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "1000"
                            , D.fromChars "that"
                            , D.fromChars "makes"
                            , D.fromChars "sense"
                            , D.fromChars "with"
                            , D.fromChars "a"
                            , D.fromName op
                            , D.fromChars "sign."
                            ]

                      else if op == "&&" || op == "||" then
                        D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "an"
                            , D.fromChars "expression"
                            , D.fromChars "next."
                            , D.fromChars "Something"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "True"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "False"
                            , D.fromChars "that"
                            , D.fromChars "makes"
                            , D.fromChars "sense"
                            , D.fromChars "with"
                            , D.fromChars "boolean"
                            , D.fromChars "logic."
                            ]

                      else if op == "|>" then
                        D.reflow "I was expecting to see a function next."

                      else if op == "<|" then
                        D.reflow "I was expecting to see an argument next."

                      else
                        D.reflow "I was expecting to see an expression next."
                    )

        T.CRES_OperatorReserved operator row col ->
            toOperatorReport source context operator row col

        T.CRES_Start row col ->
            let
                ( contextRow, contextCol, aThing ) =
                    case context of
                        InDestruct r c ->
                            ( r, c, "a definition" )

                        InDef name r c ->
                            ( r, c, "the `" ++ name ++ "` definition" )

                        InNode NRecord r c _ ->
                            ( r, c, "a record" )

                        InNode NParens r c _ ->
                            ( r, c, "some parentheses" )

                        InNode NList r c _ ->
                            ( r, c, "a list" )

                        InNode NFunc r c _ ->
                            ( r, c, "an anonymous function" )

                        InNode NCond r c _ ->
                            ( r, c, "an `if` expression" )

                        InNode NThen r c _ ->
                            ( r, c, "an `if` expression" )

                        InNode NElse r c _ ->
                            ( r, c, "an `if` expression" )

                        InNode NCase r c _ ->
                            ( r, c, "a `case` expression" )

                        InNode NBranch r c _ ->
                            ( r, c, "a `case` expression" )

                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position contextRow contextCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "MISSING EXPRESSION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <| "I am partway through parsing " ++ aThing ++ ", but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "an"
                            , D.fromChars "expression"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "42"
                            , D.fromChars "or"
                            , D.dullyellow (D.fromChars "\"hello\"")
                                |> D.a (D.fromChars ".")
                            , D.fromChars "Once"
                            , D.fromChars "there"
                            , D.fromChars "is"
                            , D.fromChars "something"
                            , D.fromChars "there,"
                            , D.fromChars "I"
                            , D.fromChars "can"
                            , D.fromChars "probably"
                            , D.fromChars "give"
                            , D.fromChars "a"
                            , D.fromChars "more"
                            , D.fromChars "specific"
                            , D.fromChars "hint!"
                            ]
                        , D.toSimpleNote "This can also happen if I run into reserved words like `let` or `as` unexpectedly. Or if I run into operators in unexpected spots. Point is, there are a couple ways I can get confused and give sort of weird advice!"
                        ]
                    )

        T.CRES_Char char row col ->
            toCharReport source char row col

        T.CRES_String_ string row col ->
            toStringReport source string row col

        T.CRES_Number number row col ->
            toNumberReport source number row col

        T.CRES_Space space row col ->
            toSpaceReport source space row col

        T.CRES_EndlessShader row col ->
            let
                region : T.CRA_Region
                region =
                    toWiderRegion row col 6
            in
            Report.Report "ENDLESS SHADER" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I cannot find the end of this shader:"
                    , D.reflow "Add a |] somewhere after this to end the shader."
                    )

        T.CRES_ShaderProblem problem row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "SHADER PROBLEM" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I ran into a problem while parsing this GLSL block."
                    , D.stack
                        [ D.reflow "I use a 3rd party GLSL parser for now, and I did my best to extract their error message:"
                        , D.indent 4 <| D.vcat <| List.map D.fromChars (List.filter ((/=) "") (String.lines problem))
                        ]
                    )

        T.CRES_IndentOperatorRight op row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "MISSING EXPRESSION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <| "I was expecting to see an expression after this " ++ op ++ " operator:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "You"
                            , D.fromChars "can"
                            , D.fromChars "just"
                            , D.fromChars "put"
                            , D.fromChars "anything"
                            , D.fromChars "for"
                            , D.fromChars "now,"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "42"
                            , D.fromChars "or"
                            , D.dullyellow (D.fromChars "\"hello\"")
                                |> D.a (D.fromChars ".")
                            , D.fromChars "Once"
                            , D.fromChars "there"
                            , D.fromChars "is"
                            , D.fromChars "something"
                            , D.fromChars "there,"
                            , D.fromChars "I"
                            , D.fromChars "can"
                            , D.fromChars "probably"
                            , D.fromChars "give"
                            , D.fromChars "a"
                            , D.fromChars "more"
                            , D.fromChars "specific"
                            , D.fromChars "hint!"
                            ]
                        , D.toSimpleNote <| "I may be getting confused by your indentation? The easiest way to make sure this is not an indentation problem is to put the expression on the right of the " ++ op ++ " operator on the same line."
                        ]
                    )



-- CHAR


toCharReport : Code.Source -> T.CRES_Char -> T.CPP_Row -> T.CPP_Col -> Report.Report
toCharReport source char row col =
    case char of
        T.CRES_CharEndless ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "MISSING SINGLE QUOTE" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow
                        "I thought I was parsing a character, but I got to the end of the line without seeing the closing single quote:"
                    , D.reflow "Add a closing single quote here!"
                    )

        T.CRES_CharEscape escape ->
            toEscapeReport source escape row col

        T.CRES_CharNotString width ->
            let
                region : T.CRA_Region
                region =
                    toWiderRegion row col width
            in
            Report.Report "NEEDS DOUBLE QUOTES" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.fromChars "The following string uses single quotes:"
                    , D.stack
                        [ D.fromChars "Please switch to double quotes instead:"
                        , D.indent 4 <|
                            (D.dullyellow (D.fromChars "'this'")
                                |> D.a (D.fromChars " => ")
                                |> D.a (D.green (D.fromChars "\"this\""))
                            )
                        , D.toSimpleNote
                            "Elm uses double quotes for strings like \"hello\", whereas it uses single quotes for individual characters like 'a' and 'ø'. This distinction helps with code like (String.any (\\c -> c == 'X') \"90210\") where you are inspecting individual characters."
                        ]
                    )



-- STRING


toStringReport : Code.Source -> T.CRES_String_ -> T.CPP_Row -> T.CPP_Col -> Report.Report
toStringReport source string row col =
    case string of
        T.CRES_StringEndless_Single ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "ENDLESS STRING" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow
                        "I got to the end of the line without seeing the closing double quote:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Strings"
                            , D.fromChars "look"
                            , D.fromChars "like"
                            , D.green <| D.fromChars "\"this\""
                            , D.fromChars "with"
                            , D.fromChars "double"
                            , D.fromChars "quotes"
                            , D.fromChars "on"
                            , D.fromChars "each"
                            , D.fromChars "end."
                            , D.fromChars "Is"
                            , D.fromChars "the"
                            , D.fromChars "closing"
                            , D.fromChars "double"
                            , D.fromChars "quote"
                            , D.fromChars "missing"
                            , D.fromChars "in"
                            , D.fromChars "your"
                            , D.fromChars "code?"
                            ]
                        , D.toSimpleNote
                            "For a string that spans multiple lines, you can use the multi-line string syntax like this:"
                        , D.dullyellow <|
                            D.indent 4 <|
                                D.vcat
                                    [ D.fromChars "\"\"\""
                                    , D.fromChars "# Multi-line Strings"
                                    , D.fromChars ""
                                    , D.fromChars "- start with triple double quotes"
                                    , D.fromChars "- write whatever you want"
                                    , D.fromChars "- no need to escape newlines or double quotes"
                                    , D.fromChars "- end with triple double quotes"
                                    , D.fromChars "\"\"\""
                                    ]
                        ]
                    )

        T.CRES_StringEndless_Multi ->
            let
                region : T.CRA_Region
                region =
                    toWiderRegion row col 3
            in
            Report.Report "ENDLESS STRING" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow
                        "I cannot find the end of this multi-line string:"
                    , D.stack
                        [ D.reflow "Add a \"\"\" somewhere after this to end the string."
                        , D.toSimpleNote "Here is a valid multi-line string for reference:"
                        , D.dullyellow <|
                            D.indent 4 <|
                                D.vcat
                                    [ D.fromChars "\"\"\""
                                    , D.fromChars "# Multi-line Strings"
                                    , D.fromChars ""
                                    , D.fromChars "- start with triple double quotes"
                                    , D.fromChars "- write whatever you want"
                                    , D.fromChars "- no need to escape newlines or double quotes"
                                    , D.fromChars "- end with triple double quotes"
                                    , D.fromChars "\"\"\""
                                    ]
                        ]
                    )

        T.CRES_StringEscape escape ->
            toEscapeReport source escape row col



-- ESCAPES


toEscapeReport : Code.Source -> T.CRES_Escape -> T.CPP_Row -> T.CPP_Col -> Report.Report
toEscapeReport source escape row col =
    case escape of
        T.CRES_EscapeUnknown ->
            let
                region : T.CRA_Region
                region =
                    toWiderRegion row col 2
            in
            Report.Report "UNKNOWN ESCAPE" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "Backslashes always start escaped characters, but I do not recognize this one:"
                    , D.stack
                        [ D.reflow "Valid escape characters include:"
                        , D.dullyellow <|
                            D.indent 4 <|
                                D.vcat
                                    [ D.fromChars "\\n"
                                    , D.fromChars "\\r"
                                    , D.fromChars "\\t"
                                    , D.fromChars "\\\""
                                    , D.fromChars "\\'"
                                    , D.fromChars "\\\\"
                                    , D.fromChars "\\u{003D}"
                                    ]
                        , D.reflow "Do you want one of those instead? Maybe you need \\\\ to escape a backslash?"
                        , D.toSimpleNote "The last style lets encode ANY character by its Unicode code point. That means \\u{0009} and \\t are the same. You can use that style for anything not covered by the other six escapes!"
                        ]
                    )

        T.CRES_BadUnicodeFormat width ->
            let
                region : T.CRA_Region
                region =
                    toWiderRegion row col width
            in
            Report.Report "BAD UNICODE ESCAPE" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I ran into an invalid Unicode escape:"
                    , D.stack
                        [ D.reflow "Here are some examples of valid Unicode escapes:"
                        , D.dullyellow <|
                            D.indent 4 <|
                                D.vcat
                                    [ D.fromChars "\\u{0041}"
                                    , D.fromChars "\\u{03BB}"
                                    , D.fromChars "\\u{6728}"
                                    , D.fromChars "\\u{1F60A}"
                                    ]
                        , D.reflow "Notice that the code point is always surrounded by curly braces. Maybe you are missing the opening or closing curly brace?"
                        ]
                    )

        T.CRES_BadUnicodeCode width ->
            let
                region : T.CRA_Region
                region =
                    toWiderRegion row col width
            in
            Report.Report "BAD UNICODE ESCAPE" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "This is not a valid code point:"
                    , D.reflow "The valid code points are between 0 and 10FFFF inclusive."
                    )

        T.CRES_BadUnicodeLength width numDigits badCode ->
            let
                region : T.CRA_Region
                region =
                    toWiderRegion row col width
            in
            Report.Report "BAD UNICODE ESCAPE" region [] <|
                Code.toSnippet source region Nothing <|
                    if numDigits < 4 then
                        ( D.reflow "Every code point needs at least four digits:"
                        , let
                            goodCode : String
                            goodCode =
                                String.repeat (4 - numDigits) "0" ++ String.toUpper (Hex.toString badCode)

                            suggestion : D.Doc
                            suggestion =
                                D.fromChars ("\\u{" ++ goodCode ++ "}")
                          in
                          D.fillSep
                            [ D.fromChars "Try"
                            , D.green suggestion
                            , D.fromChars "instead?"
                            ]
                        )

                    else
                        ( D.reflow "This code point has too many digits:"
                        , D.fillSep
                            [ D.fromChars "Valid"
                            , D.fromChars "code"
                            , D.fromChars "points"
                            , D.fromChars "are"
                            , D.fromChars "between"
                            , D.green <| D.fromChars "\\u{0000}"
                            , D.fromChars "and"
                            , D.green <| D.fromChars "\\u{10FFFF}"
                            , D.fromChars ","
                            , D.fromChars "so"
                            , D.fromChars "try"
                            , D.fromChars "trimming"
                            , D.fromChars "any"
                            , D.fromChars "leading"
                            , D.fromChars "zeros"
                            , D.fromChars "until"
                            , D.fromChars "you"
                            , D.fromChars "have"
                            , D.fromChars "between"
                            , D.fromChars "four"
                            , D.fromChars "and"
                            , D.fromChars "six"
                            , D.fromChars "digits."
                            ]
                        )



-- NUMBERS


toNumberReport : Code.Source -> T.CRES_Number -> T.CPP_Row -> T.CPP_Col -> Report.Report
toNumberReport source number row col =
    let
        region : T.CRA_Region
        region =
            toRegion row col
    in
    case number of
        T.CRES_NumberEnd ->
            Report.Report "WEIRD NUMBER" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I thought I was reading a number, but I ran into some weird stuff here:"
                    , D.stack
                        [ D.reflow "I recognize numbers in the following formats:"
                        , D.indent 4 <|
                            D.vcat
                                [ D.fromChars "42"
                                , D.fromChars "3.14"
                                , D.fromChars "6.022e23"
                                , D.fromChars "0x002B"
                                ]
                        , D.reflow "So is there a way to write it like one of those?"
                        ]
                    )

        T.CRES_NumberDot int ->
            Report.Report "WEIRD NUMBER" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "Numbers cannot end with a dot like this:"
                    , D.fillSep
                        [ D.fromChars "Switching"
                        , D.fromChars "to"
                        , D.green (D.fromChars (String.fromInt int))
                        , D.fromChars "or"
                        , D.green (D.fromChars (String.fromInt int ++ ".0"))
                        , D.fromChars "will"
                        , D.fromChars "work"
                        , D.fromChars "though!"
                        ]
                    )

        T.CRES_NumberHexDigit ->
            Report.Report "WEIRD HEXIDECIMAL" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I thought I was reading a hexidecimal number until I got here:"
                    , D.stack
                        [ D.reflow "Valid hexidecimal digits include 0123456789abcdefABCDEF, so I can only recognize things like this:"
                        , D.indent 4 <|
                            D.vcat
                                [ D.fromChars "0x2B"
                                , D.fromChars "0x002B"
                                , D.fromChars "0x00ffb3"
                                ]
                        ]
                    )

        T.CRES_NumberNoLeadingZero ->
            Report.Report "LEADING ZEROS" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I do not accept numbers with leading zeros:"
                    , D.stack
                        [ D.reflow "Just delete the leading zeros and it should work!"
                        , D.toSimpleNote "Some languages let you to specify octal numbers by adding a leading zero. So in C, writing 0111 is the same as writing 73. Some people are used to that, but others probably want it to equal 111. Either path is going to surprise people from certain backgrounds, so Elm tries to avoid this whole situation."
                        ]
                    )



-- OPERATORS


toOperatorReport : Code.Source -> Context -> T.CPS_BadOperator -> T.CPP_Row -> T.CPP_Col -> Report.Report
toOperatorReport source context operator row col =
    case operator of
        T.CPS_BadDot ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED SYMBOL" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.fromChars "I was not expecting this dot:"
                    , D.reflow "Dots are for record access and decimal points, so they cannot float around on their own. Maybe there is some extra whitespace?"
                    )

        T.CPS_BadPipe ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED SYMBOL" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I was not expecting this vertical bar:"
                    , D.reflow "Vertical bars should only appear in custom type declarations. Maybe you want || instead?"
                    )

        T.CPS_BadArrow ->
            let
                region : T.CRA_Region
                region =
                    toWiderRegion row col 2
            in
            Report.Report "UNEXPECTED ARROW" region [] <|
                Code.toSnippet source region Nothing <|
                    if isWithin NCase context then
                        ( D.reflow "I am parsing a `case` expression right now, but this arrow is confusing me:"
                        , D.stack
                            [ D.reflow "Maybe the `of` keyword is missing on a previous line?"
                            , noteForCaseError
                            ]
                        )

                    else if isWithin NBranch context then
                        ( D.reflow
                            "I am parsing a `case` expression right now, but this arrow is confusing me:"
                        , D.stack
                            [ D.reflow
                                "It makes sense to see arrows around here, so I suspect it is something earlier. Maybe this pattern is indented a bit farther than the previous patterns?"
                            , noteForCaseIndentError
                            ]
                        )

                    else
                        ( D.reflow
                            "I was partway through parsing an expression when I got stuck on this arrow:"
                        , D.stack
                            [ D.fromChars "Arrows should only appear in `case` expressions and anonymous functions.\nMaybe it was supposed to be a > sign instead?"
                            , D.toSimpleNote
                                "The syntax for anonymous functions is (\\x -> x + 1) so the arguments all appear after the backslash and before the arrow. Maybe a backslash is missing earlier?"
                            ]
                        )

        T.CPS_BadEquals ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED EQUALS" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow
                        "I was not expecting to see this equals sign:"
                    , D.stack
                        [ D.reflow "Maybe you want == instead? To check if two values are equal?"
                        , D.toSimpleNote <|
                            if isWithin NRecord context then
                                "Records look like { x = 3, y = 4 } with the equals sign right after the field name. So maybe you forgot a comma?"

                            else
                                case getDefName context of
                                    Nothing ->
                                        "I may be getting confused by your indentation. I need all definitions to be indented exactly the same amount, so if this is meant to be a new definition, it may have too many spaces in front of it."

                                    Just name ->
                                        "I may be getting confused by your indentation. I think I am still parsing the `"
                                            ++ name
                                            ++ "` definition. Is this supposed to be part of a definition after that? If so, the problem may be a bit before the equals sign. I need all definitions to be indented exactly the same amount, so the problem may be that this new definition has too many spaces in front of it."
                        ]
                    )

        T.CPS_BadHasType ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED SYMBOL" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow
                        "I was not expecting to run into the \"has type\" symbol here:"
                    , case getDefName context of
                        Nothing ->
                            D.fillSep
                                [ D.fromChars "Maybe"
                                , D.fromChars "you"
                                , D.fromChars "want"
                                , D.green <| D.fromChars "::"
                                , D.fromChars "instead?"
                                , D.fromChars "To"
                                , D.fromChars "put"
                                , D.fromChars "something"
                                , D.fromChars "on"
                                , D.fromChars "the"
                                , D.fromChars "front"
                                , D.fromChars "of"
                                , D.fromChars "a"
                                , D.fromChars "list?"
                                ]

                        Just name ->
                            D.stack
                                [ D.fillSep
                                    [ D.fromChars "Maybe"
                                    , D.fromChars "you"
                                    , D.fromChars "want"
                                    , D.green <| D.fromChars "::"
                                    , D.fromChars "instead?"
                                    , D.fromChars "To"
                                    , D.fromChars "put"
                                    , D.fromChars "something"
                                    , D.fromChars "on"
                                    , D.fromChars "the"
                                    , D.fromChars "front"
                                    , D.fromChars "of"
                                    , D.fromChars "a"
                                    , D.fromChars "list?"
                                    ]
                                , D.toSimpleNote <|
                                    "The single colon is reserved for type annotations and record types, but I think I am parsing the definition of `"
                                        ++ name
                                        ++ "` right now."
                                , D.toSimpleNote <|
                                    "I may be getting confused by your indentation. Is this supposed to be part of a type annotation AFTER the `"
                                        ++ name
                                        ++ "` definition? If so, the problem may be a bit before the \"has type\" symbol. I need all definitions to be exactly aligned (with exactly the same indentation) so the problem may be that this new definition is indented a bit too much."
                                ]
                    )



-- CASE


toLetReport : Code.Source -> Context -> T.CRES_Let -> T.CPP_Row -> T.CPP_Col -> Report.Report
toLetReport source context let_ startRow startCol =
    case let_ of
        T.CRES_LetSpace space row col ->
            toSpaceReport source space row col

        T.CRES_LetIn row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "LET PROBLEM" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow
                        "I was partway through parsing a `let` expression, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Based"
                            , D.fromChars "on"
                            , D.fromChars "the"
                            , D.fromChars "indentation,"
                            , D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "the"
                            , D.cyan <| D.fromChars "in"
                            , D.fromChars "keyword"
                            , D.fromChars "next."
                            , D.fromChars "Is"
                            , D.fromChars "there"
                            , D.fromChars "a"
                            , D.fromChars "typo?"
                            ]
                        , D.toSimpleNote
                            "This can also happen if you are trying to define another value within the `let` but it is not indented enough. Make sure each definition has exactly the same amount of spaces before it. They should line up exactly!"
                        ]
                    )

        T.CRES_LetDefAlignment _ row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "LET PROBLEM" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow
                        "I was partway through parsing a `let` expression, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Based"
                            , D.fromChars "on"
                            , D.fromChars "the"
                            , D.fromChars "indentation,"
                            , D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "the"
                            , D.cyan <| D.fromChars "in"
                            , D.fromChars "keyword"
                            , D.fromChars "next."
                            , D.fromChars "Is"
                            , D.fromChars "there"
                            , D.fromChars "a"
                            , D.fromChars "typo?"
                            ]
                        , D.toSimpleNote
                            "This can also happen if you are trying to define another value within the `let` but it is not indented enough. Make sure each definition has exactly the same amount of spaces before it. They should line up exactly!"
                        ]
                    )

        T.CRES_LetDefName row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow
                                "I was partway through parsing a `let` expression, but I got stuck here:"
                            , D.reflow <|
                                "It looks like you are trying to use `"
                                    ++ keyword
                                    ++ "` as a variable name, but it is a reserved word! Try using a different name instead."
                            )

                _ ->
                    toUnfinishLetReport source row col startRow startCol <|
                        D.reflow
                            "I was expecting the name of a definition next."

        T.CRES_LetDef name def row col ->
            toLetDefReport source name def row col

        T.CRES_LetDestruct destruct row col ->
            toLetDestructReport source destruct row col

        T.CRES_LetBody expr row col ->
            toExprReport source context expr row col

        T.CRES_LetIndentDef row col ->
            toUnfinishLetReport source row col startRow startCol <|
                D.reflow
                    "I was expecting a value to be defined here."

        T.CRES_LetIndentIn row col ->
            toUnfinishLetReport source row col startRow startCol <|
                D.fillSep
                    [ D.fromChars "I"
                    , D.fromChars "was"
                    , D.fromChars "expecting"
                    , D.fromChars "to"
                    , D.fromChars "see"
                    , D.fromChars "the"
                    , D.cyan <| D.fromChars "in"
                    , D.fromChars "keyword"
                    , D.fromChars "next."
                    , D.fromChars "Or"
                    , D.fromChars "maybe"
                    , D.fromChars "more"
                    , D.fromChars "of"
                    , D.fromChars "that"
                    , D.fromChars "expression?"
                    ]

        T.CRES_LetIndentBody row col ->
            toUnfinishLetReport source row col startRow startCol <|
                D.reflow
                    "I was expecting an expression next. Tell me what should happen with the value you just defined!"


toUnfinishLetReport : Code.Source -> T.CPP_Row -> T.CPP_Col -> T.CPP_Row -> T.CPP_Col -> D.Doc -> Report.Report
toUnfinishLetReport source row col startRow startCol message =
    let
        surroundings : T.CRA_Region
        surroundings =
            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

        region : T.CRA_Region
        region =
            toRegion row col
    in
    Report.Report "UNFINISHED LET" region [] <|
        Code.toSnippet source surroundings (Just region) <|
            ( D.reflow "I was partway through parsing a `let` expression, but I got stuck here:"
            , D.stack
                [ message
                , D.toSimpleNote "Here is an example with a valid `let` expression for reference:"
                , D.indent 4 <|
                    D.vcat
                        [ D.indent 0 <|
                            D.fillSep
                                [ D.fromChars "viewPerson"
                                , D.fromChars "person"
                                , D.fromChars "="
                                ]
                        , D.indent 2 <| D.fillSep [ D.cyan (D.fromChars "let") ]
                        , D.indent 4 <| D.fillSep [ D.fromChars "fullName", D.fromChars "=" ]
                        , D.indent 6 <|
                            D.fillSep
                                [ D.fromChars "person.firstName"
                                , D.fromChars "++"
                                , D.dullyellow (D.fromChars "\" \"")
                                , D.fromChars "++"
                                , D.fromChars "person.lastName"
                                ]
                        , D.indent 2 <| D.fillSep [ D.cyan (D.fromChars "in") ]
                        , D.indent 2 <|
                            D.fillSep
                                [ D.fromChars "div"
                                , D.fromChars "[]"
                                , D.fromChars "["
                                , D.fromChars "text"
                                , D.fromChars "fullName"
                                , D.fromChars "]"
                                ]
                        ]
                , D.reflow "Here we defined a `viewPerson` function that turns a person into some HTML. We use a `let` expression to define the `fullName` we want to show. Notice the indentation! The `fullName` is indented more than the `let` keyword, and the actual value of `fullName` is indented a bit more than that. That is important!"
                ]
            )


toLetDefReport : Code.Source -> T.CDN_Name -> T.CRES_Def -> T.CPP_Row -> T.CPP_Col -> Report.Report
toLetDefReport source name def startRow startCol =
    case def of
        T.CRES_DefSpace space row col ->
            toSpaceReport source space row col

        T.CRES_DefType tipe row col ->
            toTypeReport source (TC_Annotation name) tipe row col

        T.CRES_DefNameRepeat row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING DEFINITION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow ("I just saw the type annotation for `" ++ name ++ "` so I was expecting to see its definition here:")
                    , D.stack
                        [ D.reflow "Type annotations always appear directly above the relevant definition, without anything else in between."
                        , defNote
                        ]
                    )

        T.CRES_DefNameMatch defName row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "NAME MISMATCH" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow ("I just saw a type annotation for `" ++ name ++ "`, but it is followed by a definition for `" ++ defName ++ "`:")
                    , D.stack
                        [ D.reflow "These names do not match! Is there a typo?"
                        , D.indent 4 <|
                            D.fillSep
                                [ D.dullyellow (D.fromName defName)
                                , D.fromChars "->"
                                , D.green (D.fromName name)
                                ]
                        ]
                    )

        T.CRES_DefArg pattern row col ->
            toPatternReport source PArg pattern row col

        T.CRES_DefEquals row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.fillSep
                                [ D.fromChars "The"
                                , D.fromChars "name"
                                , D.fromChars "`"
                                    |> D.a (D.cyan (D.fromChars keyword))
                                    |> D.a (D.fromChars "`")
                                , D.fromChars "is"
                                , D.fromChars "reserved"
                                , D.fromChars "in"
                                , D.fromChars "Elm,"
                                , D.fromChars "so"
                                , D.fromChars "it"
                                , D.fromChars "cannot"
                                , D.fromChars "be"
                                , D.fromChars "used"
                                , D.fromChars "as"
                                , D.fromChars "an"
                                , D.fromChars "argument"
                                , D.fromChars "here:"
                                ]
                            , D.stack
                                [ D.reflow "Try renaming it to something else."
                                , case keyword of
                                    "as" ->
                                        D.toFancyNote
                                            [ D.fromChars "This"
                                            , D.fromChars "keyword"
                                            , D.fromChars "is"
                                            , D.fromChars "reserved"
                                            , D.fromChars "for"
                                            , D.fromChars "pattern"
                                            , D.fromChars "matches"
                                            , D.fromChars "like"
                                            , D.fromChars "((x,y)"
                                            , D.cyan <| D.fromChars "as"
                                            , D.fromChars "point)"
                                            , D.fromChars "where"
                                            , D.fromChars "you"
                                            , D.fromChars "want"
                                            , D.fromChars "to"
                                            , D.fromChars "name"
                                            , D.fromChars "a"
                                            , D.fromChars "tuple"
                                            , D.fromChars "and"
                                            , D.fromChars "the"
                                            , D.fromChars "values"
                                            , D.fromChars "it"
                                            , D.fromChars "contains."
                                            ]

                                    _ ->
                                        D.toSimpleNote <|
                                            "The `"
                                                ++ keyword
                                                ++ "` keyword has a special meaning in Elm, so it can only be used in certain situations."
                                ]
                            )

                Code.Operator "->" ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toWiderRegion row col 2
                    in
                    Report.Report "MISSING COLON?" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I was not expecting to see an arrow here:"
                            , D.stack
                                [ D.fillSep
                                    [ D.fromChars "This"
                                    , D.fromChars "usually"
                                    , D.fromChars "means"
                                    , D.fromChars "a"
                                    , D.green <| D.fromChars ":"
                                    , D.fromChars "is"
                                    , D.fromChars "missing"
                                    , D.fromChars "a"
                                    , D.fromChars "bit"
                                    , D.fromChars "earlier"
                                    , D.fromChars "in"
                                    , D.fromChars "a"
                                    , D.fromChars "type"
                                    , D.fromChars "annotation."
                                    , D.fromChars "It"
                                    , D.fromChars "could"
                                    , D.fromChars "be"
                                    , D.fromChars "something"
                                    , D.fromChars "else"
                                    , D.fromChars "though,"
                                    , D.fromChars "so"
                                    , D.fromChars "here"
                                    , D.fromChars "is"
                                    , D.fromChars "a"
                                    , D.fromChars "valid"
                                    , D.fromChars "definition"
                                    , D.fromChars "for"
                                    , D.fromChars "reference:"
                                    ]
                                , D.indent 4 <|
                                    D.vcat
                                        [ D.fromChars "greet : String -> String"
                                        , D.fromChars "greet name ="
                                        , D.fromChars "  "
                                            |> D.a (D.dullyellow (D.fromChars "\"Hello \""))
                                            |> D.a (D.fromChars " ++ name ++ ")
                                            |> D.a (D.dullyellow (D.fromChars "\"!\""))
                                        ]
                                , D.reflow ("Try to use that format with your `" ++ name ++ "` definition!")
                                ]
                            )

                Code.Operator op ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col op
                    in
                    Report.Report "UNEXPECTED SYMBOL" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I was not expecting to see this symbol here:"
                            , D.stack
                                [ D.reflow "I am not sure what is going wrong exactly, so here is a valid definition (with an optional type annotation) for reference:"
                                , D.indent 4 <|
                                    D.vcat
                                        [ D.fromChars "greet : String -> String"
                                        , D.fromChars "greet name ="
                                        , D.fromChars "  "
                                            |> D.a (D.dullyellow (D.fromChars "\"Hello \""))
                                            |> D.a (D.fromChars " ++ name ++ ")
                                            |> D.a (D.dullyellow (D.fromChars "\"!\""))
                                        ]
                                , D.reflow ("Try to use that format with your `" ++ name ++ "` definition!")
                                ]
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN DEFINITION" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow ("I got stuck while parsing the `" ++ name ++ "` definition:")
                            , D.stack
                                [ D.reflow "I am not sure what is going wrong exactly, so here is a valid definition (with an optional type annotation) for reference:"
                                , D.indent 4 <|
                                    D.vcat
                                        [ D.fromChars "greet : String -> String"
                                        , D.fromChars "greet name ="
                                        , D.fromChars "  "
                                            |> D.a (D.dullyellow (D.fromChars "\"Hello \""))
                                            |> D.a (D.fromChars " ++ name ++ ")
                                            |> D.a (D.dullyellow (D.fromChars "\"!\""))
                                        ]
                                , D.reflow "Try to use that format!"
                                ]
                            )

        T.CRES_DefBody expr row col ->
            toExprReport source (InDef name startRow startCol) expr row col

        T.CRES_DefIndentEquals row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow ("I got stuck while parsing the `" ++ name ++ "` definition:")
                    , D.stack
                        [ D.reflow "I was expecting to see an argument or an equals sign next."
                        , defNote
                        ]
                    )

        T.CRES_DefIndentType row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow ("I got stuck while parsing the `" ++ name ++ "` type annotation:")
                    , D.stack
                        [ D.reflow "I just saw a colon, so I am expecting to see a type next."
                        , defNote
                        ]
                    )

        T.CRES_DefIndentBody row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow ("I got stuck while parsing the `" ++ name ++ "` definition:")
                    , D.stack
                        [ D.reflow "I was expecting to see an expression next. What is it equal to?"
                        , declDefNote
                        ]
                    )

        T.CRES_DefAlignment indent row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col

                offset : Int
                offset =
                    indent - col
            in
            Report.Report "PROBLEM IN DEFINITION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow ("I got stuck while parsing the `" ++ name ++ "` definition:")
                    , D.reflow
                        ("I just saw a type annotation indented "
                            ++ String.fromInt indent
                            ++ " spaces, so I was expecting to see the corresponding definition next with the exact same amount of indentation. It looks like this line needs "
                            ++ String.fromInt offset
                            ++ " more "
                            ++ (if offset == 1 then
                                    "space"

                                else
                                    "spaces"
                               )
                            ++ "?"
                        )
                    )


defNote : D.Doc
defNote =
    D.stack
        [ D.reflow "Here is a valid definition (with a type annotation) for reference:"
        , D.indent 4 <|
            D.vcat
                [ D.fromChars "greet : String -> String\n"
                , D.fromChars "greet name ="
                , D.fromChars "  "
                    |> D.a (D.dullyellow (D.fromChars "\"Hello \""))
                    |> D.a (D.fromChars " ++ name ++ ")
                    |> D.a (D.dullyellow (D.fromChars "\"!\""))
                ]
        , D.reflow "The top line (called a \"type annotation\") is optional. You can leave it off if you want. As you get more comfortable with Elm and as your project grows, it becomes more and more valuable to add them though! They work great as compiler-verified documentation, and they often improve error messages!"
        ]


toLetDestructReport : Code.Source -> T.CRES_Destruct -> T.CPP_Row -> T.CPP_Col -> Report.Report
toLetDestructReport source destruct startRow startCol =
    case destruct of
        T.CRES_DestructSpace space row col ->
            toSpaceReport source space row col

        T.CRES_DestructPattern pattern row col ->
            toPatternReport source PLet pattern row col

        T.CRES_DestructEquals row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN DEFINITION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I got stuck trying to parse this definition:"
                    , case Code.whatIsNext source row col of
                        Code.Operator ":" ->
                            D.stack
                                [ D.reflow "I was expecting to see an equals sign next, followed by an expression telling me what to compute."
                                , D.toSimpleNote "It looks like you may be trying to write a type annotation? It is not possible to add type annotations on destructuring definitions like this. You can assign a name to the overall structure, put a type annotation on that, and then destructure separately though."
                                ]

                        _ ->
                            D.reflow "I was expecting to see an equals sign next, followed by an expression telling me what to compute."
                    )

        T.CRES_DestructBody expr row col ->
            toExprReport source (InDestruct startRow startCol) expr row col

        T.CRES_DestructIndentEquals row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I got stuck trying to parse this definition:"
                    , D.reflow "I was expecting to see an equals sign next, followed by an expression telling me what to compute."
                    )

        T.CRES_DestructIndentBody row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I got stuck while parsing this definition:"
                    , D.reflow "I was expecting to see an expression next. What is it equal to?"
                    )



-- CASE


toCaseReport : Code.Source -> Context -> T.CRES_Case -> T.CPP_Row -> T.CPP_Col -> Report.Report
toCaseReport source context case_ startRow startCol =
    case case_ of
        T.CRES_CaseSpace space row col ->
            toSpaceReport source space row col

        T.CRES_CaseOf row col ->
            toUnfinishCaseReport source
                row
                col
                startRow
                startCol
                (D.fillSep
                    [ D.fromChars "I"
                    , D.fromChars "was"
                    , D.fromChars "expecting"
                    , D.fromChars "to"
                    , D.fromChars "see"
                    , D.fromChars "the"
                    , D.dullyellow <| D.fromChars "of"
                    , D.fromChars "keyword"
                    , D.fromChars "next."
                    ]
                )

        T.CRES_CasePattern pattern row col ->
            toPatternReport source PCase pattern row col

        T.CRES_CaseArrow row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        (Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I am partway through parsing a `case` expression, but I got stuck here:"
                            , D.reflow ("It looks like you are trying to use `" ++ keyword ++ "` in one of your patterns, but it is a reserved word. Try using a different name?")
                            )
                        )

                Code.Operator ":" ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "UNEXPECTED OPERATOR" region [] <|
                        (Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I am partway through parsing a `case` expression, but I got stuck here:"
                            , D.fillSep
                                [ D.fromChars "I"
                                , D.fromChars "am"
                                , D.fromChars "seeing"
                                , D.dullyellow <| D.fromChars ":"
                                , D.fromChars "but"
                                , D.fromChars "maybe"
                                , D.fromChars "you"
                                , D.fromChars "want"
                                , D.green <| D.fromChars "::"
                                , D.fromChars "instead?"
                                , D.fromChars "For"
                                , D.fromChars "pattern"
                                , D.fromChars "matching"
                                , D.fromChars "on"
                                , D.fromChars "lists?"
                                ]
                            )
                        )

                Code.Operator "=" ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "UNEXPECTED OPERATOR" region [] <|
                        (Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I am partway through parsing a `case` expression, but I got stuck here:"
                            , D.fillSep
                                [ D.fromChars "I"
                                , D.fromChars "am"
                                , D.fromChars "seeing"
                                , D.dullyellow <| D.fromChars "="
                                , D.fromChars "but"
                                , D.fromChars "maybe"
                                , D.fromChars "you"
                                , D.fromChars "want"
                                , D.green <| D.fromChars "->"
                                , D.fromChars "instead?"
                                ]
                            )
                        )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "MISSING ARROW" region [] <|
                        (Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I am partway through parsing a `case` expression, but I got stuck here:"
                            , D.stack
                                [ D.reflow "I was expecting to see an arrow next."
                                , noteForCaseIndentError
                                ]
                            )
                        )

        T.CRES_CaseExpr expr row col ->
            toExprReport source (InNode NCase startRow startCol context) expr row col

        T.CRES_CaseBranch expr row col ->
            toExprReport source (InNode NBranch startRow startCol context) expr row col

        T.CRES_CaseIndentOf row col ->
            toUnfinishCaseReport source
                row
                col
                startRow
                startCol
                (D.fillSep
                    [ D.fromChars "I"
                    , D.fromChars "was"
                    , D.fromChars "expecting"
                    , D.fromChars "to"
                    , D.fromChars "see"
                    , D.fromChars "the"
                    , D.dullyellow <| D.fromChars "of"
                    , D.fromChars "keyword"
                    , D.fromChars "next."
                    ]
                )

        T.CRES_CaseIndentExpr row col ->
            toUnfinishCaseReport source
                row
                col
                startRow
                startCol
                (D.reflow "I was expecting to see an expression next.")

        T.CRES_CaseIndentPattern row col ->
            toUnfinishCaseReport source
                row
                col
                startRow
                startCol
                (D.reflow "I was expecting to see a pattern next.")

        T.CRES_CaseIndentArrow row col ->
            toUnfinishCaseReport source
                row
                col
                startRow
                startCol
                (D.fillSep
                    [ D.fromChars "I"
                    , D.fromChars "just"
                    , D.fromChars "saw"
                    , D.fromChars "a"
                    , D.fromChars "pattern,"
                    , D.fromChars "so"
                    , D.fromChars "I"
                    , D.fromChars "was"
                    , D.fromChars "expecting"
                    , D.fromChars "to"
                    , D.fromChars "see"
                    , D.fromChars "a"
                    , D.dullyellow <| D.fromChars "->"
                    , D.fromChars "next."
                    ]
                )

        T.CRES_CaseIndentBranch row col ->
            toUnfinishCaseReport source
                row
                col
                startRow
                startCol
                (D.reflow "I was expecting to see an expression next. What should I do when I run into this particular pattern?")

        T.CRES_CasePatternAlignment indent row col ->
            toUnfinishCaseReport source
                row
                col
                startRow
                startCol
                (D.reflow ("I suspect this is a pattern that is not indented far enough? (" ++ String.fromInt indent ++ " spaces)"))


toUnfinishCaseReport : Code.Source -> Int -> Int -> Int -> Int -> D.Doc -> Report.Report
toUnfinishCaseReport source row col startRow startCol message =
    let
        surroundings : T.CRA_Region
        surroundings =
            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

        region : T.CRA_Region
        region =
            toRegion row col
    in
    Report.Report "UNFINISHED CASE" region [] <|
        Code.toSnippet source surroundings (Just region) <|
            ( D.reflow "I was partway through parsing a `case` expression, but I got stuck here:"
            , D.stack
                [ message
                , noteForCaseError
                ]
            )


noteForCaseError : D.Doc
noteForCaseError =
    D.stack
        [ D.toSimpleNote "Here is an example of a valid `case` expression for reference."
        , D.vcat
            [ D.indent 4 (D.fillSep [ D.cyan (D.fromChars "case"), D.fromChars "maybeWidth", D.cyan (D.fromChars "of") ])
            , D.indent 6 (D.fillSep [ D.blue (D.fromChars "Just"), D.fromChars "width", D.fromChars "->" ])
            , D.indent 8 (D.fillSep [ D.fromChars "width", D.fromChars "+", D.dullyellow (D.fromChars "200") ])
            , D.fromChars ""
            , D.indent 6 (D.fillSep [ D.blue (D.fromChars "Nothing"), D.fromChars "->" ])
            , D.indent 8 (D.fillSep [ D.dullyellow (D.fromChars "400") ])
            ]
        , D.reflow "Notice the indentation. Each pattern is aligned, and each branch is indented a bit more than the corresponding pattern. That is important!"
        ]


noteForCaseIndentError : D.Doc
noteForCaseIndentError =
    D.stack
        [ D.toSimpleNote "Sometimes I get confused by indentation, so try to make your `case` look something like this:"
        , D.vcat
            [ D.indent 4 (D.fillSep [ D.cyan (D.fromChars "case"), D.fromChars "maybeWidth", D.cyan (D.fromChars "of") ])
            , D.indent 6 (D.fillSep [ D.blue (D.fromChars "Just"), D.fromChars "width", D.fromChars "->" ])
            , D.indent 8 (D.fillSep [ D.fromChars "width", D.fromChars "+", D.dullyellow (D.fromChars "200") ])
            , D.fromChars ""
            , D.indent 6 (D.fillSep [ D.blue (D.fromChars "Nothing"), D.fromChars "->" ])
            , D.indent 8 (D.fillSep [ D.dullyellow (D.fromChars "400") ])
            ]
        , D.reflow "Notice the indentation! Patterns are aligned with each other. Same indentation. The expressions after each arrow are all indented a bit more than the patterns. That is important!"
        ]



-- IF


toIfReport : Code.Source -> Context -> T.CRES_If -> T.CPP_Row -> T.CPP_Col -> Report.Report
toIfReport source context if_ startRow startCol =
    case if_ of
        T.CRES_IfSpace space row col ->
            toSpaceReport source space row col

        T.CRES_IfThen row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was expecting to see more of this `if` expression, but I got stuck here:"
                    , D.fillSep
                        [ D.fromChars "I"
                        , D.fromChars "was"
                        , D.fromChars "expecting"
                        , D.fromChars "to"
                        , D.fromChars "see"
                        , D.fromChars "the"
                        , D.cyan <| D.fromChars "then"
                        , D.fromChars "keyword"
                        , D.fromChars "next."
                        ]
                    )

        T.CRES_IfElse row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was expecting to see more of this `if` expression, but I got stuck here:"
                    , D.fillSep
                        [ D.fromChars "I"
                        , D.fromChars "was"
                        , D.fromChars "expecting"
                        , D.fromChars "to"
                        , D.fromChars "see"
                        , D.fromChars "the"
                        , D.cyan <| D.fromChars "else"
                        , D.fromChars "keyword"
                        , D.fromChars "next."
                        ]
                    )

        T.CRES_IfElseBranchStart row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I just saw the start of an `else` branch, but then I got stuck here:"
                    , D.reflow "I was expecting to see an expression next. Maybe it is not filled in yet?"
                    )

        T.CRES_IfCondition expr row col ->
            toExprReport source (InNode NCond startRow startCol context) expr row col

        T.CRES_IfThenBranch expr row col ->
            toExprReport source (InNode NThen startRow startCol context) expr row col

        T.CRES_IfElseBranch expr row col ->
            toExprReport source (InNode NElse startRow startCol context) expr row col

        T.CRES_IfIndentCondition row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was expecting to see more of this `if` expression, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "an"
                            , D.fromChars "expression"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "x < 0"
                            , D.fromChars "that"
                            , D.fromChars "evaluates"
                            , D.fromChars "to"
                            , D.fromChars "True"
                            , D.fromChars "or"
                            , D.fromChars "False."
                            ]
                        , D.toSimpleNote "I can be confused by indentation. Maybe something is not indented enough?"
                        ]
                    )

        T.CRES_IfIndentThen row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was expecting to see more of this `if` expression, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "the"
                            , D.cyan <| D.fromChars "then"
                            , D.fromChars "keyword"
                            , D.fromChars "next."
                            ]
                        , D.toSimpleNote "I can be confused by indentation. Maybe something is not indented enough?"
                        ]
                    )

        T.CRES_IfIndentThenBranch row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I got stuck after the start of this `then` branch:"
                    , D.stack
                        [ D.reflow "I was expecting to see an expression next. Maybe it is not filled in yet?"
                        , D.toSimpleNote "I can be confused by indentation, so if the `then` branch is already present, it may not be indented enough for me to recognize it."
                        ]
                    )

        T.CRES_IfIndentElseBranch row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I got stuck after the start of this `else` branch:"
                    , D.stack
                        [ D.reflow "I was expecting to see an expression next. Maybe it is not filled in yet?"
                        , D.toSimpleNote "I can be confused by indentation, so if the `else` branch is already present, it may not be indented enough for me to recognize it."
                        ]
                    )

        T.CRES_IfIndentElse row col ->
            case Code.nextLineStartsWithKeyword "else" source row of
                Just ( elseRow, elseCol ) ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position elseRow elseCol)

                        region : T.CRA_Region
                        region =
                            toWiderRegion elseRow elseCol 4
                    in
                    Report.Report "WEIRD ELSE BRANCH" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I was partway through an `if` expression when I got stuck here:"
                            , D.fillSep
                                [ D.fromChars "I"
                                , D.fromChars "think"
                                , D.fromChars "this"
                                , D.cyan <| D.fromChars "else"
                                , D.fromChars "keyword"
                                , D.fromChars "needs"
                                , D.fromChars "to"
                                , D.fromChars "be"
                                , D.fromChars "indented"
                                , D.fromChars "more."
                                , D.fromChars "Try"
                                , D.fromChars "adding"
                                , D.fromChars "some"
                                , D.fromChars "spaces"
                                , D.fromChars "before"
                                , D.fromChars "it."
                                ]
                            )

                Nothing ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED IF" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I was expecting to see an `else` branch after this:"
                            , D.stack
                                [ D.fillSep
                                    [ D.fromChars "I"
                                    , D.fromChars "know"
                                    , D.fromChars "what"
                                    , D.fromChars "to"
                                    , D.fromChars "do"
                                    , D.fromChars "when"
                                    , D.fromChars "the"
                                    , D.fromChars "condition"
                                    , D.fromChars "is"
                                    , D.fromChars "True,"
                                    , D.fromChars "but"
                                    , D.fromChars "what"
                                    , D.fromChars "happens"
                                    , D.fromChars "when"
                                    , D.fromChars "it"
                                    , D.fromChars "is"
                                    , D.fromChars "False?"
                                    , D.fromChars "Add"
                                    , D.fromChars "an"
                                    , D.cyan <| D.fromChars "else"
                                    , D.fromChars "branch"
                                    , D.fromChars "to"
                                    , D.fromChars "handle"
                                    , D.fromChars "that"
                                    , D.fromChars "scenario!"
                                    ]
                                ]
                            )



-- RECORD


toRecordReport : Code.Source -> Context -> T.CRES_Record -> T.CPP_Row -> T.CPP_Col -> Report.Report
toRecordReport source context record startRow startCol =
    case record of
        T.CRES_RecordOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I just started parsing a record, but I got stuck on this field name:"
                            , D.reflow <|
                                ("It looks like you are trying to use `" ++ keyword ++ "` as a field name, but that is a reserved word. Try using a different name!")
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN RECORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I just started parsing a record, but I got stuck here:"
                            , D.stack
                                [ D.fillSep
                                    [ D.fromChars "I"
                                    , D.fromChars "was"
                                    , D.fromChars "expecting"
                                    , D.fromChars "to"
                                    , D.fromChars "see"
                                    , D.fromChars "a"
                                    , D.fromChars "record"
                                    , D.fromChars "field"
                                    , D.fromChars "defined"
                                    , D.fromChars "next,"
                                    , D.fromChars "so"
                                    , D.fromChars "I"
                                    , D.fromChars "am"
                                    , D.fromChars "looking"
                                    , D.fromChars "for"
                                    , D.fromChars "a"
                                    , D.fromChars "name"
                                    , D.fromChars "like"
                                    , D.dullyellow <| D.fromChars "userName"
                                    , D.fromChars "or"
                                    , D.dullyellow <| D.fromChars "plantHeight"
                                    , D.fromChars "."
                                    ]
                                , D.toSimpleNote "Field names must start with a lower-case letter. After that, you can use any sequence of letters, numbers, and underscores."
                                , noteForRecordError
                                ]
                            )

        T.CRES_RecordEnd row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN RECORD" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a record, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "a"
                            , D.fromChars "closing"
                            , D.fromChars "curly"
                            , D.fromChars "brace"
                            , D.fromChars "before"
                            , D.fromChars "this,"
                            , D.fromChars "so"
                            , D.fromChars "try"
                            , D.fromChars "adding"
                            , D.fromChars "a"
                            , D.dullyellow <| D.fromChars "}"
                            , D.fromChars "and"
                            , D.fromChars "see"
                            , D.fromChars "if"
                            , D.fromChars "that"
                            , D.fromChars "helps?"
                            ]
                        , D.toSimpleNote "When I get stuck like this, it usually means that there is a missing parenthesis or bracket somewhere earlier. It could also be a stray keyword or operator."
                        ]
                    )

        T.CRES_RecordField row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I am partway through parsing a record, but I got stuck on this field name:"
                            , D.reflow <| ("It looks like you are trying to use `" ++ keyword ++ "` as a field name, but that is a reserved word. Try using a different name!")
                            )

                Code.Other (Just ',') ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "EXTRA COMMA" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I am partway through parsing a record, but I got stuck here:"
                            , D.stack
                                [ D.reflow "I am seeing two commas in a row. This is the second one!"
                                , D.reflow "Just delete one of the commas and you should be all set!"
                                , noteForRecordError
                                ]
                            )

                Code.Close _ '}' ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "EXTRA COMMA" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I am partway through parsing a record, but I got stuck here:"
                            , D.stack
                                [ D.reflow "Trailing commas are not allowed in records. Try deleting the comma that appears before this closing curly brace."
                                , noteForRecordError
                                ]
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN RECORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I am partway through parsing a record, but I got stuck here:"
                            , D.stack
                                [ D.fillSep
                                    [ D.fromChars "I"
                                    , D.fromChars "was"
                                    , D.fromChars "expecting"
                                    , D.fromChars "to"
                                    , D.fromChars "see"
                                    , D.fromChars "another"
                                    , D.fromChars "record"
                                    , D.fromChars "field"
                                    , D.fromChars "defined"
                                    , D.fromChars "next,"
                                    , D.fromChars "so"
                                    , D.fromChars "I"
                                    , D.fromChars "am"
                                    , D.fromChars "looking"
                                    , D.fromChars "for"
                                    , D.fromChars "a"
                                    , D.fromChars "name"
                                    , D.fromChars "like"
                                    , D.dullyellow <| D.fromChars "userName"
                                    , D.fromChars "or"
                                    , D.dullyellow <| D.fromChars "plantHeight"
                                    , D.fromChars "."
                                    ]
                                , D.toSimpleNote "Field names must start with a lower-case letter. After that, you can use any sequence of letters, numbers, and underscores."
                                , noteForRecordError
                                ]
                            )

        T.CRES_RecordEquals row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN RECORD" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a record, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "just"
                            , D.fromChars "saw"
                            , D.fromChars "a"
                            , D.fromChars "field"
                            , D.fromChars "name,"
                            , D.fromChars "so"
                            , D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "an"
                            , D.fromChars "equals"
                            , D.fromChars "sign"
                            , D.fromChars "next."
                            , D.fromChars "So"
                            , D.fromChars "try"
                            , D.fromChars "putting"
                            , D.fromChars "an"
                            , D.green <| D.fromChars "="
                            , D.fromChars "sign"
                            , D.fromChars "here?"
                            ]
                        , noteForRecordError
                        ]
                    )

        T.CRES_RecordExpr expr row col ->
            toExprReport source (InNode NRecord startRow startCol context) expr row col

        T.CRES_RecordSpace space row col ->
            toSpaceReport source space row col

        T.CRES_RecordIndentOpen row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I just saw the opening curly brace of a record, but then I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "am"
                            , D.fromChars "expecting"
                            , D.fromChars "a"
                            , D.fromChars "record"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "{ x = 3, y = 4 }"
                            , D.fromChars "here."
                            , D.fromChars "Try"
                            , D.fromChars "defining"
                            , D.fromChars "some"
                            , D.fromChars "fields"
                            , D.fromChars "of"
                            , D.fromChars "your"
                            , D.fromChars "own?"
                            ]
                        , noteForRecordIndentError
                        ]
                    )

        T.CRES_RecordIndentEnd row col ->
            case Code.nextLineStartsWithCloseCurly source row of
                Just ( curlyRow, curlyCol ) ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position curlyRow curlyCol)

                        region : T.CRA_Region
                        region =
                            toRegion curlyRow curlyCol
                    in
                    Report.Report "NEED MORE INDENTATION" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I was partway through parsing a record, but I got stuck here:"
                            , D.stack
                                [ D.reflow "I need this curly brace to be indented more. Try adding some spaces before it!"
                                , noteForRecordError
                                ]
                            )

                Nothing ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED RECORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I was partway through parsing a record, but I got stuck here:"
                            , D.stack
                                [ D.fillSep
                                    [ D.fromChars "I"
                                    , D.fromChars "was"
                                    , D.fromChars "expecting"
                                    , D.fromChars "to"
                                    , D.fromChars "see"
                                    , D.fromChars "a"
                                    , D.fromChars "closing"
                                    , D.fromChars "curly"
                                    , D.fromChars "brace"
                                    , D.fromChars "next."
                                    , D.fromChars "Try"
                                    , D.fromChars "putting"
                                    , D.fromChars "a"
                                    , D.green <| D.fromChars "}"
                                    , D.fromChars "next"
                                    , D.fromChars "and"
                                    , D.fromChars "see"
                                    , D.fromChars "if"
                                    , D.fromChars "that"
                                    , D.fromChars "helps?"
                                    ]
                                , noteForRecordIndentError
                                ]
                            )

        T.CRES_RecordIndentField row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a record, but I got stuck after that last comma:"
                    , D.stack
                        [ D.reflow "Trailing commas are not allowed in records, so the fix may be to delete that last comma? Or maybe you were in the middle of defining an additional field?"
                        , noteForRecordError
                        ]
                    )

        T.CRES_RecordIndentEquals row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a record. I just saw a record field, so I was expecting to see an equals sign next:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Try"
                            , D.fromChars "putting"
                            , D.fromChars "an"
                            , D.green <| D.fromChars "="
                            , D.fromChars "followed"
                            , D.fromChars "by"
                            , D.fromChars "an"
                            , D.fromChars "expression?"
                            ]
                        , noteForRecordIndentError
                        ]
                    )

        T.CRES_RecordIndentExpr row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a record, and I was expecting to run into an expression next:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Try"
                            , D.fromChars "putting"
                            , D.fromChars "something"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "42"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "\"hello\""
                            , D.fromChars "for"
                            , D.fromChars "now?"
                            ]
                        , noteForRecordIndentError
                        ]
                    )


noteForRecordError : D.Doc
noteForRecordError =
    D.stack
        [ D.toSimpleNote "If you are trying to define a record across multiple lines, I recommend using this format:"
        , D.indent 4 <|
            D.vcat
                [ D.fromChars "{ name = " |> D.a (D.dullyellow (D.fromChars "\"Alice\""))
                , D.fromChars ", age = " |> D.a (D.dullyellow (D.fromChars "42"))
                , D.fromChars ", height = " |> D.a (D.dullyellow (D.fromChars "1.75"))
                , D.fromChars "}"
                ]
        , D.reflow "Notice that each line starts with some indentation. Usually two or four spaces. This is the stylistic convention in the Elm ecosystem."
        ]


noteForRecordIndentError : D.Doc
noteForRecordIndentError =
    D.stack
        [ D.toSimpleNote "I may be confused by indentation. For example, if you are trying to define a record across multiple lines, I recommend using this format:"
        , D.indent 4 <|
            D.vcat
                [ D.fromChars "{ name = " |> D.a (D.dullyellow (D.fromChars "\"Alice\""))
                , D.fromChars ", age = " |> D.a (D.dullyellow (D.fromChars "42"))
                , D.fromChars ", height = " |> D.a (D.dullyellow (D.fromChars "1.75"))
                , D.fromChars "}"
                ]
        , D.reflow "Notice that each line starts with some indentation. Usually two or four spaces. This is the stylistic convention in the Elm ecosystem!"
        ]



-- TUPLE


toTupleReport : Code.Source -> Context -> T.CRES_Tuple -> T.CPP_Row -> T.CPP_Col -> Report.Report
toTupleReport source context tuple startRow startCol =
    case tuple of
        T.CRES_TupleExpr expr row col ->
            toExprReport source (InNode NParens startRow startCol context) expr row col

        T.CRES_TupleSpace space row col ->
            toSpaceReport source space row col

        T.CRES_TupleEnd row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was expecting to see a closing parentheses next, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Try"
                            , D.fromChars "adding"
                            , D.fromChars "a"
                            , D.dullyellow <| D.fromChars ")"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "if"
                            , D.fromChars "that"
                            , D.fromChars "helps?"
                            ]
                        , D.toSimpleNote "I can get stuck when I run into keywords, operators, parentheses, or brackets unexpectedly. So there may be some earlier syntax trouble (like extra parenthesis or missing brackets) that is confusing me."
                        ]
                    )

        T.CRES_TupleOperatorClose row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED OPERATOR FUNCTION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was expecting a closing parenthesis here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Try"
                            , D.fromChars "adding"
                            , D.fromChars "a"
                            , D.dullyellow <| D.fromChars ")"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "if"
                            , D.fromChars "that"
                            , D.fromChars "helps!"
                            ]
                        , D.toSimpleNote "I think I am parsing an operator function right now, so I am expecting to see something like (+) or (&&) where an operator is surrounded by parentheses with no extra spaces."
                        ]
                    )

        T.CRES_TupleOperatorReserved operator row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED SYMBOL" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I ran into an unexpected symbol here:"
                    , D.fillSep
                        (case operator of
                            T.CPS_BadDot ->
                                [ D.fromChars "Maybe"
                                , D.fromChars "you"
                                , D.fromChars "wanted"
                                , D.fromChars "a"
                                , D.fromChars "record"
                                , D.fromChars "accessor"
                                , D.fromChars "like"
                                , D.dullyellow <| D.fromChars ".x"
                                , D.fromChars "or"
                                , D.dullyellow <| D.fromChars ".name"
                                , D.fromChars "instead?"
                                ]

                            T.CPS_BadPipe ->
                                [ D.fromChars "Try"
                                , D.dullyellow <| D.fromChars "(||)"
                                , D.fromChars "instead?"
                                , D.fromChars "To"
                                , D.fromChars "turn"
                                , D.fromChars "boolean"
                                , D.fromChars "OR"
                                , D.fromChars "into"
                                , D.fromChars "a"
                                , D.fromChars "function?"
                                ]

                            T.CPS_BadArrow ->
                                [ D.fromChars "Maybe"
                                , D.fromChars "you"
                                , D.fromChars "wanted"
                                , D.dullyellow <| D.fromChars "(>)"
                                , D.fromChars "or"
                                , D.dullyellow <| D.fromChars "(>=)"
                                , D.fromChars "instead?"
                                ]

                            T.CPS_BadEquals ->
                                [ D.fromChars "Try"
                                , D.dullyellow <| D.fromChars "(==)"
                                , D.fromChars "instead?"
                                , D.fromChars "To"
                                , D.fromChars "make"
                                , D.fromChars "a"
                                , D.fromChars "function"
                                , D.fromChars "that"
                                , D.fromChars "checks"
                                , D.fromChars "equality?"
                                ]

                            T.CPS_BadHasType ->
                                [ D.fromChars "Try"
                                , D.dullyellow <| D.fromChars "(::)"
                                , D.fromChars "instead?"
                                , D.fromChars "To"
                                , D.fromChars "add"
                                , D.fromChars "values"
                                , D.fromChars "to"
                                , D.fromChars "the"
                                , D.fromChars "front"
                                , D.fromChars "of"
                                , D.fromChars "lists?"
                                ]
                        )
                    )

        T.CRES_TupleIndentExpr1 row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I just saw an open parenthesis, so I was expecting to see an expression next."
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Something"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "(4 + 5)"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "(String.reverse \"desserts\")"
                            , D.fromChars "."
                            , D.fromChars "Anything"
                            , D.fromChars "where"
                            , D.fromChars "you"
                            , D.fromChars "are"
                            , D.fromChars "putting"
                            , D.fromChars "parentheses"
                            , D.fromChars "around"
                            , D.fromChars "normal"
                            , D.fromChars "expressions."
                            ]
                        , D.toSimpleNote "I can get confused by indentation in cases like this, so maybe you have an expression but it is not indented enough?"
                        ]
                    )

        T.CRES_TupleIndentExprN row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED TUPLE" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I think I am in the middle of parsing a tuple. I just saw a comma, so I was expecting to see an expression next."
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "A"
                            , D.fromChars "tuple"
                            , D.fromChars "looks"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "(3,4)"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "(\"Tom\",42)"
                            , D.fromChars ","
                            , D.fromChars "so"
                            , D.fromChars "I"
                            , D.fromChars "think"
                            , D.fromChars "there"
                            , D.fromChars "is"
                            , D.fromChars "an"
                            , D.fromChars "expression"
                            , D.fromChars "missing"
                            , D.fromChars "here?"
                            ]
                        , D.toSimpleNote "I can get confused by indentation in cases like this, so maybe you have an expression but it is not indented enough?"
                        ]
                    )

        T.CRES_TupleIndentEnd row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was expecting to see a closing parenthesis next:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Try"
                            , D.fromChars "adding"
                            , D.fromChars "a"
                            , D.dullyellow <| D.fromChars ")"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "if"
                            , D.fromChars "that"
                            , D.fromChars "helps!"
                            ]
                        , D.toSimpleNote "I can get confused by indentation in cases like this, so maybe you have a closing parenthesis but it is not indented enough?"
                        ]
                    )


toListReport : Code.Source -> Context -> T.CRES_List_ -> T.CPP_Row -> T.CPP_Col -> Report.Report
toListReport source context list startRow startCol =
    case list of
        T.CRES_ListSpace space row col ->
            toSpaceReport source space row col

        T.CRES_ListOpen row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a list, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "a"
                            , D.fromChars "closing"
                            , D.fromChars "square"
                            , D.fromChars "bracket"
                            , D.fromChars "before"
                            , D.fromChars "this,"
                            , D.fromChars "so"
                            , D.fromChars "try"
                            , D.fromChars "adding"
                            , D.fromChars "a"
                            , D.dullyellow <| D.fromChars "]"
                            , D.fromChars "and"
                            , D.fromChars "see"
                            , D.fromChars "if"
                            , D.fromChars "that"
                            , D.fromChars "helps?"
                            ]
                        , D.toSimpleNote
                            "When I get stuck like this, it usually means that there is a missing parenthesis or bracket somewhere earlier. It could also be a stray keyword or operator."
                        ]
                    )

        T.CRES_ListExpr expr row col ->
            case expr of
                T.CRES_Start r c ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position r c)

                        region : T.CRA_Region
                        region =
                            toRegion r c
                    in
                    Report.Report "UNFINISHED LIST" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I was expecting to see another list entry after that last comma:"
                            , D.stack
                                [ D.reflow "Trailing commas are not allowed in lists, so the fix may be to delete the comma?"
                                , D.toSimpleNote "I recommend using the following format for lists that span multiple lines:"
                                , D.indent 4 <|
                                    D.vcat
                                        [ D.fromChars "[ " |> D.a (D.dullyellow (D.fromChars "\"Alice\""))
                                        , D.fromChars ", " |> D.a (D.dullyellow (D.fromChars "\"Bob\""))
                                        , D.fromChars ", " |> D.a (D.dullyellow (D.fromChars "\"Chuck\""))
                                        , D.fromChars "]"
                                        ]
                                , D.reflow "Notice that each line starts with some indentation. Usually two or four spaces. This is the stylistic convention in the Elm ecosystem."
                                ]
                            )

                _ ->
                    toExprReport source (InNode NList startRow startCol context) expr row col

        T.CRES_ListEnd row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a list, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "a"
                            , D.fromChars "closing"
                            , D.fromChars "square"
                            , D.fromChars "bracket"
                            , D.fromChars "before"
                            , D.fromChars "this,"
                            , D.fromChars "so"
                            , D.fromChars "try"
                            , D.fromChars "adding"
                            , D.fromChars "a"
                            , D.dullyellow <| D.fromChars "]"
                            , D.fromChars "and"
                            , D.fromChars "see"
                            , D.fromChars "if"
                            , D.fromChars "that"
                            , D.fromChars "helps?"
                            ]
                        , D.toSimpleNote
                            "When I get stuck like this, it usually means that there is a missing parenthesis or bracket somewhere earlier. It could also be a stray keyword or operator."
                        ]
                    )

        T.CRES_ListIndentOpen row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I cannot find the end of this list:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "You"
                            , D.fromChars "could"
                            , D.fromChars "change"
                            , D.fromChars "it"
                            , D.fromChars "to"
                            , D.fromChars "something"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "[3,4,5]"
                            , D.fromChars "or"
                            , D.fromChars "even"
                            , D.fromChars "just"
                            , D.dullyellow <| D.fromChars "[]"
                            , D.fromChars "."
                            , D.fromChars "Anything"
                            , D.fromChars "where"
                            , D.fromChars "there"
                            , D.fromChars "is"
                            , D.fromChars "an"
                            , D.fromChars "open"
                            , D.fromChars "and"
                            , D.fromChars "close"
                            , D.fromChars "square"
                            , D.fromChars "brace,"
                            , D.fromChars "and"
                            , D.fromChars "where"
                            , D.fromChars "the"
                            , D.fromChars "elements"
                            , D.fromChars "of"
                            , D.fromChars "the"
                            , D.fromChars "list"
                            , D.fromChars "are"
                            , D.fromChars "separated"
                            , D.fromChars "by"
                            , D.fromChars "commas."
                            ]
                        , D.toSimpleNote
                            "I may be confused by indentation. For example, if you are trying to define a list across multiple lines, I recommend using this format:"
                        , D.indent 4 <|
                            D.vcat
                                [ D.fromChars "[ " |> D.a (D.dullyellow (D.fromChars "\"Alice\""))
                                , D.fromChars ", " |> D.a (D.dullyellow (D.fromChars "\"Bob\""))
                                , D.fromChars ", " |> D.a (D.dullyellow (D.fromChars "\"Chuck\""))
                                , D.fromChars "]"
                                ]
                        , D.reflow "Notice that each line starts with some indentation. Usually two or four spaces. This is the stylistic convention in the Elm ecosystem."
                        ]
                    )

        T.CRES_ListIndentEnd row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I cannot find the end of this list:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "You"
                            , D.fromChars "can"
                            , D.fromChars "just"
                            , D.fromChars "add"
                            , D.fromChars "a"
                            , D.fromChars "closing"
                            , D.dullyellow <| D.fromChars "]"
                            , D.fromChars "right"
                            , D.fromChars "here,"
                            , D.fromChars "and"
                            , D.fromChars "I"
                            , D.fromChars "will"
                            , D.fromChars "be"
                            , D.fromChars "all"
                            , D.fromChars "set!"
                            ]
                        , D.toSimpleNote
                            "I may be confused by indentation. For example, if you are trying to define a list across multiple lines, I recommend using this format:"
                        , D.indent 4 <|
                            D.vcat
                                [ D.fromChars "[ " |> D.a (D.dullyellow (D.fromChars "\"Alice\""))
                                , D.fromChars ", " |> D.a (D.dullyellow (D.fromChars "\"Bob\""))
                                , D.fromChars ", " |> D.a (D.dullyellow (D.fromChars "\"Chuck\""))
                                , D.fromChars "]"
                                ]
                        , D.reflow "Notice that each line starts with some indentation. Usually two or four spaces. This is the stylistic convention in the Elm ecosystem."
                        ]
                    )

        T.CRES_ListIndentExpr row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was expecting to see another list entry after this comma:"
                    , D.stack
                        [ D.reflow "Trailing commas are not allowed in lists, so the fix may be to delete the comma?"
                        , D.toSimpleNote "I recommend using the following format for lists that span multiple lines:"
                        , D.indent 4 <|
                            D.vcat
                                [ D.fromChars "[ " |> D.a (D.dullyellow (D.fromChars "\"Alice\""))
                                , D.fromChars ", " |> D.a (D.dullyellow (D.fromChars "\"Bob\""))
                                , D.fromChars ", " |> D.a (D.dullyellow (D.fromChars "\"Chuck\""))
                                , D.fromChars "]"
                                ]
                        , D.reflow "Notice that each line starts with some indentation. Usually two or four spaces. This is the stylistic convention in the Elm ecosystem."
                        ]
                    )


toFuncReport : Code.Source -> Context -> T.CRES_Func -> T.CPP_Row -> T.CPP_Col -> Report.Report
toFuncReport source context func startRow startCol =
    case func of
        T.CRES_FuncSpace space row col ->
            toSpaceReport source space row col

        T.CRES_FuncArg pattern row col ->
            toPatternReport source PArg pattern row col

        T.CRES_FuncBody expr row col ->
            toExprReport source (InNode NFunc startRow startCol context) expr row col

        T.CRES_FuncArrow row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I was parsing an anonymous function, but I got stuck here:"
                            , D.reflow ("It looks like you are trying to use `" ++ keyword ++ "` as an argument, but it is a reserved word in this language. Try using a different argument name!")
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED ANONYMOUS FUNCTION" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I just saw the beginning of an anonymous function, so I was expecting to see an arrow next:"
                            , D.fillSep
                                [ D.fromChars "The"
                                , D.fromChars "syntax"
                                , D.fromChars "for"
                                , D.fromChars "anonymous"
                                , D.fromChars "functions"
                                , D.fromChars "is"
                                , D.dullyellow <| D.fromChars "(\\x -> x + 1)"
                                , D.fromChars "so"
                                , D.fromChars "I"
                                , D.fromChars "am"
                                , D.fromChars "missing"
                                , D.fromChars "the"
                                , D.fromChars "arrow"
                                , D.fromChars "and"
                                , D.fromChars "the"
                                , D.fromChars "body"
                                , D.fromChars "of"
                                , D.fromChars "the"
                                , D.fromChars "function."
                                ]
                            )

        T.CRES_FuncIndentArg row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "MISSING ARGUMENT" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I just saw the beginning of an anonymous function, so I was expecting to see an argument next:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Something"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "x"
                            , D.fromChars "or"
                            , D.dullyellow (D.fromChars "name") |> D.a (D.fromChars ".")
                            , D.fromChars "Anything"
                            , D.fromChars "that"
                            , D.fromChars "starts"
                            , D.fromChars "with"
                            , D.fromChars "a"
                            , D.fromChars "lower"
                            , D.fromChars "case"
                            , D.fromChars "letter!"
                            ]
                        , D.toSimpleNote "The syntax for anonymous functions is (\\x -> x + 1) where the backslash is meant to look a bit like a lambda if you squint. This visual pun seemed like a better idea at the time!"
                        ]
                    )

        T.CRES_FuncIndentArrow row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED ANONYMOUS FUNCTION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I just saw the beginning of an anonymous function, so I was expecting to see an arrow next:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "The"
                            , D.fromChars "syntax"
                            , D.fromChars "for"
                            , D.fromChars "anonymous"
                            , D.fromChars "functions"
                            , D.fromChars "is"
                            , D.dullyellow <| D.fromChars "(\\x -> x + 1)"
                            , D.fromChars "so"
                            , D.fromChars "I"
                            , D.fromChars "am"
                            , D.fromChars "missing"
                            , D.fromChars "the"
                            , D.fromChars "arrow"
                            , D.fromChars "and"
                            , D.fromChars "the"
                            , D.fromChars "body"
                            , D.fromChars "of"
                            , D.fromChars "the"
                            , D.fromChars "function."
                            ]
                        , D.toSimpleNote "It is possible that I am confused about indentation! I generally recommend switching to named functions if the definition cannot fit inline nicely, so either (1) try to fit the whole anonymous function on one line or (2) break the whole thing out into a named function. Things tend to be clearer that way!"
                        ]
                    )

        T.CRES_FuncIndentBody row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED ANONYMOUS FUNCTION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was expecting to see the body of your anonymous function next:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "The"
                            , D.fromChars "syntax"
                            , D.fromChars "for"
                            , D.fromChars "anonymous"
                            , D.fromChars "functions"
                            , D.fromChars "is"
                            , D.dullyellow <| D.fromChars "(\\x -> x + 1)"
                            , D.fromChars "so"
                            , D.fromChars "I"
                            , D.fromChars "am"
                            , D.fromChars "missing"
                            , D.fromChars "all"
                            , D.fromChars "the"
                            , D.fromChars "stuff"
                            , D.fromChars "after"
                            , D.fromChars "the"
                            , D.fromChars "arrow!"
                            ]
                        , D.toSimpleNote "It is possible that I am confused about indentation! I generally recommend switching to named functions if the definition cannot fit inline nicely, so either (1) try to fit the whole anonymous function on one line or (2) break the whole thing out into a named function. Things tend to be clearer that way!"
                        ]
                    )



-- PATTERN


type PContext
    = PCase
    | PArg
    | PLet


toPatternReport : Code.Source -> PContext -> T.CRES_Pattern -> T.CPP_Row -> T.CPP_Col -> Report.Report
toPatternReport source context pattern startRow startCol =
    case pattern of
        T.CRES_PRecord record row col ->
            toPRecordReport source record row col

        T.CRES_PTuple tuple row col ->
            toPTupleReport source context tuple row col

        T.CRES_PList list row col ->
            toPListReport source context list row col

        T.CRES_PStart row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword

                        inThisThing : String
                        inThisThing =
                            case context of
                                PArg ->
                                    "as an argument"

                                PCase ->
                                    "in this pattern"

                                PLet ->
                                    "in this pattern"
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                ("It looks like you are trying to use `" ++ keyword ++ "` " ++ inThisThing ++ ":")
                            , D.reflow <|
                                "This is a reserved word! Try using some other name?"
                            )

                Code.Operator "-" ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "UNEXPECTED SYMBOL" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I ran into a minus sign unexpectedly in this pattern:"
                            , D.reflow <|
                                "It is not possible to pattern match on negative numbers at this time. Try using an `if` expression for that sort of thing for now."
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN PATTERN" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I wanted to parse a pattern next, but I got stuck here:"
                            , D.fillSep
                                [ D.fromChars "I"
                                , D.fromChars "am"
                                , D.fromChars "not"
                                , D.fromChars "sure"
                                , D.fromChars "why"
                                , D.fromChars "I"
                                , D.fromChars "am"
                                , D.fromChars "getting"
                                , D.fromChars "stuck"
                                , D.fromChars "exactly."
                                , D.fromChars "I"
                                , D.fromChars "just"
                                , D.fromChars "know"
                                , D.fromChars "that"
                                , D.fromChars "I"
                                , D.fromChars "want"
                                , D.fromChars "a"
                                , D.fromChars "pattern"
                                , D.fromChars "next."
                                , D.fromChars "Something"
                                , D.fromChars "as"
                                , D.fromChars "simple"
                                , D.fromChars "as"
                                , D.dullyellow <| D.fromChars "maybeHeight"
                                , D.fromChars "or"
                                , D.dullyellow <| D.fromChars "result"
                                , D.fromChars "would"
                                , D.fromChars "work!"
                                ]
                            )

        T.CRES_PChar char row col ->
            toCharReport source char row col

        T.CRES_PString string row col ->
            toStringReport source string row col

        T.CRES_PNumber number row col ->
            toNumberReport source number row col

        T.CRES_PFloat width row col ->
            let
                region : T.CRA_Region
                region =
                    toWiderRegion row col width
            in
            Report.Report "UNEXPECTED PATTERN" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I cannot pattern match with floating point numbers:"
                    , D.fillSep
                        [ D.fromChars "Equality"
                        , D.fromChars "on"
                        , D.fromChars "floats"
                        , D.fromChars "can"
                        , D.fromChars "be"
                        , D.fromChars "unreliable,"
                        , D.fromChars "so"
                        , D.fromChars "you"
                        , D.fromChars "usually"
                        , D.fromChars "want"
                        , D.fromChars "to"
                        , D.fromChars "check"
                        , D.fromChars "that"
                        , D.fromChars "they"
                        , D.fromChars "are"
                        , D.fromChars "nearby"
                        , D.fromChars "with"
                        , D.fromChars "some"
                        , D.fromChars "sort"
                        , D.fromChars "of"
                        , D.dullyellow <| D.fromChars "(abs (actual - expected) < 0.001)"
                        , D.fromChars "check."
                        ]
                    )

        T.CRES_PAlias row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PATTERN" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was expecting to see a variable name after the `as` keyword:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "The"
                            , D.fromChars "`as`"
                            , D.fromChars "keyword"
                            , D.fromChars "lets"
                            , D.fromChars "you"
                            , D.fromChars "write"
                            , D.fromChars "patterns"
                            , D.fromChars "like"
                            , D.fromChars "(("
                                |> D.a (D.dullyellow (D.fromChars "x"))
                                |> D.a (D.fromChars ",")
                                |> D.a (D.dullyellow (D.fromChars "y"))
                                |> D.a (D.fromChars ") ")
                                |> D.a (D.cyan (D.fromChars "as"))
                                |> D.a (D.dullyellow (D.fromChars " point"))
                                |> D.a (D.fromChars ")")
                            , D.fromChars "so"
                            , D.fromChars "you"
                            , D.fromChars "can"
                            , D.fromChars "refer"
                            , D.fromChars "to"
                            , D.fromChars "individual"
                            , D.fromChars "parts"
                            , D.fromChars "of"
                            , D.fromChars "the"
                            , D.fromChars "tuple"
                            , D.fromChars "with"
                            , D.dullyellow <| D.fromChars "x"
                            , D.fromChars "and"
                            , D.dullyellow <| D.fromChars "y"
                            , D.fromChars "or"
                            , D.fromChars "you"
                            , D.fromChars "refer"
                            , D.fromChars "to"
                            , D.fromChars "the"
                            , D.fromChars "whole"
                            , D.fromChars "thing"
                            , D.fromChars "with"
                            , D.dullyellow (D.fromChars "point")
                                |> D.a (D.fromChars ".")
                            ]
                        , D.reflow <|
                            "So I was expecting to see a variable name after the `as` keyword here. Sometimes people just want to use `as` as a variable name though. Try using a different name in that case!"
                        ]
                    )

        T.CRES_PWildcardNotVar name width row col ->
            let
                region : T.CRA_Region
                region =
                    toWiderRegion row col width

                examples : List D.Doc
                examples =
                    case String.uncons (String.filter ((/=) '_') name) of
                        Nothing ->
                            [ D.dullyellow (D.fromChars "x"), D.fromChars "or", D.dullyellow (D.fromChars "age") ]

                        Just ( c, cs ) ->
                            [ D.dullyellow (D.fromChars (String.cons (Char.toLower c) cs)) ]
            in
            Report.Report "UNEXPECTED NAME" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "Variable names cannot start with underscores like this:"
                    , D.fillSep
                        ([ D.fromChars "You"
                         , D.fromChars "can"
                         , D.fromChars "either"
                         , D.fromChars "have"
                         , D.fromChars "an"
                         , D.fromChars "underscore"
                         , D.fromChars "like"
                         , D.dullyellow <| D.fromChars "_"
                         , D.fromChars "to"
                         , D.fromChars "ignore"
                         , D.fromChars "the"
                         , D.fromChars "value,"
                         , D.fromChars "or"
                         , D.fromChars "you"
                         , D.fromChars "can"
                         , D.fromChars "have"
                         , D.fromChars "a"
                         , D.fromChars "name"
                         , D.fromChars "like"
                         ]
                            ++ examples
                            ++ [ D.fromChars "to"
                               , D.fromChars "use"
                               , D.fromChars "the"
                               , D.fromChars "matched"
                               , D.fromChars "value."
                               ]
                        )
                    )

        T.CRES_PSpace space row col ->
            toSpaceReport source space row col

        T.CRES_PIndentStart row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PATTERN" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I wanted to parse a pattern next, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "am"
                            , D.fromChars "not"
                            , D.fromChars "sure"
                            , D.fromChars "why"
                            , D.fromChars "I"
                            , D.fromChars "am"
                            , D.fromChars "getting"
                            , D.fromChars "stuck"
                            , D.fromChars "exactly."
                            , D.fromChars "I"
                            , D.fromChars "just"
                            , D.fromChars "know"
                            , D.fromChars "that"
                            , D.fromChars "I"
                            , D.fromChars "want"
                            , D.fromChars "a"
                            , D.fromChars "pattern"
                            , D.fromChars "next."
                            , D.fromChars "Something"
                            , D.fromChars "as"
                            , D.fromChars "simple"
                            , D.fromChars "as"
                            , D.dullyellow <| D.fromChars "maybeHeight"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "result"
                            , D.fromChars "would"
                            , D.fromChars "work!"
                            ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation. If you think there is a pattern next, maybe it needs to be indented a bit more?"
                        ]
                    )

        T.CRES_PIndentAlias row col ->
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PATTERN" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was expecting to see a variable name after the `as` keyword:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "The"
                            , D.fromChars "`as`"
                            , D.fromChars "keyword"
                            , D.fromChars "lets"
                            , D.fromChars "you"
                            , D.fromChars "write"
                            , D.fromChars "patterns"
                            , D.fromChars "like"
                            , D.fromChars "(("
                                |> D.a (D.dullyellow (D.fromChars "x"))
                                |> D.a (D.fromChars ",")
                                |> D.a (D.dullyellow (D.fromChars "y"))
                                |> D.a (D.fromChars ") ")
                                |> D.a (D.cyan (D.fromChars "as"))
                                |> D.a (D.dullyellow (D.fromChars " point"))
                                |> D.a (D.fromChars ")")
                            , D.fromChars "so"
                            , D.fromChars "you"
                            , D.fromChars "can"
                            , D.fromChars "refer"
                            , D.fromChars "to"
                            , D.fromChars "individual"
                            , D.fromChars "parts"
                            , D.fromChars "of"
                            , D.fromChars "the"
                            , D.fromChars "tuple"
                            , D.fromChars "with"
                            , D.dullyellow <| D.fromChars "x"
                            , D.fromChars "and"
                            , D.dullyellow <| D.fromChars "y"
                            , D.fromChars "or"
                            , D.fromChars "you"
                            , D.fromChars "refer"
                            , D.fromChars "to"
                            , D.fromChars "the"
                            , D.fromChars "whole"
                            , D.fromChars "thing"
                            , D.fromChars "with"
                            , D.dullyellow <| D.fromChars "point."
                            ]
                        , D.reflow <|
                            "So I was expecting to see a variable name after the `as` keyword here. Sometimes people just want to use `as` as a variable name though. Try using a different name in that case!"
                        ]
                    )


toPRecordReport : Code.Source -> T.CRES_PRecord -> T.CPP_Row -> T.CPP_Col -> Report.Report
toPRecordReport source record startRow startCol =
    case record of
        T.CRES_PRecordOpen row col ->
            toUnfinishRecordPatternReport source row col startRow startCol <|
                D.reflow "I was expecting to see a field name next."

        T.CRES_PRecordEnd row col ->
            toUnfinishRecordPatternReport source row col startRow startCol <|
                D.fillSep
                    [ D.fromChars "I"
                    , D.fromChars "was"
                    , D.fromChars "expecting"
                    , D.fromChars "to"
                    , D.fromChars "see"
                    , D.fromChars "a"
                    , D.fromChars "closing"
                    , D.fromChars "curly"
                    , D.fromChars "brace"
                    , D.fromChars "next."
                    , D.fromChars "Try"
                    , D.fromChars "adding"
                    , D.fromChars "a"
                    , D.dullyellow <| D.fromChars "}"
                    , D.fromChars "here?"
                    ]

        T.CRES_PRecordField row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was not expecting to see `"
                                    ++ keyword
                                    ++ "` as a record field name:"
                            , D.reflow <|
                                "This is a reserved word, not available for variable names. Try another name!"
                            )

                _ ->
                    toUnfinishRecordPatternReport source row col startRow startCol <|
                        D.reflow "I was expecting to see a field name next."

        T.CRES_PRecordSpace space row col ->
            toSpaceReport source space row col

        T.CRES_PRecordIndentOpen row col ->
            toUnfinishRecordPatternReport source row col startRow startCol <|
                D.reflow "I was expecting to see a field name next."

        T.CRES_PRecordIndentEnd row col ->
            toUnfinishRecordPatternReport source row col startRow startCol <|
                D.fillSep
                    [ D.fromChars "I"
                    , D.fromChars "was"
                    , D.fromChars "expecting"
                    , D.fromChars "to"
                    , D.fromChars "see"
                    , D.fromChars "a"
                    , D.fromChars "closing"
                    , D.fromChars "curly"
                    , D.fromChars "brace"
                    , D.fromChars "next."
                    , D.fromChars "Try"
                    , D.fromChars "adding"
                    , D.fromChars "a"
                    , D.dullyellow <| D.fromChars "}"
                    , D.fromChars "here?"
                    ]

        T.CRES_PRecordIndentField row col ->
            toUnfinishRecordPatternReport source row col startRow startCol <|
                D.reflow "I was expecting to see a field name next."


toUnfinishRecordPatternReport : Code.Source -> T.CPP_Row -> T.CPP_Col -> T.CPP_Row -> T.CPP_Col -> D.Doc -> Report.Report
toUnfinishRecordPatternReport source row col startRow startCol message =
    let
        surroundings : T.CRA_Region
        surroundings =
            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

        region : T.CRA_Region
        region =
            toRegion row col
    in
    Report.Report "UNFINISHED RECORD PATTERN" region [] <|
        Code.toSnippet source surroundings (Just region) <|
            ( D.reflow "I was partway through parsing a record pattern, but I got stuck here:"
            , D.stack
                [ message
                , D.toFancyHint <|
                    [ D.fromChars "A"
                    , D.fromChars "record"
                    , D.fromChars "pattern"
                    , D.fromChars "looks"
                    , D.fromChars "like"
                    , D.dullyellow <| D.fromChars "{x,y}"
                    , D.fromChars "or"
                    , D.dullyellow <| D.fromChars "{name,age}"
                    , D.fromChars "where"
                    , D.fromChars "you"
                    , D.fromChars "list"
                    , D.fromChars "the"
                    , D.fromChars "field"
                    , D.fromChars "names"
                    , D.fromChars "you"
                    , D.fromChars "want"
                    , D.fromChars "to"
                    , D.fromChars "access."
                    ]
                ]
            )


toPTupleReport : Code.Source -> PContext -> T.CRES_PTuple -> T.CPP_Row -> T.CPP_Col -> Report.Report
toPTupleReport source context tuple startRow startCol =
    case tuple of
        T.CRES_PTupleOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "It looks like you are trying to use `"
                                    ++ keyword
                                    ++ "` as a variable name:"
                            , D.reflow <|
                                "This is a reserved word! Try using some other name?"
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED PARENTHESES" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I just saw an open parenthesis, but I got stuck here:"
                            , D.fillSep
                                [ D.fromChars "I"
                                , D.fromChars "was"
                                , D.fromChars "expecting"
                                , D.fromChars "to"
                                , D.fromChars "see"
                                , D.fromChars "a"
                                , D.fromChars "pattern"
                                , D.fromChars "next."
                                , D.fromChars "Maybe"
                                , D.fromChars "it"
                                , D.fromChars "will"
                                , D.fromChars "end"
                                , D.fromChars "up"
                                , D.fromChars "being"
                                , D.fromChars "something"
                                , D.fromChars "like"
                                , D.dullyellow <| D.fromChars "(x,y)"
                                , D.fromChars "or"
                                , D.dullyellow <| D.fromChars "(name, _)"
                                , D.fromChars "?"
                                ]
                            )

        T.CRES_PTupleEnd row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I ran into a reserved word in this pattern:"
                            , D.reflow <|
                                "The `"
                                    ++ keyword
                                    ++ "` keyword is reserved. Try using a different name instead!"
                            )

                Code.Operator op ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col op
                    in
                    Report.Report "UNEXPECTED SYMBOL" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I ran into the "
                                    ++ op
                                    ++ " symbol unexpectedly in this pattern:"
                            , D.reflow <|
                                "Only the :: symbol that works in patterns. It is useful if you are pattern matching on lists, trying to get the first element off the front. Did you want that instead?"
                            )

                Code.Close term bracket ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report ("STRAY " ++ String.toUpper term) region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I ran into a an unexpected "
                                    ++ term
                                    ++ " in this pattern:"
                            , D.reflow <|
                                "This "
                                    ++ String.fromChar bracket
                                    ++ " does not match up with an earlier open "
                                    ++ term
                                    ++ ". Try deleting it?"
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED PARENTHESES" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was partway through parsing a pattern, but I got stuck here:"
                            , D.fillSep
                                [ D.fromChars "I"
                                , D.fromChars "was"
                                , D.fromChars "expecting"
                                , D.fromChars "a"
                                , D.fromChars "closing"
                                , D.fromChars "parenthesis"
                                , D.fromChars "next,"
                                , D.fromChars "so"
                                , D.fromChars "try"
                                , D.fromChars "adding"
                                , D.fromChars "a"
                                , D.dullyellow <| D.fromChars ")"
                                , D.fromChars "to"
                                , D.fromChars "see"
                                , D.fromChars "if"
                                , D.fromChars "that"
                                , D.fromChars "helps?"
                                ]
                            )

        T.CRES_PTupleExpr pattern row col ->
            toPatternReport source context pattern row col

        T.CRES_PTupleSpace space row col ->
            toSpaceReport source space row col

        T.CRES_PTupleIndentEnd row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting a closing parenthesis next:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Try"
                            , D.fromChars "adding"
                            , D.fromChars "a"
                            , D.dullyellow <| D.fromChars ")"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "if"
                            , D.fromChars "that"
                            , D.fromChars "helps?"
                            ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation in cases like this, so maybe you have a closing parenthesis but it is not indented enough?"
                        ]
                    )

        T.CRES_PTupleIndentExpr1 row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw an open parenthesis, but then I got stuck here:"
                    , D.fillSep
                        [ D.fromChars "I"
                        , D.fromChars "was"
                        , D.fromChars "expecting"
                        , D.fromChars "to"
                        , D.fromChars "see"
                        , D.fromChars "a"
                        , D.fromChars "pattern"
                        , D.fromChars "next."
                        , D.fromChars "Maybe"
                        , D.fromChars "it"
                        , D.fromChars "will"
                        , D.fromChars "end"
                        , D.fromChars "up"
                        , D.fromChars "being"
                        , D.fromChars "something"
                        , D.fromChars "like"
                        , D.dullyellow <| D.fromChars "(x,y)"
                        , D.fromChars "or"
                        , D.dullyellow <| D.fromChars "(name, _)"
                        , D.fromChars "?"
                        ]
                    )

        T.CRES_PTupleIndentExprN row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED TUPLE PATTERN" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a tuple pattern, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "a"
                            , D.fromChars "pattern"
                            , D.fromChars "next."
                            , D.fromChars "I"
                            , D.fromChars "am"
                            , D.fromChars "expecting"
                            , D.fromChars "the"
                            , D.fromChars "final"
                            , D.fromChars "result"
                            , D.fromChars "to"
                            , D.fromChars "be"
                            , D.fromChars "something"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "(x,y)"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "(name, _)"
                            , D.fromChars "."
                            ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation in cases like this, so the problem may be that the next part is not indented enough?"
                        ]
                    )


toPListReport : Code.Source -> PContext -> T.CRES_PList -> T.CPP_Row -> T.CPP_Col -> Report.Report
toPListReport source context list startRow startCol =
    case list of
        T.CRES_PListOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow ("It looks like you are trying to use `" ++ keyword ++ "` to name an element of a list:")
                            , D.reflow "This is a reserved word though! Try using some other name?"
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED LIST PATTERN" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I just saw an open square bracket, but then I got stuck here:"
                            , D.fillSep
                                [ D.fromChars "Try"
                                , D.fromChars "adding"
                                , D.fromChars "a"
                                , D.dullyellow <| D.fromChars "]"
                                , D.fromChars "to"
                                , D.fromChars "see"
                                , D.fromChars "if"
                                , D.fromChars "that"
                                , D.fromChars "helps?"
                                ]
                            )

        T.CRES_PListEnd row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST PATTERN" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was expecting a closing square bracket to end this list pattern:"
                    , D.fillSep
                        [ D.fromChars "Try"
                        , D.fromChars "adding"
                        , D.fromChars "a"
                        , D.dullyellow <| D.fromChars "]"
                        , D.fromChars "to"
                        , D.fromChars "see"
                        , D.fromChars "if"
                        , D.fromChars "that"
                        , D.fromChars "helps?"
                        ]
                    )

        T.CRES_PListExpr pattern row col ->
            toPatternReport source context pattern row col

        T.CRES_PListSpace space row col ->
            toSpaceReport source space row col

        T.CRES_PListIndentOpen row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST PATTERN" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I just saw an open square bracket, but then I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Try"
                            , D.fromChars "adding"
                            , D.fromChars "a"
                            , D.dullyellow <| D.fromChars "]"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "if"
                            , D.fromChars "that"
                            , D.fromChars "helps?"
                            ]
                        , D.toSimpleNote "I can get confused by indentation in cases like this, so maybe there is something next, but it is not indented enough?"
                        ]
                    )

        T.CRES_PListIndentEnd row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST PATTERN" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was expecting a closing square bracket to end this list pattern:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Try"
                            , D.fromChars "adding"
                            , D.fromChars "a"
                            , D.dullyellow <| D.fromChars "]"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "if"
                            , D.fromChars "that"
                            , D.fromChars "helps?"
                            ]
                        , D.toSimpleNote "I can get confused by indentation in cases like this, so maybe you have a closing square bracket but it is not indented enough?"
                        ]
                    )

        T.CRES_PListIndentExpr row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST PATTERN" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a list pattern, but I got stuck here:"
                    , D.stack
                        [ D.reflow "I was expecting to see another pattern next. Maybe a variable name."
                        , D.toSimpleNote "I can get confused by indentation in cases like this, so maybe there is more to this pattern but it is not indented enough?"
                        ]
                    )



-- TYPES


type TContext
    = TC_Annotation T.CDN_Name
    | TC_CustomType
    | TC_TypeAlias
    | TC_Port


toTypeReport : Code.Source -> TContext -> T.CRES_Type -> T.CPP_Row -> T.CPP_Col -> Report.Report
toTypeReport source context tipe startRow startCol =
    case tipe of
        T.CRES_TRecord record row col ->
            toTRecordReport source context record row col

        T.CRES_TTuple tuple row col ->
            toTTupleReport source context tuple row col

        T.CRES_TStart row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I was expecting to see a type next, but I got stuck on this reserved word:"
                            , D.reflow ("It looks like you are trying to use `" ++ keyword ++ "` as a type variable, but it is a reserved word. Try using a different name!")
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col

                        thing : String
                        thing =
                            case context of
                                TC_Annotation _ ->
                                    "type annotation"

                                TC_CustomType ->
                                    "custom type"

                                TC_TypeAlias ->
                                    "type alias"

                                TC_Port ->
                                    "port"

                        something : String
                        something =
                            case context of
                                TC_Annotation name ->
                                    "the `" ++ name ++ "` type annotation"

                                TC_CustomType ->
                                    "a custom type"

                                TC_TypeAlias ->
                                    "a type alias"

                                TC_Port ->
                                    "a port"
                    in
                    Report.Report ("PROBLEM IN " ++ String.toUpper thing) region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow ("I was partway through parsing " ++ something ++ ", but I got stuck here:")
                            , D.fillSep
                                [ D.fromChars "I"
                                , D.fromChars "was"
                                , D.fromChars "expecting"
                                , D.fromChars "to"
                                , D.fromChars "see"
                                , D.fromChars "a"
                                , D.fromChars "type"
                                , D.fromChars "next."
                                , D.fromChars "Try"
                                , D.fromChars "putting"
                                , D.dullyellow <| D.fromChars "Int"
                                , D.fromChars "or"
                                , D.dullyellow <| D.fromChars "String"
                                , D.fromChars "for"
                                , D.fromChars "now?"
                                ]
                            )

        T.CRES_TSpace space row col ->
            toSpaceReport source space row col

        T.CRES_TIndentStart row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col

                thing : String
                thing =
                    case context of
                        TC_Annotation _ ->
                            "type annotation"

                        TC_CustomType ->
                            "custom type"

                        TC_TypeAlias ->
                            "type alias"

                        TC_Port ->
                            "port"
            in
            Report.Report ("UNFINISHED " ++ String.toUpper thing) region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow ("I was partway through parsing a " ++ thing ++ ", but I got stuck here:")
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "a"
                            , D.fromChars "type"
                            , D.fromChars "next."
                            , D.fromChars "Try"
                            , D.fromChars "putting"
                            , D.dullyellow <| D.fromChars "Int"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "String"
                            , D.fromChars "for"
                            , D.fromChars "now?"
                            ]
                        , D.toSimpleNote "I can get confused by indentation. If you think there is already a type next, maybe it is not indented enough?"
                        ]
                    )


toTRecordReport : Code.Source -> TContext -> T.CRES_TRecord -> T.CPP_Row -> T.CPP_Col -> Report.Report
toTRecordReport source context record startRow startCol =
    case record of
        T.CRES_TRecordOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I just started parsing a record type, but I got stuck on this field name:"
                            , D.reflow ("It looks like you are trying to use `" ++ keyword ++ "` as a field name, but that is a reserved word. Try using a different name!")
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED RECORD TYPE" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I just started parsing a record type, but I got stuck here:"
                            , D.fillSep
                                [ D.fromChars "Record"
                                , D.fromChars "types"
                                , D.fromChars "look"
                                , D.fromChars "like"
                                , D.dullyellow <| D.fromChars "{ name : String, age : Int },"
                                , D.fromChars "so"
                                , D.fromChars "I"
                                , D.fromChars "was"
                                , D.fromChars "expecting"
                                , D.fromChars "to"
                                , D.fromChars "see"
                                , D.fromChars "a"
                                , D.fromChars "field"
                                , D.fromChars "name"
                                , D.fromChars "next."
                                ]
                            )

        T.CRES_TRecordEnd row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD TYPE" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a record type, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "a"
                            , D.fromChars "closing"
                            , D.fromChars "curly"
                            , D.fromChars "brace"
                            , D.fromChars "before"
                            , D.fromChars "this,"
                            , D.fromChars "so"
                            , D.fromChars "try"
                            , D.fromChars "adding"
                            , D.fromChars "a"
                            , D.dullyellow <| D.fromChars "}"
                            , D.fromChars "and"
                            , D.fromChars "see"
                            , D.fromChars "if"
                            , D.fromChars "that"
                            , D.fromChars "helps?"
                            ]
                        , D.toSimpleNote "When I get stuck like this, it usually means that there is a missing parenthesis or bracket somewhere earlier. It could also be a stray keyword or operator."
                        ]
                    )

        T.CRES_TRecordField row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I am partway through parsing a record type, but I got stuck on this field name:"
                            , D.reflow ("It looks like you are trying to use `" ++ keyword ++ "` as a field name, but that is a reserved word. Try using a different name!")
                            )

                Code.Other (Just ',') ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "EXTRA COMMA" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I am partway through parsing a record type, but I got stuck here:"
                            , D.stack
                                [ D.reflow "I am seeing two commas in a row. This is the second one!"
                                , D.reflow "Just delete one of the commas and you should be all set!"
                                , noteForRecordTypeError
                                ]
                            )

                Code.Close _ '}' ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "EXTRA COMMA" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I am partway through parsing a record type, but I got stuck here:"
                            , D.stack
                                [ D.reflow "Trailing commas are not allowed in record types. Try deleting the comma that appears before this closing curly brace."
                                , noteForRecordTypeError
                                ]
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN RECORD TYPE" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I am partway through parsing a record type, but I got stuck here:"
                            , D.stack
                                [ D.fillSep
                                    [ D.fromChars "I"
                                    , D.fromChars "was"
                                    , D.fromChars "expecting"
                                    , D.fromChars "to"
                                    , D.fromChars "see"
                                    , D.fromChars "another"
                                    , D.fromChars "record"
                                    , D.fromChars "field"
                                    , D.fromChars "defined"
                                    , D.fromChars "next,"
                                    , D.fromChars "so"
                                    , D.fromChars "I"
                                    , D.fromChars "am"
                                    , D.fromChars "looking"
                                    , D.fromChars "for"
                                    , D.fromChars "a"
                                    , D.fromChars "name"
                                    , D.fromChars "like"
                                    , D.dullyellow <| D.fromChars "userName"
                                    , D.fromChars "or"
                                    , D.dullyellow (D.fromChars "plantHeight")
                                        |> D.a (D.fromChars ".")
                                    ]
                                , noteForRecordTypeError
                                ]
                            )

        T.CRES_TRecordColon row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD TYPE" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a record type, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "just"
                            , D.fromChars "saw"
                            , D.fromChars "a"
                            , D.fromChars "field"
                            , D.fromChars "name,"
                            , D.fromChars "so"
                            , D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "a"
                            , D.fromChars "colon"
                            , D.fromChars "next."
                            , D.fromChars "So"
                            , D.fromChars "try"
                            , D.fromChars "putting"
                            , D.fromChars "an"
                            , D.green <| D.fromChars ":"
                            , D.fromChars "sign"
                            , D.fromChars "here?"
                            ]
                        , noteForRecordTypeError
                        ]
                    )

        T.CRES_TRecordType tipe row col ->
            toTypeReport source context tipe row col

        T.CRES_TRecordSpace space row col ->
            toSpaceReport source space row col

        T.CRES_TRecordIndentOpen row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD TYPE" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I just saw the opening curly brace of a record type, but then I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "I"
                            , D.fromChars "am"
                            , D.fromChars "expecting"
                            , D.fromChars "a"
                            , D.fromChars "record"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "{ name : String, age : Int }"
                            , D.fromChars "here."
                            , D.fromChars "Try"
                            , D.fromChars "defining"
                            , D.fromChars "some"
                            , D.fromChars "fields"
                            , D.fromChars "of"
                            , D.fromChars "your"
                            , D.fromChars "own?"
                            ]
                        , noteForRecordTypeIndentError
                        ]
                    )

        T.CRES_TRecordIndentEnd row col ->
            case Code.nextLineStartsWithCloseCurly source row of
                Just ( curlyRow, curlyCol ) ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position curlyRow curlyCol)

                        region : T.CRA_Region
                        region =
                            toRegion curlyRow curlyCol
                    in
                    Report.Report "NEED MORE INDENTATION" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I was partway through parsing a record type, but I got stuck here:"
                            , D.stack
                                [ D.reflow "I need this curly brace to be indented more. Try adding some spaces before it!"
                                , noteForRecordTypeError
                                ]
                            )

                Nothing ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED RECORD TYPE" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow "I was partway through parsing a record type, but I got stuck here:"
                            , D.stack
                                [ D.fillSep
                                    [ D.fromChars "I"
                                    , D.fromChars "was"
                                    , D.fromChars "expecting"
                                    , D.fromChars "to"
                                    , D.fromChars "see"
                                    , D.fromChars "a"
                                    , D.fromChars "closing"
                                    , D.fromChars "curly"
                                    , D.fromChars "brace"
                                    , D.fromChars "next."
                                    , D.fromChars "Try"
                                    , D.fromChars "putting"
                                    , D.fromChars "a"
                                    , D.green <| D.fromChars "}"
                                    , D.fromChars "next"
                                    , D.fromChars "and"
                                    , D.fromChars "see"
                                    , D.fromChars "if"
                                    , D.fromChars "that"
                                    , D.fromChars "helps?"
                                    ]
                                , noteForRecordTypeIndentError
                                ]
                            )

        T.CRES_TRecordIndentField row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD TYPE" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a record type, but I got stuck after that last comma:"
                    , D.stack
                        [ D.reflow "Trailing commas are not allowed in record types, so the fix may be to delete that last comma? Or maybe you were in the middle of defining an additional field?"
                        , noteForRecordTypeIndentError
                        ]
                    )

        T.CRES_TRecordIndentColon row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD TYPE" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a record type. I just saw a record field, so I was expecting to see a colon next:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Try"
                            , D.fromChars "putting"
                            , D.fromChars "an"
                            , D.green <| D.fromChars ":"
                            , D.fromChars "followed"
                            , D.fromChars "by"
                            , D.fromChars "a"
                            , D.fromChars "type?"
                            ]
                        , noteForRecordTypeIndentError
                        ]
                    )

        T.CRES_TRecordIndentType row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD TYPE" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I am partway through parsing a record type, and I was expecting to run into a type next:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Try"
                            , D.fromChars "putting"
                            , D.fromChars "something"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "Int"
                            , D.fromChars "or"
                            , D.dullyellow <| D.fromChars "String"
                            , D.fromChars "for"
                            , D.fromChars "now?"
                            ]
                        , noteForRecordTypeIndentError
                        ]
                    )


noteForRecordTypeError : D.Doc
noteForRecordTypeError =
    D.stack
        [ D.toSimpleNote
            "If you are trying to define a record type across multiple lines, I recommend using this format:"
        , D.indent 4 <|
            D.vcat
                [ D.fromChars "{ name : String"
                , D.fromChars ", age : Int"
                , D.fromChars ", height : Float"
                , D.fromChars "}"
                ]
        , D.reflow
            "Notice that each line starts with some indentation. Usually two or four spaces. This is the stylistic convention in the Elm ecosystem."
        ]


noteForRecordTypeIndentError : D.Doc
noteForRecordTypeIndentError =
    D.stack
        [ D.toSimpleNote
            "I may be confused by indentation. For example, if you are trying to define a record type across multiple lines, I recommend using this format:"
        , D.indent 4 <|
            D.vcat
                [ D.fromChars "{ name : String"
                , D.fromChars ", age : Int"
                , D.fromChars ", height : Float"
                , D.fromChars "}"
                ]
        , D.reflow
            "Notice that each line starts with some indentation. Usually two or four spaces. This is the stylistic convention in the Elm ecosystem."
        ]


toTTupleReport : Code.Source -> TContext -> T.CRES_TTuple -> T.CPP_Row -> T.CPP_Col -> Report.Report
toTTupleReport source context tuple startRow startCol =
    case tuple of
        T.CRES_TTupleOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow
                                "I ran into a reserved word unexpectedly:"
                            , D.reflow
                                ("It looks like you are trying to use `" ++ keyword ++ "` as a variable name, but  it is a reserved word. Try using a different name!")
                            )

                _ ->
                    let
                        surroundings : T.CRA_Region
                        surroundings =
                            T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                        region : T.CRA_Region
                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED PARENTHESES" region [] <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow
                                "I just saw an open parenthesis, so I was expecting to see a type next."
                            , D.fillSep
                                [ D.fromChars "Something"
                                , D.fromChars "like"
                                , D.dullyellow <| D.fromChars "(Maybe Int)"
                                , D.fromChars "or"
                                , D.dullyellow (D.fromChars "(List Person)") |> D.a (D.fromChars ".")
                                , D.fromChars "Anything"
                                , D.fromChars "where"
                                , D.fromChars "you"
                                , D.fromChars "are"
                                , D.fromChars "putting"
                                , D.fromChars "parentheses"
                                , D.fromChars "around"
                                , D.fromChars "normal"
                                , D.fromChars "types."
                                ]
                            )

        T.CRES_TTupleEnd row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow
                        "I was expecting to see a closing parenthesis next, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Try"
                            , D.fromChars "adding"
                            , D.fromChars "a"
                            , D.dullyellow <| D.fromChars ")"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "if"
                            , D.fromChars "that"
                            , D.fromChars "helps?"
                            ]
                        , D.toSimpleNote
                            "I can get stuck when I run into keywords, operators, parentheses, or brackets unexpectedly. So there may be some earlier syntax trouble (like extra parenthesis or missing brackets) that is confusing me."
                        ]
                    )

        T.CRES_TTupleType tipe row col ->
            toTypeReport source context tipe row col

        T.CRES_TTupleSpace space row col ->
            toSpaceReport source space row col

        T.CRES_TTupleIndentType1 row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow
                        "I just saw an open parenthesis, so I was expecting to see a type next."
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Something"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "(Maybe Int)"
                            , D.fromChars "or"
                            , D.dullyellow (D.fromChars "(List Person)") |> D.a (D.fromChars ".")
                            , D.fromChars "Anything"
                            , D.fromChars "where"
                            , D.fromChars "you"
                            , D.fromChars "are"
                            , D.fromChars "putting"
                            , D.fromChars "parentheses"
                            , D.fromChars "around"
                            , D.fromChars "normal"
                            , D.fromChars "types."
                            ]
                        , D.toSimpleNote
                            "I can get confused by indentation in cases like this, so maybe you have a type but it is not indented enough?"
                        ]
                    )

        T.CRES_TTupleIndentTypeN row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED TUPLE TYPE" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow
                        "I think I am in the middle of parsing a tuple type. I just saw a comma, so I was expecting to see a type next."
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "A"
                            , D.fromChars "tuple"
                            , D.fromChars "type"
                            , D.fromChars "looks"
                            , D.fromChars "like"
                            , D.dullyellow <| D.fromChars "(Float,Float)"
                            , D.fromChars "or"
                            , D.dullyellow (D.fromChars "(String,Int)") |> D.a (D.fromChars ",")
                            , D.fromChars "so"
                            , D.fromChars "I"
                            , D.fromChars "think"
                            , D.fromChars "there"
                            , D.fromChars "is"
                            , D.fromChars "a"
                            , D.fromChars "type"
                            , D.fromChars "missing"
                            , D.fromChars "here?"
                            ]
                        , D.toSimpleNote
                            "I can get confused by indentation in cases like this, so maybe you have an expression but it is not indented enough?"
                        ]
                    )

        T.CRES_TTupleIndentEnd row col ->
            let
                surroundings : T.CRA_Region
                surroundings =
                    T.CRA_Region (T.CRA_Position startRow startCol) (T.CRA_Position row col)

                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow
                        "I was expecting to see a closing parenthesis next:"
                    , D.stack
                        [ D.fillSep
                            [ D.fromChars "Try"
                            , D.fromChars "adding"
                            , D.fromChars "a"
                            , D.dullyellow <| D.fromChars ")"
                            , D.fromChars "to"
                            , D.fromChars "see"
                            , D.fromChars "if"
                            , D.fromChars "that"
                            , D.fromChars "helps!"
                            ]
                        , D.toSimpleNote
                            "I can get confused by indentation in cases like this, so maybe you have a closing parenthesis but it is not indented enough?"
                        ]
                    )



-- ENCODERS and DECODERS


spaceEncoder : T.CRES_Space -> Encode.Value
spaceEncoder space =
    case space of
        T.CRES_HasTab ->
            Encode.string "HasTab"

        T.CRES_EndlessMultiComment ->
            Encode.string "EndlessMultiComment"


spaceDecoder : Decode.Decoder T.CRES_Space
spaceDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "HasTab" ->
                        Decode.succeed T.CRES_HasTab

                    "EndlessMultiComment" ->
                        Decode.succeed T.CRES_EndlessMultiComment

                    _ ->
                        Decode.fail ("Unknown Space: " ++ str)
            )


typeEncoder : T.CRES_Type -> Encode.Value
typeEncoder type_ =
    case type_ of
        T.CRES_TRecord record row col ->
            Encode.object
                [ ( "type", Encode.string "TRecord" )
                , ( "record", tRecordEncoder record )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TTuple tuple row col ->
            Encode.object
                [ ( "type", Encode.string "TTuple" )
                , ( "tuple", tTupleEncoder tuple )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TStart row col ->
            Encode.object
                [ ( "type", Encode.string "TStart" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "TSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TIndentStart row col ->
            Encode.object
                [ ( "type", Encode.string "TIndentStart" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


typeDecoder : Decode.Decoder T.CRES_Type
typeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "TRecord" ->
                        Decode.map3 T.CRES_TRecord
                            (Decode.field "record" tRecordDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TTuple" ->
                        Decode.map3 T.CRES_TTuple
                            (Decode.field "tuple" tTupleDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TStart" ->
                        Decode.map2 T.CRES_TStart
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TSpace" ->
                        Decode.map3 T.CRES_TSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TIndentStart" ->
                        Decode.map2 T.CRES_TIndentStart
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Type's type: " ++ type_)
            )


patternEncoder : T.CRES_Pattern -> Encode.Value
patternEncoder pattern =
    case pattern of
        T.CRES_PRecord record row col ->
            Encode.object
                [ ( "type", Encode.string "PRecord" )
                , ( "record", pRecordEncoder record )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PTuple tuple row col ->
            Encode.object
                [ ( "type", Encode.string "PTuple" )
                , ( "tuple", pTupleEncoder tuple )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PList list row col ->
            Encode.object
                [ ( "type", Encode.string "PList" )
                , ( "list", pListEncoder list )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PStart row col ->
            Encode.object
                [ ( "type", Encode.string "PStart" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PChar char row col ->
            Encode.object
                [ ( "type", Encode.string "PChar" )
                , ( "char", charEncoder char )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PString string row col ->
            Encode.object
                [ ( "type", Encode.string "PString" )
                , ( "string", stringEncoder string )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PNumber number row col ->
            Encode.object
                [ ( "type", Encode.string "PNumber" )
                , ( "number", numberEncoder number )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PFloat width row col ->
            Encode.object
                [ ( "type", Encode.string "PFloat" )
                , ( "width", Encode.int width )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PAlias row col ->
            Encode.object
                [ ( "type", Encode.string "PAlias" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PWildcardNotVar name width row col ->
            Encode.object
                [ ( "type", Encode.string "PWildcardNotVar" )
                , ( "name", Encode.string name )
                , ( "width", Encode.int width )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "PSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PIndentStart row col ->
            Encode.object
                [ ( "type", Encode.string "PIndentStart" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PIndentAlias row col ->
            Encode.object
                [ ( "type", Encode.string "PIndentAlias" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


patternDecoder : Decode.Decoder T.CRES_Pattern
patternDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "PRecord" ->
                        Decode.map3 T.CRES_PRecord
                            (Decode.field "record" pRecordDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PTuple" ->
                        Decode.map3 T.CRES_PTuple
                            (Decode.field "tuple" pTupleDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PList" ->
                        Decode.map3 T.CRES_PList
                            (Decode.field "list" pListDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PStart" ->
                        Decode.map2 T.CRES_PStart
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PChar" ->
                        Decode.map3 T.CRES_PChar
                            (Decode.field "char" charDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PString" ->
                        Decode.map3 T.CRES_PString
                            (Decode.field "string" stringDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PNumber" ->
                        Decode.map3 T.CRES_PNumber
                            (Decode.field "number" numberDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PFloat" ->
                        Decode.map3 T.CRES_PFloat
                            (Decode.field "width" Decode.int)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PAlias" ->
                        Decode.map2 T.CRES_PAlias
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PWildcardNotVar" ->
                        Decode.map4 T.CRES_PWildcardNotVar
                            (Decode.field "name" Decode.string)
                            (Decode.field "width" Decode.int)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PSpace" ->
                        Decode.map3 T.CRES_PSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PIndentStart" ->
                        Decode.map2 T.CRES_PIndentStart
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PIndentAlias" ->
                        Decode.map2 T.CRES_PIndentAlias
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Pattern's type: " ++ type_)
            )


exprEncoder : T.CRES_Expr -> Encode.Value
exprEncoder expr =
    case expr of
        T.CRES_Let let_ row col ->
            Encode.object
                [ ( "type", Encode.string "Let" )
                , ( "let", letEncoder let_ )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_Case case_ row col ->
            Encode.object
                [ ( "type", Encode.string "Case" )
                , ( "case", caseEncoder case_ )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_If if_ row col ->
            Encode.object
                [ ( "type", Encode.string "If" )
                , ( "if", ifEncoder if_ )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_List list row col ->
            Encode.object
                [ ( "type", Encode.string "List" )
                , ( "list", listEncoder list )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_Record record row col ->
            Encode.object
                [ ( "type", Encode.string "Record" )
                , ( "record", recordEncoder record )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_Tuple tuple row col ->
            Encode.object
                [ ( "type", Encode.string "Tuple" )
                , ( "tuple", tupleEncoder tuple )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_Func func row col ->
            Encode.object
                [ ( "type", Encode.string "Func" )
                , ( "func", funcEncoder func )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_Dot row col ->
            Encode.object
                [ ( "type", Encode.string "Dot" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_Access row col ->
            Encode.object
                [ ( "type", Encode.string "Access" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_OperatorRight op row col ->
            Encode.object
                [ ( "type", Encode.string "OperatorRight" )
                , ( "op", Encode.string op )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_OperatorReserved operator row col ->
            Encode.object
                [ ( "type", Encode.string "OperatorReserved" )
                , ( "operator", Compiler.Parse.Symbol.badOperatorEncoder operator )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_Start row col ->
            Encode.object
                [ ( "type", Encode.string "Start" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_Char char row col ->
            Encode.object
                [ ( "type", Encode.string "Char" )
                , ( "char", charEncoder char )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_String_ string row col ->
            Encode.object
                [ ( "type", Encode.string "String" )
                , ( "string", stringEncoder string )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_Number number row col ->
            Encode.object
                [ ( "type", Encode.string "Number" )
                , ( "number", numberEncoder number )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_Space space row col ->
            Encode.object
                [ ( "type", Encode.string "Space" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_EndlessShader row col ->
            Encode.object
                [ ( "type", Encode.string "EndlessShader" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_ShaderProblem problem row col ->
            Encode.object
                [ ( "type", Encode.string "ShaderProblem" )
                , ( "problem", Encode.string problem )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_IndentOperatorRight op row col ->
            Encode.object
                [ ( "type", Encode.string "IndentOperatorRight" )
                , ( "op", Encode.string op )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


exprDecoder : Decode.Decoder T.CRES_Expr
exprDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Let" ->
                        Decode.map3 T.CRES_Let
                            (Decode.field "let" letDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "Case" ->
                        Decode.map3 T.CRES_Case
                            (Decode.field "case" caseDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "If" ->
                        Decode.map3 T.CRES_If
                            (Decode.field "if" ifDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "List" ->
                        Decode.map3 T.CRES_List
                            (Decode.field "list" listDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "Record" ->
                        Decode.map3 T.CRES_Record
                            (Decode.field "record" recordDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "Tuple" ->
                        Decode.map3 T.CRES_Tuple
                            (Decode.field "tuple" tupleDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "Func" ->
                        Decode.map3 T.CRES_Func
                            (Decode.field "func" funcDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "Dot" ->
                        Decode.map2 T.CRES_Dot
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "Access" ->
                        Decode.map2 T.CRES_Access
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "OperatorRight" ->
                        Decode.map3 T.CRES_OperatorRight
                            (Decode.field "op" Decode.string)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "OperatorReserved" ->
                        Decode.map3 T.CRES_OperatorReserved
                            (Decode.field "operator" Compiler.Parse.Symbol.badOperatorDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "Start" ->
                        Decode.map2 T.CRES_Start
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "Char" ->
                        Decode.map3 T.CRES_Char
                            (Decode.field "char" charDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "String" ->
                        Decode.map3 T.CRES_String_
                            (Decode.field "string" stringDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "Number" ->
                        Decode.map3 T.CRES_Number
                            (Decode.field "number" numberDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "Space" ->
                        Decode.map3 T.CRES_Space
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "EndlessShader" ->
                        Decode.map2 T.CRES_EndlessShader
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "ShaderProblem" ->
                        Decode.map3 T.CRES_ShaderProblem
                            (Decode.field "problem" Decode.string)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "IndentOperatorRight" ->
                        Decode.map3 T.CRES_IndentOperatorRight
                            (Decode.field "op" Decode.string)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Expr's type: " ++ type_)
            )


letEncoder : T.CRES_Let -> Encode.Value
letEncoder let_ =
    case let_ of
        T.CRES_LetSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "LetSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_LetIn row col ->
            Encode.object
                [ ( "type", Encode.string "LetIn" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_LetDefAlignment int row col ->
            Encode.object
                [ ( "type", Encode.string "LetDefAlignment" )
                , ( "int", Encode.int int )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_LetDefName row col ->
            Encode.object
                [ ( "type", Encode.string "LetDefName" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_LetDef name def row col ->
            Encode.object
                [ ( "type", Encode.string "LetDef" )
                , ( "name", Encode.string name )
                , ( "def", defEncoder def )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_LetDestruct destruct row col ->
            Encode.object
                [ ( "type", Encode.string "LetDestruct" )
                , ( "destruct", destructEncoder destruct )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_LetBody expr row col ->
            Encode.object
                [ ( "type", Encode.string "LetBody" )
                , ( "expr", exprEncoder expr )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_LetIndentDef row col ->
            Encode.object
                [ ( "type", Encode.string "LetIndentDef" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_LetIndentIn row col ->
            Encode.object
                [ ( "type", Encode.string "LetIndentIn" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_LetIndentBody row col ->
            Encode.object
                [ ( "type", Encode.string "LetIndentBody" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


letDecoder : Decode.Decoder T.CRES_Let
letDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "LetSpace" ->
                        Decode.map3 T.CRES_LetSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "LetIn" ->
                        Decode.map2 T.CRES_LetIn
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "LetDefAlignment" ->
                        Decode.map3 T.CRES_LetDefAlignment
                            (Decode.field "int" Decode.int)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "LetDefName" ->
                        Decode.map2 T.CRES_LetDefName
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "LetDef" ->
                        Decode.map4 T.CRES_LetDef
                            (Decode.field "name" Decode.string)
                            (Decode.field "def" defDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "LetDestruct" ->
                        Decode.map3 T.CRES_LetDestruct
                            (Decode.field "destruct" destructDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "LetBody" ->
                        Decode.map3 T.CRES_LetBody
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "LetIndentDef" ->
                        Decode.map2 T.CRES_LetIndentDef
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "LetIndentIn" ->
                        Decode.map2 T.CRES_LetIndentIn
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "LetIndentBody" ->
                        Decode.map2 T.CRES_LetIndentBody
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Let's type: " ++ type_)
            )


caseEncoder : T.CRES_Case -> Encode.Value
caseEncoder case_ =
    case case_ of
        T.CRES_CaseSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "CaseSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_CaseOf row col ->
            Encode.object
                [ ( "type", Encode.string "CaseOf" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_CasePattern pattern row col ->
            Encode.object
                [ ( "type", Encode.string "CasePattern" )
                , ( "pattern", patternEncoder pattern )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_CaseArrow row col ->
            Encode.object
                [ ( "type", Encode.string "CaseArrow" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_CaseExpr expr row col ->
            Encode.object
                [ ( "type", Encode.string "CaseExpr" )
                , ( "expr", exprEncoder expr )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_CaseBranch expr row col ->
            Encode.object
                [ ( "type", Encode.string "CaseBranch" )
                , ( "expr", exprEncoder expr )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_CaseIndentOf row col ->
            Encode.object
                [ ( "type", Encode.string "CaseIndentOf" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_CaseIndentExpr row col ->
            Encode.object
                [ ( "type", Encode.string "CaseIndentExpr" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_CaseIndentPattern row col ->
            Encode.object
                [ ( "type", Encode.string "CaseIndentPattern" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_CaseIndentArrow row col ->
            Encode.object
                [ ( "type", Encode.string "CaseIndentArrow" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_CaseIndentBranch row col ->
            Encode.object
                [ ( "type", Encode.string "CaseIndentBranch" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_CasePatternAlignment indent row col ->
            Encode.object
                [ ( "type", Encode.string "CasePatternAlignment" )
                , ( "indent", Encode.int indent )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


caseDecoder : Decode.Decoder T.CRES_Case
caseDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "CaseSpace" ->
                        Decode.map3 T.CRES_CaseSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "CaseOf" ->
                        Decode.map2 T.CRES_CaseOf
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "CasePattern" ->
                        Decode.map3 T.CRES_CasePattern
                            (Decode.field "pattern" patternDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "CaseArrow" ->
                        Decode.map2 T.CRES_CaseArrow
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "CaseExpr" ->
                        Decode.map3 T.CRES_CaseExpr
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "CaseBranch" ->
                        Decode.map3 T.CRES_CaseBranch
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "CaseIndentOf" ->
                        Decode.map2 T.CRES_CaseIndentOf
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "CaseIndentExpr" ->
                        Decode.map2 T.CRES_CaseIndentExpr
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "CaseIndentPattern" ->
                        Decode.map2 T.CRES_CaseIndentPattern
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "CaseIndentArrow" ->
                        Decode.map2 T.CRES_CaseIndentArrow
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "CaseIndentBranch" ->
                        Decode.map2 T.CRES_CaseIndentBranch
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "CasePatternAlignment" ->
                        Decode.map3 T.CRES_CasePatternAlignment
                            (Decode.field "indent" Decode.int)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Case's type: " ++ type_)
            )


ifEncoder : T.CRES_If -> Encode.Value
ifEncoder if_ =
    case if_ of
        T.CRES_IfSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "IfSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_IfThen row col ->
            Encode.object
                [ ( "type", Encode.string "IfThen" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_IfElse row col ->
            Encode.object
                [ ( "type", Encode.string "IfElse" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_IfElseBranchStart row col ->
            Encode.object
                [ ( "type", Encode.string "IfElseBranchStart" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_IfCondition expr row col ->
            Encode.object
                [ ( "type", Encode.string "IfCondition" )
                , ( "expr", exprEncoder expr )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_IfThenBranch expr row col ->
            Encode.object
                [ ( "type", Encode.string "IfThenBranch" )
                , ( "expr", exprEncoder expr )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_IfElseBranch expr row col ->
            Encode.object
                [ ( "type", Encode.string "IfElseBranch" )
                , ( "expr", exprEncoder expr )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_IfIndentCondition row col ->
            Encode.object
                [ ( "type", Encode.string "IfIndentCondition" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_IfIndentThen row col ->
            Encode.object
                [ ( "type", Encode.string "IfIndentThen" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_IfIndentThenBranch row col ->
            Encode.object
                [ ( "type", Encode.string "IfIndentThenBranch" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_IfIndentElseBranch row col ->
            Encode.object
                [ ( "type", Encode.string "IfIndentElseBranch" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_IfIndentElse row col ->
            Encode.object
                [ ( "type", Encode.string "IfIndentElse" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


ifDecoder : Decode.Decoder T.CRES_If
ifDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "IfSpace" ->
                        Decode.map3 T.CRES_IfSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "IfThen" ->
                        Decode.map2 T.CRES_IfThen
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "IfElse" ->
                        Decode.map2 T.CRES_IfElse
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "IfElseBranchStart" ->
                        Decode.map2 T.CRES_IfElseBranchStart
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "IfCondition" ->
                        Decode.map3 T.CRES_IfCondition
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "IfThenBranch" ->
                        Decode.map3 T.CRES_IfThenBranch
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "IfElseBranch" ->
                        Decode.map3 T.CRES_IfElseBranch
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "IfIndentCondition" ->
                        Decode.map2 T.CRES_IfIndentCondition
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "IfIndentThen" ->
                        Decode.map2 T.CRES_IfIndentThen
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "IfIndentThenBranch" ->
                        Decode.map2 T.CRES_IfIndentThenBranch
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "IfIndentElseBranch" ->
                        Decode.map2 T.CRES_IfIndentElseBranch
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "IfIndentElse" ->
                        Decode.map2 T.CRES_IfIndentElse
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode If's type: " ++ type_)
            )


listEncoder : T.CRES_List_ -> Encode.Value
listEncoder list_ =
    case list_ of
        T.CRES_ListSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "ListSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_ListOpen row col ->
            Encode.object
                [ ( "type", Encode.string "ListOpen" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_ListExpr expr row col ->
            Encode.object
                [ ( "type", Encode.string "ListExpr" )
                , ( "expr", exprEncoder expr )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_ListEnd row col ->
            Encode.object
                [ ( "type", Encode.string "ListEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_ListIndentOpen row col ->
            Encode.object
                [ ( "type", Encode.string "ListIndentOpen" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_ListIndentEnd row col ->
            Encode.object
                [ ( "type", Encode.string "ListIndentEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_ListIndentExpr row col ->
            Encode.object
                [ ( "type", Encode.string "ListIndentExpr" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


listDecoder : Decode.Decoder T.CRES_List_
listDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "ListSpace" ->
                        Decode.map3 T.CRES_ListSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "ListOpen" ->
                        Decode.map2 T.CRES_ListOpen
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "ListExpr" ->
                        Decode.map3 T.CRES_ListExpr
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "ListEnd" ->
                        Decode.map2 T.CRES_ListEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "ListIndentOpen" ->
                        Decode.map2 T.CRES_ListIndentOpen
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "ListIndentEnd" ->
                        Decode.map2 T.CRES_ListIndentEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "ListIndentExpr" ->
                        Decode.map2 T.CRES_ListIndentExpr
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode List's type: " ++ type_)
            )


recordEncoder : T.CRES_Record -> Encode.Value
recordEncoder record =
    case record of
        T.CRES_RecordOpen row col ->
            Encode.object
                [ ( "type", Encode.string "RecordOpen" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_RecordEnd row col ->
            Encode.object
                [ ( "type", Encode.string "RecordEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_RecordField row col ->
            Encode.object
                [ ( "type", Encode.string "RecordField" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_RecordEquals row col ->
            Encode.object
                [ ( "type", Encode.string "RecordEquals" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_RecordExpr expr row col ->
            Encode.object
                [ ( "type", Encode.string "RecordExpr" )
                , ( "expr", exprEncoder expr )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_RecordSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "RecordSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_RecordIndentOpen row col ->
            Encode.object
                [ ( "type", Encode.string "RecordIndentOpen" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_RecordIndentEnd row col ->
            Encode.object
                [ ( "type", Encode.string "RecordIndentEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_RecordIndentField row col ->
            Encode.object
                [ ( "type", Encode.string "RecordIndentField" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_RecordIndentEquals row col ->
            Encode.object
                [ ( "type", Encode.string "RecordIndentEquals" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_RecordIndentExpr row col ->
            Encode.object
                [ ( "type", Encode.string "RecordIndentExpr" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


recordDecoder : Decode.Decoder T.CRES_Record
recordDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "RecordOpen" ->
                        Decode.map2 T.CRES_RecordOpen
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "RecordEnd" ->
                        Decode.map2 T.CRES_RecordEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "RecordField" ->
                        Decode.map2 T.CRES_RecordField
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "RecordEquals" ->
                        Decode.map2 T.CRES_RecordEquals
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "RecordExpr" ->
                        Decode.map3 T.CRES_RecordExpr
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "RecordSpace" ->
                        Decode.map3 T.CRES_RecordSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "RecordIndentOpen" ->
                        Decode.map2 T.CRES_RecordIndentOpen
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "RecordIndentEnd" ->
                        Decode.map2 T.CRES_RecordIndentEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "RecordIndentField" ->
                        Decode.map2 T.CRES_RecordIndentField
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "RecordIndentEquals" ->
                        Decode.map2 T.CRES_RecordIndentEquals
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "RecordIndentExpr" ->
                        Decode.map2 T.CRES_RecordIndentExpr
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Record's type: " ++ type_)
            )


tupleEncoder : T.CRES_Tuple -> Encode.Value
tupleEncoder tuple =
    case tuple of
        T.CRES_TupleExpr expr row col ->
            Encode.object
                [ ( "type", Encode.string "TupleExpr" )
                , ( "expr", exprEncoder expr )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TupleSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "TupleSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TupleEnd row col ->
            Encode.object
                [ ( "type", Encode.string "TupleEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TupleOperatorClose row col ->
            Encode.object
                [ ( "type", Encode.string "TupleOperatorClose" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TupleOperatorReserved operator row col ->
            Encode.object
                [ ( "type", Encode.string "TupleOperatorReserved" )
                , ( "operator", Compiler.Parse.Symbol.badOperatorEncoder operator )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TupleIndentExpr1 row col ->
            Encode.object
                [ ( "type", Encode.string "TupleIndentExpr1" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TupleIndentExprN row col ->
            Encode.object
                [ ( "type", Encode.string "TupleIndentExprN" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TupleIndentEnd row col ->
            Encode.object
                [ ( "type", Encode.string "TupleIndentEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


tupleDecoder : Decode.Decoder T.CRES_Tuple
tupleDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "TupleExpr" ->
                        Decode.map3 T.CRES_TupleExpr
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TupleSpace" ->
                        Decode.map3 T.CRES_TupleSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TupleEnd" ->
                        Decode.map2 T.CRES_TupleEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TupleOperatorClose" ->
                        Decode.map2 T.CRES_TupleOperatorClose
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TupleOperatorReserved" ->
                        Decode.map3 T.CRES_TupleOperatorReserved
                            (Decode.field "operator" Compiler.Parse.Symbol.badOperatorDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TupleIndentExpr1" ->
                        Decode.map2 T.CRES_TupleIndentExpr1
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TupleIndentExprN" ->
                        Decode.map2 T.CRES_TupleIndentExprN
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TupleIndentEnd" ->
                        Decode.map2 T.CRES_TupleIndentEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Tuple's type: " ++ type_)
            )


funcEncoder : T.CRES_Func -> Encode.Value
funcEncoder func =
    case func of
        T.CRES_FuncSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "FuncSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_FuncArg pattern row col ->
            Encode.object
                [ ( "type", Encode.string "FuncArg" )
                , ( "pattern", patternEncoder pattern )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_FuncBody expr row col ->
            Encode.object
                [ ( "type", Encode.string "FuncBody" )
                , ( "expr", exprEncoder expr )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_FuncArrow row col ->
            Encode.object
                [ ( "type", Encode.string "FuncArrow" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_FuncIndentArg row col ->
            Encode.object
                [ ( "type", Encode.string "FuncIndentArg" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_FuncIndentArrow row col ->
            Encode.object
                [ ( "type", Encode.string "FuncIndentArrow" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_FuncIndentBody row col ->
            Encode.object
                [ ( "type", Encode.string "FuncIndentBody" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


funcDecoder : Decode.Decoder T.CRES_Func
funcDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "FuncSpace" ->
                        Decode.map3 T.CRES_FuncSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "FuncArg" ->
                        Decode.map3 T.CRES_FuncArg
                            (Decode.field "pattern" patternDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "FuncBody" ->
                        Decode.map3 T.CRES_FuncBody
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "FuncArrow" ->
                        Decode.map2 T.CRES_FuncArrow
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "FuncIndentArg" ->
                        Decode.map2 T.CRES_FuncIndentArg
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "FuncIndentArrow" ->
                        Decode.map2 T.CRES_FuncIndentArrow
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "FuncIndentBody" ->
                        Decode.map2 T.CRES_FuncIndentBody
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Func's type: " ++ type_)
            )


charEncoder : T.CRES_Char -> Encode.Value
charEncoder char =
    case char of
        T.CRES_CharEndless ->
            Encode.object
                [ ( "type", Encode.string "CharEndless" )
                ]

        T.CRES_CharEscape escape ->
            Encode.object
                [ ( "type", Encode.string "CharEscape" )
                , ( "escape", escapeEncoder escape )
                ]

        T.CRES_CharNotString width ->
            Encode.object
                [ ( "type", Encode.string "CharNotString" )
                , ( "width", Encode.int width )
                ]


charDecoder : Decode.Decoder T.CRES_Char
charDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "CharEndless" ->
                        Decode.succeed T.CRES_CharEndless

                    "CharEscape" ->
                        Decode.map T.CRES_CharEscape (Decode.field "escape" escapeDecoder)

                    "CharNotString" ->
                        Decode.map T.CRES_CharNotString (Decode.field "width" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Char's type: " ++ type_)
            )


stringEncoder : T.CRES_String_ -> Encode.Value
stringEncoder string_ =
    case string_ of
        T.CRES_StringEndless_Single ->
            Encode.object
                [ ( "type", Encode.string "StringEndless_Single" ) ]

        T.CRES_StringEndless_Multi ->
            Encode.object
                [ ( "type", Encode.string "StringEndless_Multi" ) ]

        T.CRES_StringEscape escape ->
            Encode.object
                [ ( "type", Encode.string "StringEscape" )
                , ( "escape", escapeEncoder escape )
                ]


stringDecoder : Decode.Decoder T.CRES_String_
stringDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "StringEndless_Single" ->
                        Decode.succeed T.CRES_StringEndless_Single

                    "StringEndless_Multi" ->
                        Decode.succeed T.CRES_StringEndless_Multi

                    "StringEscape" ->
                        Decode.map T.CRES_StringEscape (Decode.field "escape" escapeDecoder)

                    _ ->
                        Decode.fail ("Failed to decode String's type: " ++ type_)
            )


numberEncoder : T.CRES_Number -> Encode.Value
numberEncoder number =
    case number of
        T.CRES_NumberEnd ->
            Encode.object
                [ ( "type", Encode.string "NumberEnd" )
                ]

        T.CRES_NumberDot n ->
            Encode.object
                [ ( "type", Encode.string "NumberDot" )
                , ( "n", Encode.int n )
                ]

        T.CRES_NumberHexDigit ->
            Encode.object
                [ ( "type", Encode.string "NumberHexDigit" )
                ]

        T.CRES_NumberNoLeadingZero ->
            Encode.object
                [ ( "type", Encode.string "NumberNoLeadingZero" )
                ]


numberDecoder : Decode.Decoder T.CRES_Number
numberDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "NumberEnd" ->
                        Decode.succeed T.CRES_NumberEnd

                    "NumberDot" ->
                        Decode.map T.CRES_NumberDot (Decode.field "n" Decode.int)

                    "NumberHexDigit" ->
                        Decode.succeed T.CRES_NumberHexDigit

                    "NumberNoLeadingZero" ->
                        Decode.succeed T.CRES_NumberNoLeadingZero

                    _ ->
                        Decode.fail ("Failed to decode Number's type: " ++ type_)
            )


escapeEncoder : T.CRES_Escape -> Encode.Value
escapeEncoder escape =
    case escape of
        T.CRES_EscapeUnknown ->
            Encode.object
                [ ( "type", Encode.string "EscapeUnknown" )
                ]

        T.CRES_BadUnicodeFormat width ->
            Encode.object
                [ ( "type", Encode.string "BadUnicodeFormat" )
                , ( "width", Encode.int width )
                ]

        T.CRES_BadUnicodeCode width ->
            Encode.object
                [ ( "type", Encode.string "BadUnicodeCode" )
                , ( "width", Encode.int width )
                ]

        T.CRES_BadUnicodeLength width numDigits badCode ->
            Encode.object
                [ ( "type", Encode.string "BadUnicodeLength" )
                , ( "width", Encode.int width )
                , ( "numDigits", Encode.int numDigits )
                , ( "badCode", Encode.int badCode )
                ]


escapeDecoder : Decode.Decoder T.CRES_Escape
escapeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "EscapeUnknown" ->
                        Decode.succeed T.CRES_EscapeUnknown

                    "BadUnicodeFormat" ->
                        Decode.map T.CRES_BadUnicodeFormat (Decode.field "width" Decode.int)

                    "BadUnicodeCode" ->
                        Decode.map T.CRES_BadUnicodeCode (Decode.field "width" Decode.int)

                    "BadUnicodeLength" ->
                        Decode.map3 T.CRES_BadUnicodeLength
                            (Decode.field "width" Decode.int)
                            (Decode.field "numDigits" Decode.int)
                            (Decode.field "badCode" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Escape's type: " ++ type_)
            )


defEncoder : T.CRES_Def -> Encode.Value
defEncoder def =
    case def of
        T.CRES_DefSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "DefSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DefType tipe row col ->
            Encode.object
                [ ( "type", Encode.string "DefType" )
                , ( "tipe", typeEncoder tipe )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DefNameRepeat row col ->
            Encode.object
                [ ( "type", Encode.string "DefNameRepeat" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DefNameMatch name row col ->
            Encode.object
                [ ( "type", Encode.string "DefNameMatch" )
                , ( "name", Encode.string name )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DefArg pattern row col ->
            Encode.object
                [ ( "type", Encode.string "DefArg" )
                , ( "pattern", patternEncoder pattern )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DefEquals row col ->
            Encode.object
                [ ( "type", Encode.string "DefEquals" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DefBody expr row col ->
            Encode.object
                [ ( "type", Encode.string "DefBody" )
                , ( "expr", exprEncoder expr )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DefIndentEquals row col ->
            Encode.object
                [ ( "type", Encode.string "DefIndentEquals" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DefIndentType row col ->
            Encode.object
                [ ( "type", Encode.string "DefIndentType" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DefIndentBody row col ->
            Encode.object
                [ ( "type", Encode.string "DefIndentBody" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DefAlignment indent row col ->
            Encode.object
                [ ( "type", Encode.string "DefAlignment" )
                , ( "indent", Encode.int indent )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


defDecoder : Decode.Decoder T.CRES_Def
defDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "DefSpace" ->
                        Decode.map3 T.CRES_DefSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DefType" ->
                        Decode.map3 T.CRES_DefType
                            (Decode.field "tipe" typeDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DefNameRepeat" ->
                        Decode.map2 T.CRES_DefNameRepeat
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DefNameMatch" ->
                        Decode.map3 T.CRES_DefNameMatch
                            (Decode.field "name" Decode.string)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DefArg" ->
                        Decode.map3 T.CRES_DefArg
                            (Decode.field "pattern" patternDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DefEquals" ->
                        Decode.map2 T.CRES_DefEquals
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DefBody" ->
                        Decode.map3 T.CRES_DefBody
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DefIndentEquals" ->
                        Decode.map2 T.CRES_DefIndentEquals
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DefIndentType" ->
                        Decode.map2 T.CRES_DefIndentType
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DefIndentBody" ->
                        Decode.map2 T.CRES_DefIndentBody
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DefAlignment" ->
                        Decode.map3 T.CRES_DefAlignment
                            (Decode.field "indent" Decode.int)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Def's type: " ++ type_)
            )


destructEncoder : T.CRES_Destruct -> Encode.Value
destructEncoder destruct =
    case destruct of
        T.CRES_DestructSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "DestructSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DestructPattern pattern row col ->
            Encode.object
                [ ( "type", Encode.string "DestructPattern" )
                , ( "pattern", patternEncoder pattern )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DestructEquals row col ->
            Encode.object
                [ ( "type", Encode.string "DestructEquals" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DestructBody expr row col ->
            Encode.object
                [ ( "type", Encode.string "DestructBody" )
                , ( "expr", exprEncoder expr )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DestructIndentEquals row col ->
            Encode.object
                [ ( "type", Encode.string "DestructIndentEquals" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_DestructIndentBody row col ->
            Encode.object
                [ ( "type", Encode.string "DestructIndentBody" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


destructDecoder : Decode.Decoder T.CRES_Destruct
destructDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "DestructSpace" ->
                        Decode.map3 T.CRES_DestructSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DestructPattern" ->
                        Decode.map3 T.CRES_DestructPattern
                            (Decode.field "pattern" patternDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DestructEquals" ->
                        Decode.map2 T.CRES_DestructEquals
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DestructBody" ->
                        Decode.map3 T.CRES_DestructBody
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DestructIndentEquals" ->
                        Decode.map2 T.CRES_DestructIndentEquals
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "DestructIndentBody" ->
                        Decode.map2 T.CRES_DestructIndentBody
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Destruct's type: " ++ type_)
            )


pRecordEncoder : T.CRES_PRecord -> Encode.Value
pRecordEncoder pRecord =
    case pRecord of
        T.CRES_PRecordOpen row col ->
            Encode.object
                [ ( "type", Encode.string "PRecordOpen" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PRecordEnd row col ->
            Encode.object
                [ ( "type", Encode.string "PRecordEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PRecordField row col ->
            Encode.object
                [ ( "type", Encode.string "PRecordField" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PRecordSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "PRecordSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PRecordIndentOpen row col ->
            Encode.object
                [ ( "type", Encode.string "PRecordIndentOpen" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PRecordIndentEnd row col ->
            Encode.object
                [ ( "type", Encode.string "PRecordIndentEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PRecordIndentField row col ->
            Encode.object
                [ ( "type", Encode.string "PRecordIndentField" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


pRecordDecoder : Decode.Decoder T.CRES_PRecord
pRecordDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "PRecordOpen" ->
                        Decode.map2 T.CRES_PRecordOpen
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PRecordEnd" ->
                        Decode.map2 T.CRES_PRecordEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PRecordField" ->
                        Decode.map2 T.CRES_PRecordField
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PRecordSpace" ->
                        Decode.map3 T.CRES_PRecordSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PRecordIndentOpen" ->
                        Decode.map2 T.CRES_PRecordIndentOpen
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PRecordIndentEnd" ->
                        Decode.map2 T.CRES_PRecordIndentEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PRecordIndentField" ->
                        Decode.map2 T.CRES_PRecordIndentField
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode PRecord's type: " ++ type_)
            )


pTupleEncoder : T.CRES_PTuple -> Encode.Value
pTupleEncoder pTuple =
    case pTuple of
        T.CRES_PTupleOpen row col ->
            Encode.object
                [ ( "type", Encode.string "PTupleOpen" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PTupleEnd row col ->
            Encode.object
                [ ( "type", Encode.string "PTupleEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PTupleExpr pattern row col ->
            Encode.object
                [ ( "type", Encode.string "PTupleExpr" )
                , ( "pattern", patternEncoder pattern )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PTupleSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "PTupleSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PTupleIndentEnd row col ->
            Encode.object
                [ ( "type", Encode.string "PTupleIndentEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PTupleIndentExpr1 row col ->
            Encode.object
                [ ( "type", Encode.string "PTupleIndentExpr1" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PTupleIndentExprN row col ->
            Encode.object
                [ ( "type", Encode.string "PTupleIndentExprN" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


pTupleDecoder : Decode.Decoder T.CRES_PTuple
pTupleDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "PTupleOpen" ->
                        Decode.map2 T.CRES_PTupleOpen
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PTupleEnd" ->
                        Decode.map2 T.CRES_PTupleEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PTupleExpr" ->
                        Decode.map3 T.CRES_PTupleExpr
                            (Decode.field "pattern" patternDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PTupleSpace" ->
                        Decode.map3 T.CRES_PTupleSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PTupleIndentEnd" ->
                        Decode.map2 T.CRES_PTupleIndentEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PTupleIndentExpr1" ->
                        Decode.map2 T.CRES_PTupleIndentExpr1
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PTupleIndentExprN" ->
                        Decode.map2 T.CRES_PTupleIndentExprN
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode PTuple's type: " ++ type_)
            )


pListEncoder : T.CRES_PList -> Encode.Value
pListEncoder pList =
    case pList of
        T.CRES_PListOpen row col ->
            Encode.object
                [ ( "type", Encode.string "PListOpen" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PListEnd row col ->
            Encode.object
                [ ( "type", Encode.string "PListEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PListExpr pattern row col ->
            Encode.object
                [ ( "type", Encode.string "PListExpr" )
                , ( "pattern", patternEncoder pattern )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PListSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "PListSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PListIndentOpen row col ->
            Encode.object
                [ ( "type", Encode.string "PListIndentOpen" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PListIndentEnd row col ->
            Encode.object
                [ ( "type", Encode.string "PListIndentEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_PListIndentExpr row col ->
            Encode.object
                [ ( "type", Encode.string "PListIndentExpr" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


pListDecoder : Decode.Decoder T.CRES_PList
pListDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "PListOpen" ->
                        Decode.map2 T.CRES_PListOpen
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PListEnd" ->
                        Decode.map2 T.CRES_PListEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PListExpr" ->
                        Decode.map3 T.CRES_PListExpr
                            (Decode.field "pattern" patternDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PListSpace" ->
                        Decode.map3 T.CRES_PListSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PListIndentOpen" ->
                        Decode.map2 T.CRES_PListIndentOpen
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PListIndentEnd" ->
                        Decode.map2 T.CRES_PListIndentEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "PListIndentExpr" ->
                        Decode.map2 T.CRES_PListIndentExpr
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode PList's type: " ++ type_)
            )


tRecordEncoder : T.CRES_TRecord -> Encode.Value
tRecordEncoder tRecord =
    case tRecord of
        T.CRES_TRecordOpen row col ->
            Encode.object
                [ ( "type", Encode.string "TRecordOpen" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TRecordEnd row col ->
            Encode.object
                [ ( "type", Encode.string "TRecordEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TRecordField row col ->
            Encode.object
                [ ( "type", Encode.string "TRecordField" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TRecordColon row col ->
            Encode.object
                [ ( "type", Encode.string "TRecordColon" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TRecordType tipe row col ->
            Encode.object
                [ ( "type", Encode.string "TRecordType" )
                , ( "tipe", typeEncoder tipe )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TRecordSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "TRecordSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TRecordIndentOpen row col ->
            Encode.object
                [ ( "type", Encode.string "TRecordIndentOpen" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TRecordIndentField row col ->
            Encode.object
                [ ( "type", Encode.string "TRecordIndentField" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TRecordIndentColon row col ->
            Encode.object
                [ ( "type", Encode.string "TRecordIndentColon" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TRecordIndentType row col ->
            Encode.object
                [ ( "type", Encode.string "TRecordIndentType" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TRecordIndentEnd row col ->
            Encode.object
                [ ( "type", Encode.string "TRecordIndentEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


tRecordDecoder : Decode.Decoder T.CRES_TRecord
tRecordDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "TRecordOpen" ->
                        Decode.map2 T.CRES_TRecordOpen
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TRecordEnd" ->
                        Decode.map2 T.CRES_TRecordEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TRecordField" ->
                        Decode.map2 T.CRES_TRecordField
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TRecordColon" ->
                        Decode.map2 T.CRES_TRecordColon
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TRecordType" ->
                        Decode.map3 T.CRES_TRecordType
                            (Decode.field "tipe" typeDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TRecordSpace" ->
                        Decode.map3 T.CRES_TRecordSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TRecordIndentOpen" ->
                        Decode.map2 T.CRES_TRecordIndentOpen
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TRecordIndentField" ->
                        Decode.map2 T.CRES_TRecordIndentField
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TRecordIndentColon" ->
                        Decode.map2 T.CRES_TRecordIndentColon
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TRecordIndentType" ->
                        Decode.map2 T.CRES_TRecordIndentType
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TRecordIndentEnd" ->
                        Decode.map2 T.CRES_TRecordIndentEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode TRecord's type: " ++ type_)
            )


tTupleEncoder : T.CRES_TTuple -> Encode.Value
tTupleEncoder tTuple =
    case tTuple of
        T.CRES_TTupleOpen row col ->
            Encode.object
                [ ( "type", Encode.string "TTupleOpen" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TTupleEnd row col ->
            Encode.object
                [ ( "type", Encode.string "TTupleEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TTupleType tipe row col ->
            Encode.object
                [ ( "type", Encode.string "TTupleType" )
                , ( "tipe", typeEncoder tipe )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TTupleSpace space row col ->
            Encode.object
                [ ( "type", Encode.string "TTupleSpace" )
                , ( "space", spaceEncoder space )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TTupleIndentType1 row col ->
            Encode.object
                [ ( "type", Encode.string "TTupleIndentType1" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TTupleIndentTypeN row col ->
            Encode.object
                [ ( "type", Encode.string "TTupleIndentTypeN" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        T.CRES_TTupleIndentEnd row col ->
            Encode.object
                [ ( "type", Encode.string "TTupleIndentEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


tTupleDecoder : Decode.Decoder T.CRES_TTuple
tTupleDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "TTupleOpen" ->
                        Decode.map2 T.CRES_TTupleOpen
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TTupleEnd" ->
                        Decode.map2 T.CRES_TTupleEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TTupleType" ->
                        Decode.map3 T.CRES_TTupleType
                            (Decode.field "tipe" typeDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TTupleSpace" ->
                        Decode.map3 T.CRES_TTupleSpace
                            (Decode.field "space" spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TTupleIndentType1" ->
                        Decode.map2 T.CRES_TTupleIndentType1
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TTupleIndentTypeN" ->
                        Decode.map2 T.CRES_TTupleIndentTypeN
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "TTupleIndentEnd" ->
                        Decode.map2 T.CRES_TTupleIndentEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode TTuple's type: " ++ type_)
            )
