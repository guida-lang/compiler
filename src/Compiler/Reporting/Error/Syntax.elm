module Compiler.Reporting.Error.Syntax exposing
    ( Case(..)
    , Char(..)
    , CustomType(..)
    , Decl(..)
    , DeclDef(..)
    , DeclType(..)
    , Def(..)
    , Destruct(..)
    , Error(..)
    , Escape(..)
    , Exposing(..)
    , Expr(..)
    , Func(..)
    , If(..)
    , Let(..)
    , List_(..)
    , Module(..)
    , Number(..)
    , PList(..)
    , PRecord(..)
    , PTuple(..)
    , Pattern(..)
    , Port(..)
    , Record(..)
    , Space(..)
    , String_(..)
    , TRecord(..)
    , TTuple(..)
    , Tuple(..)
    , Type(..)
    , TypeAlias(..)
    , errorCodec
    , toReport
    , toSpaceReport
    )

import Compiler.Data.Name exposing (Name)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Parse.Primitives exposing (Col, Row)
import Compiler.Parse.Symbol exposing (BadOperator(..))
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Report as Report
import Hex
import Serialize exposing (Codec)



-- ALL SYNTAX ERRORS


type Error
    = ModuleNameUnspecified ModuleName.Raw
    | ModuleNameMismatch ModuleName.Raw (A.Located ModuleName.Raw)
    | UnexpectedPort A.Region
    | NoPorts A.Region
    | NoPortsInPackage (A.Located Name)
    | NoPortModulesInPackage A.Region
    | NoEffectsOutsideKernel A.Region
    | ParseError Module


type Module
    = ModuleSpace Space Row Col
    | ModuleBadEnd Row Col
      --
    | ModuleProblem Row Col
    | ModuleName Row Col
    | ModuleExposing Exposing Row Col
      --
    | PortModuleProblem Row Col
    | PortModuleName Row Col
    | PortModuleExposing Exposing Row Col
      --
    | Effect Row Col
      --
    | FreshLine Row Col
      --
    | ImportStart Row Col
    | ImportName Row Col
    | ImportAs Row Col
    | ImportAlias Row Col
    | ImportExposing Row Col
    | ImportExposingList Exposing Row Col
    | ImportEnd Row Col -- different based on col=1 or if greater
      --
    | ImportIndentName Row Col
    | ImportIndentAlias Row Col
    | ImportIndentExposingList Row Col
      --
    | Infix Row Col
      --
    | Declarations Decl Row Col


type Exposing
    = ExposingSpace Space Row Col
    | ExposingStart Row Col
    | ExposingValue Row Col
    | ExposingOperator Row Col
    | ExposingOperatorReserved BadOperator Row Col
    | ExposingOperatorRightParen Row Col
    | ExposingTypePrivacy Row Col
    | ExposingEnd Row Col
      --
    | ExposingIndentEnd Row Col
    | ExposingIndentValue Row Col



-- DECLARATIONS


type Decl
    = DeclStart Row Col
    | DeclSpace Space Row Col
      --
    | Port Port Row Col
    | DeclType DeclType Row Col
    | DeclDef Name DeclDef Row Col
      --
    | DeclFreshLineAfterDocComment Row Col


type DeclDef
    = DeclDefSpace Space Row Col
    | DeclDefEquals Row Col
    | DeclDefType Type Row Col
    | DeclDefArg Pattern Row Col
    | DeclDefBody Expr Row Col
    | DeclDefNameRepeat Row Col
    | DeclDefNameMatch Name Row Col
      --
    | DeclDefIndentType Row Col
    | DeclDefIndentEquals Row Col
    | DeclDefIndentBody Row Col


type Port
    = PortSpace Space Row Col
    | PortName Row Col
    | PortColon Row Col
    | PortType Type Row Col
    | PortIndentName Row Col
    | PortIndentColon Row Col
    | PortIndentType Row Col



-- TYPE DECLARATIONS


type DeclType
    = DT_Space Space Row Col
    | DT_Name Row Col
    | DT_Alias TypeAlias Row Col
    | DT_Union CustomType Row Col
      --
    | DT_IndentName Row Col


type TypeAlias
    = AliasSpace Space Row Col
    | AliasName Row Col
    | AliasEquals Row Col
    | AliasBody Type Row Col
      --
    | AliasIndentEquals Row Col
    | AliasIndentBody Row Col


type CustomType
    = CT_Space Space Row Col
    | CT_Name Row Col
    | CT_Equals Row Col
    | CT_Bar Row Col
    | CT_Variant Row Col
    | CT_VariantArg Type Row Col
      --
    | CT_IndentEquals Row Col
    | CT_IndentBar Row Col
    | CT_IndentAfterBar Row Col
    | CT_IndentAfterEquals Row Col



-- EXPRESSIONS


type Expr
    = Let Let Row Col
    | Case Case Row Col
    | If If Row Col
    | List List_ Row Col
    | Record Record Row Col
    | Tuple Tuple Row Col
    | Func Func Row Col
      --
    | Dot Row Col
    | Access Row Col
    | OperatorRight Name Row Col
    | OperatorReserved BadOperator Row Col
      --
    | Start Row Col
    | Char Char Row Col
    | String_ String_ Row Col
    | Number Number Row Col
    | Space Space Row Col
    | EndlessShader Row Col
    | ShaderProblem String Row Col
    | IndentOperatorRight Name Row Col


type Record
    = RecordOpen Row Col
    | RecordEnd Row Col
    | RecordField Row Col
    | RecordEquals Row Col
    | RecordExpr Expr Row Col
    | RecordSpace Space Row Col
      --
    | RecordIndentOpen Row Col
    | RecordIndentEnd Row Col
    | RecordIndentField Row Col
    | RecordIndentEquals Row Col
    | RecordIndentExpr Row Col


type Tuple
    = TupleExpr Expr Row Col
    | TupleSpace Space Row Col
    | TupleEnd Row Col
    | TupleOperatorClose Row Col
    | TupleOperatorReserved BadOperator Row Col
      --
    | TupleIndentExpr1 Row Col
    | TupleIndentExprN Row Col
    | TupleIndentEnd Row Col


type List_
    = ListSpace Space Row Col
    | ListOpen Row Col
    | ListExpr Expr Row Col
    | ListEnd Row Col
      --
    | ListIndentOpen Row Col
    | ListIndentEnd Row Col
    | ListIndentExpr Row Col


type Func
    = FuncSpace Space Row Col
    | FuncArg Pattern Row Col
    | FuncBody Expr Row Col
    | FuncArrow Row Col
      --
    | FuncIndentArg Row Col
    | FuncIndentArrow Row Col
    | FuncIndentBody Row Col


type Case
    = CaseSpace Space Row Col
    | CaseOf Row Col
    | CasePattern Pattern Row Col
    | CaseArrow Row Col
    | CaseExpr Expr Row Col
    | CaseBranch Expr Row Col
      --
    | CaseIndentOf Row Col
    | CaseIndentExpr Row Col
    | CaseIndentPattern Row Col
    | CaseIndentArrow Row Col
    | CaseIndentBranch Row Col
    | CasePatternAlignment Int Row Col


type If
    = IfSpace Space Row Col
    | IfThen Row Col
    | IfElse Row Col
    | IfElseBranchStart Row Col
      --
    | IfCondition Expr Row Col
    | IfThenBranch Expr Row Col
    | IfElseBranch Expr Row Col
      --
    | IfIndentCondition Row Col
    | IfIndentThen Row Col
    | IfIndentThenBranch Row Col
    | IfIndentElseBranch Row Col
    | IfIndentElse Row Col


type Let
    = LetSpace Space Row Col
    | LetIn Row Col
    | LetDefAlignment Int Row Col
    | LetDefName Row Col
    | LetDef Name Def Row Col
    | LetDestruct Destruct Row Col
    | LetBody Expr Row Col
    | LetIndentDef Row Col
    | LetIndentIn Row Col
    | LetIndentBody Row Col


type Def
    = DefSpace Space Row Col
    | DefType Type Row Col
    | DefNameRepeat Row Col
    | DefNameMatch Name Row Col
    | DefArg Pattern Row Col
    | DefEquals Row Col
    | DefBody Expr Row Col
    | DefIndentEquals Row Col
    | DefIndentType Row Col
    | DefIndentBody Row Col
    | DefAlignment Int Row Col


type Destruct
    = DestructSpace Space Row Col
    | DestructPattern Pattern Row Col
    | DestructEquals Row Col
    | DestructBody Expr Row Col
    | DestructIndentEquals Row Col
    | DestructIndentBody Row Col



-- PATTERNS


type Pattern
    = PRecord PRecord Row Col
    | PTuple PTuple Row Col
    | PList PList Row Col
      --
    | PStart Row Col
    | PChar Char Row Col
    | PString String_ Row Col
    | PNumber Number Row Col
    | PFloat Int Row Col
    | PAlias Row Col
    | PWildcardNotVar Name Int Row Col
    | PSpace Space Row Col
      --
    | PIndentStart Row Col
    | PIndentAlias Row Col


type PRecord
    = PRecordOpen Row Col
    | PRecordEnd Row Col
    | PRecordField Row Col
    | PRecordSpace Space Row Col
      --
    | PRecordIndentOpen Row Col
    | PRecordIndentEnd Row Col
    | PRecordIndentField Row Col


type PTuple
    = PTupleOpen Row Col
    | PTupleEnd Row Col
    | PTupleExpr Pattern Row Col
    | PTupleSpace Space Row Col
      --
    | PTupleIndentEnd Row Col
    | PTupleIndentExpr1 Row Col
    | PTupleIndentExprN Row Col


type PList
    = PListOpen Row Col
    | PListEnd Row Col
    | PListExpr Pattern Row Col
    | PListSpace Space Row Col
      --
    | PListIndentOpen Row Col
    | PListIndentEnd Row Col
    | PListIndentExpr Row Col



-- TYPES


type Type
    = TRecord TRecord Row Col
    | TTuple TTuple Row Col
      --
    | TStart Row Col
    | TSpace Space Row Col
      --
    | TIndentStart Row Col


type TRecord
    = TRecordOpen Row Col
    | TRecordEnd Row Col
      --
    | TRecordField Row Col
    | TRecordColon Row Col
    | TRecordType Type Row Col
      --
    | TRecordSpace Space Row Col
      --
    | TRecordIndentOpen Row Col
    | TRecordIndentField Row Col
    | TRecordIndentColon Row Col
    | TRecordIndentType Row Col
    | TRecordIndentEnd Row Col


type TTuple
    = TTupleOpen Row Col
    | TTupleEnd Row Col
    | TTupleType Type Row Col
    | TTupleSpace Space Row Col
      --
    | TTupleIndentType1 Row Col
    | TTupleIndentTypeN Row Col
    | TTupleIndentEnd Row Col



-- LITERALS


type Char
    = CharEndless
    | CharEscape Escape
    | CharNotString Int


type String_
    = StringEndless_Single
    | StringEndless_Multi
    | StringEscape Escape


type Escape
    = EscapeUnknown
    | BadUnicodeFormat Int
    | BadUnicodeCode Int
    | BadUnicodeLength Int Int Int


type Number
    = NumberEnd
    | NumberDot Int
    | NumberHexDigit
    | NumberNoLeadingZero



-- MISC


type Space
    = HasTab
    | EndlessMultiComment



-- TO REPORT


toReport : Code.Source -> Error -> Report.Report
toReport source err =
    case err of
        ModuleNameUnspecified name ->
            let
                region : A.Region
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

        ModuleNameMismatch expectedName (A.At region actualName) ->
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

        UnexpectedPort region ->
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

        NoPorts region ->
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

        NoPortsInPackage (A.At region _) ->
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

        NoPortModulesInPackage region ->
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

        NoEffectsOutsideKernel region ->
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

        ParseError modul ->
            toParseErrorReport source modul


noteForPortsInPackage : D.Doc
noteForPortsInPackage =
    D.stack
        [ D.toSimpleNote <|
            "One of the major goals of the package ecosystem is to be completely written in Elm. This means when you install an Elm package, you can be sure you are safe from security issues on install and that you are not going to get any runtime exceptions coming from your new dependency. This design also sets the ecosystem up to target other platforms more easily (like mobile phones, WebAssembly, etc.) since no community code explicitly depends on JavaScript even existing."
        , D.reflow <|
            "Given that overall goal, allowing ports in packages would lead to some pretty surprising behavior. If ports were allowed in packages, you could install a package but not realize that it brings in an indirect dependency that defines a port. Now you have a program that does not work and the fix is to realize that some JavaScript needs to be added for a dependency you did not even know about. That would be extremely frustrating! \"So why not allow the package author to include the necessary JS code as well?\" Now we are back in conflict with our overall goal to keep all community packages free from runtime exceptions."
        ]


toParseErrorReport : Code.Source -> Module -> Report.Report
toParseErrorReport source modul =
    case modul of
        ModuleSpace space row col ->
            toSpaceReport source space row col

        ModuleBadEnd row col ->
            if col == 1 then
                toDeclStartReport source row col

            else
                toWeirdEndReport source row col

        ModuleProblem row col ->
            let
                region : A.Region
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

        ModuleName row col ->
            let
                region : A.Region
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

        ModuleExposing exposing_ row col ->
            toExposingReport source exposing_ row col

        PortModuleProblem row col ->
            let
                region : A.Region
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

        PortModuleName row col ->
            let
                region : A.Region
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

        PortModuleExposing exposing_ row col ->
            toExposingReport source exposing_ row col

        Effect row col ->
            let
                region : A.Region
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

        FreshLine row col ->
            let
                region : A.Region
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

        ImportStart row col ->
            toImportReport source row col

        ImportName row col ->
            let
                region : A.Region
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

        ImportAs row col ->
            toImportReport source row col

        ImportAlias row col ->
            let
                region : A.Region
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

        ImportExposing row col ->
            toImportReport source row col

        ImportExposingList exposing_ row col ->
            toExposingReport source exposing_ row col

        ImportEnd row col ->
            toImportReport source row col

        ImportIndentName row col ->
            toImportReport source row col

        ImportIndentAlias row col ->
            toImportReport source row col

        ImportIndentExposingList row col ->
            let
                region : A.Region
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

        Infix row col ->
            let
                region : A.Region
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

        Declarations decl _ _ ->
            toDeclarationsReport source decl



-- WEIRD END


toWeirdEndReport : Code.Source -> Row -> Col -> Report.Report
toWeirdEndReport source row col =
    case Code.whatIsNext source row col of
        Code.Keyword keyword ->
            let
                region : A.Region
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
                region : A.Region
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
                region : A.Region
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
                region : A.Region
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
                region : A.Region
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
                region : A.Region
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


toWeirdEndSyntaxProblemReport : Code.Source -> A.Region -> Report.Report
toWeirdEndSyntaxProblemReport source region =
    Report.Report "SYNTAX PROBLEM" region [] <|
        Code.toSnippet source region Nothing <|
            ( D.reflow "I got stuck here:"
            , D.reflow "Whatever I am running into is confusing me a lot! Normally I can give fairly specific hints, but something is really tripping me up this time."
            )



-- IMPORTS


toImportReport : Code.Source -> Row -> Col -> Report.Report
toImportReport source row col =
    let
        region : A.Region
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


toExposingReport : Code.Source -> Exposing -> Row -> Col -> Report.Report
toExposingReport source exposing_ startRow startCol =
    case exposing_ of
        ExposingSpace space row col ->
            toSpaceReport source space row col

        ExposingStart row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        ExposingValue row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        ExposingOperator row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        ExposingOperatorReserved op row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
                region =
                    toRegion row col
            in
            Report.Report "RESERVED SYMBOL" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I cannot expose this as an operator:"
                    , case op of
                        BadDot ->
                            D.reflow "Try getting rid of this entry? Maybe I can give you a better hint after that?"

                        BadPipe ->
                            D.fillSep
                                [ D.fromChars "Maybe"
                                , D.fromChars "you"
                                , D.fromChars "want"
                                , D.dullyellow <| D.fromChars "(||)"
                                , D.fromChars "instead?"
                                ]

                        BadArrow ->
                            D.reflow "Try getting rid of this entry? Maybe I can give you a better hint after that?"

                        BadEquals ->
                            D.fillSep
                                [ D.fromChars "Maybe"
                                , D.fromChars "you"
                                , D.fromChars "want"
                                , D.dullyellow <| D.fromChars "(==)"
                                , D.fromChars "instead?"
                                ]

                        BadHasType ->
                            D.fillSep
                                [ D.fromChars "Maybe"
                                , D.fromChars "you"
                                , D.fromChars "want"
                                , D.dullyellow <| D.fromChars "(::)"
                                , D.fromChars "instead?"
                                ]
                    )

        ExposingOperatorRightParen row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        ExposingEnd row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED EXPOSING" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was partway through parsing exposed values, but I got stuck here:"
                    , D.reflow "Maybe there is a comma missing before this?"
                    )

        ExposingTypePrivacy row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        ExposingIndentEnd row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        ExposingIndentValue row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED EXPOSING" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was partway through parsing exposed values, but I got stuck here:"
                    , D.reflow "I was expecting another value to expose."
                    )



-- SPACES


toSpaceReport : Code.Source -> Space -> Row -> Col -> Report.Report
toSpaceReport source space row col =
    case space of
        HasTab ->
            let
                region : A.Region
                region =
                    toRegion row col
            in
            Report.Report "NO TABS" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I ran into a tab, but tabs are not allowed in Elm files."
                    , D.reflow "Replace the tab with spaces."
                    )

        EndlessMultiComment ->
            let
                region : A.Region
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


toRegion : Row -> Col -> A.Region
toRegion row col =
    let
        pos : A.Position
        pos =
            A.Position row col
    in
    A.Region pos pos


toWiderRegion : Row -> Col -> Int -> A.Region
toWiderRegion row col extra =
    A.Region
        (A.Position row col)
        (A.Position row (col + extra))


toKeywordRegion : Row -> Col -> String -> A.Region
toKeywordRegion row col keyword =
    A.Region
        (A.Position row col)
        (A.Position row (col + String.length keyword))


toDeclarationsReport : Code.Source -> Decl -> Report.Report
toDeclarationsReport source decl =
    case decl of
        DeclStart row col ->
            toDeclStartReport source row col

        DeclSpace space row col ->
            toSpaceReport source space row col

        Port port_ row col ->
            toPortReport source port_ row col

        DeclType declType row col ->
            toDeclTypeReport source declType row col

        DeclDef name declDef row col ->
            toDeclDefReport source name declDef row col

        DeclFreshLineAfterDocComment row col ->
            let
                region : A.Region
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING DECLARATION" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I just saw a doc comment, but then I got stuck here:"
                    , D.reflow "I was expecting to see the corresponding declaration next, starting on a fresh line with no indentation."
                    )


toDeclStartReport : Code.Source -> Row -> Col -> Report.Report
toDeclStartReport source row col =
    case Code.whatIsNext source row col of
        Code.Close term bracket ->
            let
                region : A.Region
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
                region : A.Region
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
                region : A.Region
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
                region : A.Region
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


toDeclStartWeirdDeclarationReport : Code.Source -> A.Region -> Report.Report
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


toPortReport : Code.Source -> Port -> Row -> Col -> Report.Report
toPortReport source port_ startRow startCol =
    case port_ of
        PortSpace space row col ->
            toSpaceReport source space row col

        PortName row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        PortColon row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        PortType tipe row col ->
            toTypeReport source TC_Port tipe row col

        PortIndentName row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        PortIndentColon row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        PortIndentType row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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


toDeclTypeReport : Code.Source -> DeclType -> Row -> Col -> Report.Report
toDeclTypeReport source declType startRow startCol =
    case declType of
        DT_Space space row col ->
            toSpaceReport source space row col

        DT_Name row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        DT_Alias typeAlias row col ->
            toTypeAliasReport source typeAlias row col

        DT_Union customType row col ->
            toCustomTypeReport source customType row col

        DT_IndentName row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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


toTypeAliasReport : Code.Source -> TypeAlias -> Row -> Col -> Report.Report
toTypeAliasReport source typeAlias startRow startCol =
    case typeAlias of
        AliasSpace space row col ->
            toSpaceReport source space row col

        AliasName row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        AliasEquals row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        AliasBody tipe row col ->
            toTypeReport source TC_TypeAlias tipe row col

        AliasIndentEquals row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        AliasIndentBody row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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


toCustomTypeReport : Code.Source -> CustomType -> Row -> Col -> Report.Report
toCustomTypeReport source customType startRow startCol =
    case customType of
        CT_Space space row col ->
            toSpaceReport source space row col

        CT_Name row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        CT_Equals row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        CT_Bar row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        CT_Variant row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        CT_VariantArg tipe row col ->
            toTypeReport source TC_CustomType tipe row col

        CT_IndentEquals row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        CT_IndentBar row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        CT_IndentAfterBar row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        CT_IndentAfterEquals row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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


toDeclDefReport : Code.Source -> Name -> DeclDef -> Row -> Col -> Report.Report
toDeclDefReport source name declDef startRow startCol =
    case declDef of
        DeclDefSpace space row col ->
            toSpaceReport source space row col

        DeclDefEquals row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        DeclDefType tipe row col ->
            toTypeReport source (TC_Annotation name) tipe row col

        DeclDefArg pattern row col ->
            toPatternReport source PArg pattern row col

        DeclDefBody expr row col ->
            toExprReport source (InDef name startRow startCol) expr row col

        DeclDefNameRepeat row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        DeclDefNameMatch defName row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        DeclDefIndentType row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        DeclDefIndentEquals row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        DeclDefIndentBody row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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
    = InNode Node Row Col Context
    | InDef Name Row Col
    | InDestruct Row Col


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


getDefName : Context -> Maybe Name
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


toExprReport : Code.Source -> Context -> Expr -> Row -> Col -> Report.Report
toExprReport source context expr startRow startCol =
    case expr of
        Let let_ row col ->
            toLetReport source context let_ row col

        Case case_ row col ->
            toCaseReport source context case_ row col

        If if_ row col ->
            toIfReport source context if_ row col

        List list row col ->
            toListReport source context list row col

        Record record row col ->
            toRecordReport source context record row col

        Tuple tuple row col ->
            toTupleReport source context tuple row col

        Func func row col ->
            toFuncReport source context func row col

        Dot row col ->
            let
                region : A.Region
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

        Access row col ->
            let
                region : A.Region
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

        OperatorRight op row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        OperatorReserved operator row col ->
            toOperatorReport source context operator row col

        Start row col ->
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

                surroundings : A.Region
                surroundings =
                    A.Region (A.Position contextRow contextCol) (A.Position row col)

                region : A.Region
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

        Char char row col ->
            toCharReport source char row col

        String_ string row col ->
            toStringReport source string row col

        Number number row col ->
            toNumberReport source number row col

        Space space row col ->
            toSpaceReport source space row col

        EndlessShader row col ->
            let
                region : A.Region
                region =
                    toWiderRegion row col 6
            in
            Report.Report "ENDLESS SHADER" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I cannot find the end of this shader:"
                    , D.reflow "Add a |] somewhere after this to end the shader."
                    )

        ShaderProblem problem row col ->
            let
                region : A.Region
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

        IndentOperatorRight op row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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


toCharReport : Code.Source -> Char -> Row -> Col -> Report.Report
toCharReport source char row col =
    case char of
        CharEndless ->
            let
                region : A.Region
                region =
                    toRegion row col
            in
            Report.Report "MISSING SINGLE QUOTE" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow
                        "I thought I was parsing a character, but I got to the end of the line without seeing the closing single quote:"
                    , D.reflow "Add a closing single quote here!"
                    )

        CharEscape escape ->
            toEscapeReport source escape row col

        CharNotString width ->
            let
                region : A.Region
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


toStringReport : Code.Source -> String_ -> Row -> Col -> Report.Report
toStringReport source string row col =
    case string of
        StringEndless_Single ->
            let
                region : A.Region
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

        StringEndless_Multi ->
            let
                region : A.Region
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

        StringEscape escape ->
            toEscapeReport source escape row col



-- ESCAPES


toEscapeReport : Code.Source -> Escape -> Row -> Col -> Report.Report
toEscapeReport source escape row col =
    case escape of
        EscapeUnknown ->
            let
                region : A.Region
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

        BadUnicodeFormat width ->
            let
                region : A.Region
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

        BadUnicodeCode width ->
            let
                region : A.Region
                region =
                    toWiderRegion row col width
            in
            Report.Report "BAD UNICODE ESCAPE" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "This is not a valid code point:"
                    , D.reflow "The valid code points are between 0 and 10FFFF inclusive."
                    )

        BadUnicodeLength width numDigits badCode ->
            let
                region : A.Region
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


toNumberReport : Code.Source -> Number -> Row -> Col -> Report.Report
toNumberReport source number row col =
    let
        region : A.Region
        region =
            toRegion row col
    in
    case number of
        NumberEnd ->
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

        NumberDot int ->
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

        NumberHexDigit ->
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

        NumberNoLeadingZero ->
            Report.Report "LEADING ZEROS" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I do not accept numbers with leading zeros:"
                    , D.stack
                        [ D.reflow "Just delete the leading zeros and it should work!"
                        , D.toSimpleNote "Some languages let you to specify octal numbers by adding a leading zero. So in C, writing 0111 is the same as writing 73. Some people are used to that, but others probably want it to equal 111. Either path is going to surprise people from certain backgrounds, so Elm tries to avoid this whole situation."
                        ]
                    )



-- OPERATORS


toOperatorReport : Code.Source -> Context -> BadOperator -> Row -> Col -> Report.Report
toOperatorReport source context operator row col =
    case operator of
        BadDot ->
            let
                region : A.Region
                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED SYMBOL" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.fromChars "I was not expecting this dot:"
                    , D.reflow "Dots are for record access and decimal points, so they cannot float around on their own. Maybe there is some extra whitespace?"
                    )

        BadPipe ->
            let
                region : A.Region
                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED SYMBOL" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I was not expecting this vertical bar:"
                    , D.reflow "Vertical bars should only appear in custom type declarations. Maybe you want || instead?"
                    )

        BadArrow ->
            let
                region : A.Region
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

        BadEquals ->
            let
                region : A.Region
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

        BadHasType ->
            let
                region : A.Region
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


toLetReport : Code.Source -> Context -> Let -> Row -> Col -> Report.Report
toLetReport source context let_ startRow startCol =
    case let_ of
        LetSpace space row col ->
            toSpaceReport source space row col

        LetIn row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        LetDefAlignment _ row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        LetDefName row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        LetDef name def row col ->
            toLetDefReport source name def row col

        LetDestruct destruct row col ->
            toLetDestructReport source destruct row col

        LetBody expr row col ->
            toExprReport source context expr row col

        LetIndentDef row col ->
            toUnfinishLetReport source row col startRow startCol <|
                D.reflow
                    "I was expecting a value to be defined here."

        LetIndentIn row col ->
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

        LetIndentBody row col ->
            toUnfinishLetReport source row col startRow startCol <|
                D.reflow
                    "I was expecting an expression next. Tell me what should happen with the value you just defined!"


toUnfinishLetReport : Code.Source -> Row -> Col -> Row -> Col -> D.Doc -> Report.Report
toUnfinishLetReport source row col startRow startCol message =
    let
        surroundings : A.Region
        surroundings =
            A.Region (A.Position startRow startCol) (A.Position row col)

        region : A.Region
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


toLetDefReport : Code.Source -> Name -> Def -> Row -> Col -> Report.Report
toLetDefReport source name def startRow startCol =
    case def of
        DefSpace space row col ->
            toSpaceReport source space row col

        DefType tipe row col ->
            toTypeReport source (TC_Annotation name) tipe row col

        DefNameRepeat row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        DefNameMatch defName row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        DefArg pattern row col ->
            toPatternReport source PArg pattern row col

        DefEquals row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        DefBody expr row col ->
            toExprReport source (InDef name startRow startCol) expr row col

        DefIndentEquals row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        DefIndentType row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        DefIndentBody row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        DefAlignment indent row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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


toLetDestructReport : Code.Source -> Destruct -> Row -> Col -> Report.Report
toLetDestructReport source destruct startRow startCol =
    case destruct of
        DestructSpace space row col ->
            toSpaceReport source space row col

        DestructPattern pattern row col ->
            toPatternReport source PLet pattern row col

        DestructEquals row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        DestructBody expr row col ->
            toExprReport source (InDestruct startRow startCol) expr row col

        DestructIndentEquals row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I got stuck trying to parse this definition:"
                    , D.reflow "I was expecting to see an equals sign next, followed by an expression telling me what to compute."
                    )

        DestructIndentBody row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I got stuck while parsing this definition:"
                    , D.reflow "I was expecting to see an expression next. What is it equal to?"
                    )



-- CASE


toCaseReport : Code.Source -> Context -> Case -> Row -> Col -> Report.Report
toCaseReport source context case_ startRow startCol =
    case case_ of
        CaseSpace space row col ->
            toSpaceReport source space row col

        CaseOf row col ->
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

        CasePattern pattern row col ->
            toPatternReport source PCase pattern row col

        CaseArrow row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        CaseExpr expr row col ->
            toExprReport source (InNode NCase startRow startCol context) expr row col

        CaseBranch expr row col ->
            toExprReport source (InNode NBranch startRow startCol context) expr row col

        CaseIndentOf row col ->
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

        CaseIndentExpr row col ->
            toUnfinishCaseReport source
                row
                col
                startRow
                startCol
                (D.reflow "I was expecting to see an expression next.")

        CaseIndentPattern row col ->
            toUnfinishCaseReport source
                row
                col
                startRow
                startCol
                (D.reflow "I was expecting to see a pattern next.")

        CaseIndentArrow row col ->
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

        CaseIndentBranch row col ->
            toUnfinishCaseReport source
                row
                col
                startRow
                startCol
                (D.reflow "I was expecting to see an expression next. What should I do when I run into this particular pattern?")

        CasePatternAlignment indent row col ->
            toUnfinishCaseReport source
                row
                col
                startRow
                startCol
                (D.reflow ("I suspect this is a pattern that is not indented far enough? (" ++ String.fromInt indent ++ " spaces)"))


toUnfinishCaseReport : Code.Source -> Int -> Int -> Int -> Int -> D.Doc -> Report.Report
toUnfinishCaseReport source row col startRow startCol message =
    let
        surroundings : A.Region
        surroundings =
            A.Region (A.Position startRow startCol) (A.Position row col)

        region : A.Region
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


toIfReport : Code.Source -> Context -> If -> Row -> Col -> Report.Report
toIfReport source context if_ startRow startCol =
    case if_ of
        IfSpace space row col ->
            toSpaceReport source space row col

        IfThen row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        IfElse row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        IfElseBranchStart row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I just saw the start of an `else` branch, but then I got stuck here:"
                    , D.reflow "I was expecting to see an expression next. Maybe it is not filled in yet?"
                    )

        IfCondition expr row col ->
            toExprReport source (InNode NCond startRow startCol context) expr row col

        IfThenBranch expr row col ->
            toExprReport source (InNode NThen startRow startCol context) expr row col

        IfElseBranch expr row col ->
            toExprReport source (InNode NElse startRow startCol context) expr row col

        IfIndentCondition row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        IfIndentThen row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        IfIndentThenBranch row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        IfIndentElseBranch row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        IfIndentElse row col ->
            case Code.nextLineStartsWithKeyword "else" source row of
                Just ( elseRow, elseCol ) ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position elseRow elseCol)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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


toRecordReport : Code.Source -> Context -> Record -> Row -> Col -> Report.Report
toRecordReport source context record startRow startCol =
    case record of
        RecordOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        RecordEnd row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        RecordField row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        RecordEquals row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        RecordExpr expr row col ->
            toExprReport source (InNode NRecord startRow startCol context) expr row col

        RecordSpace space row col ->
            toSpaceReport source space row col

        RecordIndentOpen row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        RecordIndentEnd row col ->
            case Code.nextLineStartsWithCloseCurly source row of
                Just ( curlyRow, curlyCol ) ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position curlyRow curlyCol)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        RecordIndentField row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        RecordIndentEquals row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        RecordIndentExpr row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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


toTupleReport : Code.Source -> Context -> Tuple -> Row -> Col -> Report.Report
toTupleReport source context tuple startRow startCol =
    case tuple of
        TupleExpr expr row col ->
            toExprReport source (InNode NParens startRow startCol context) expr row col

        TupleSpace space row col ->
            toSpaceReport source space row col

        TupleEnd row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        TupleOperatorClose row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        TupleOperatorReserved operator row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED SYMBOL" region [] <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I ran into an unexpected symbol here:"
                    , D.fillSep
                        (case operator of
                            BadDot ->
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

                            BadPipe ->
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

                            BadArrow ->
                                [ D.fromChars "Maybe"
                                , D.fromChars "you"
                                , D.fromChars "wanted"
                                , D.dullyellow <| D.fromChars "(>)"
                                , D.fromChars "or"
                                , D.dullyellow <| D.fromChars "(>=)"
                                , D.fromChars "instead?"
                                ]

                            BadEquals ->
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

                            BadHasType ->
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

        TupleIndentExpr1 row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        TupleIndentExprN row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        TupleIndentEnd row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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


toListReport : Code.Source -> Context -> List_ -> Row -> Col -> Report.Report
toListReport source context list startRow startCol =
    case list of
        ListSpace space row col ->
            toSpaceReport source space row col

        ListOpen row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        ListExpr expr row col ->
            case expr of
                Start r c ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position r c)

                        region : A.Region
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

        ListEnd row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        ListIndentOpen row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        ListIndentEnd row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        ListIndentExpr row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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


toFuncReport : Code.Source -> Context -> Func -> Row -> Col -> Report.Report
toFuncReport source context func startRow startCol =
    case func of
        FuncSpace space row col ->
            toSpaceReport source space row col

        FuncArg pattern row col ->
            toPatternReport source PArg pattern row col

        FuncBody expr row col ->
            toExprReport source (InNode NFunc startRow startCol context) expr row col

        FuncArrow row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        FuncIndentArg row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        FuncIndentArrow row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        FuncIndentBody row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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


toPatternReport : Code.Source -> PContext -> Pattern -> Row -> Col -> Report.Report
toPatternReport source context pattern startRow startCol =
    case pattern of
        PRecord record row col ->
            toPRecordReport source record row col

        PTuple tuple row col ->
            toPTupleReport source context tuple row col

        PList list row col ->
            toPListReport source context list row col

        PStart row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        PChar char row col ->
            toCharReport source char row col

        PString string row col ->
            toStringReport source string row col

        PNumber number row col ->
            toNumberReport source number row col

        PFloat width row col ->
            let
                region : A.Region
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

        PAlias row col ->
            let
                region : A.Region
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

        PWildcardNotVar name width row col ->
            let
                region : A.Region
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

        PSpace space row col ->
            toSpaceReport source space row col

        PIndentStart row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        PIndentAlias row col ->
            let
                region : A.Region
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


toPRecordReport : Code.Source -> PRecord -> Row -> Col -> Report.Report
toPRecordReport source record startRow startCol =
    case record of
        PRecordOpen row col ->
            toUnfinishRecordPatternReport source row col startRow startCol <|
                D.reflow "I was expecting to see a field name next."

        PRecordEnd row col ->
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

        PRecordField row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        PRecordSpace space row col ->
            toSpaceReport source space row col

        PRecordIndentOpen row col ->
            toUnfinishRecordPatternReport source row col startRow startCol <|
                D.reflow "I was expecting to see a field name next."

        PRecordIndentEnd row col ->
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

        PRecordIndentField row col ->
            toUnfinishRecordPatternReport source row col startRow startCol <|
                D.reflow "I was expecting to see a field name next."


toUnfinishRecordPatternReport : Code.Source -> Row -> Col -> Row -> Col -> D.Doc -> Report.Report
toUnfinishRecordPatternReport source row col startRow startCol message =
    let
        surroundings : A.Region
        surroundings =
            A.Region (A.Position startRow startCol) (A.Position row col)

        region : A.Region
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


toPTupleReport : Code.Source -> PContext -> PTuple -> Row -> Col -> Report.Report
toPTupleReport source context tuple startRow startCol =
    case tuple of
        PTupleOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        PTupleEnd row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        PTupleExpr pattern row col ->
            toPatternReport source context pattern row col

        PTupleSpace space row col ->
            toSpaceReport source space row col

        PTupleIndentEnd row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        PTupleIndentExpr1 row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        PTupleIndentExprN row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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


toPListReport : Code.Source -> PContext -> PList -> Row -> Col -> Report.Report
toPListReport source context list startRow startCol =
    case list of
        PListOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        PListEnd row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        PListExpr pattern row col ->
            toPatternReport source context pattern row col

        PListSpace space row col ->
            toSpaceReport source space row col

        PListIndentOpen row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        PListIndentEnd row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        PListIndentExpr row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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
    = TC_Annotation Name
    | TC_CustomType
    | TC_TypeAlias
    | TC_Port


toTypeReport : Code.Source -> TContext -> Type -> Row -> Col -> Report.Report
toTypeReport source context tipe startRow startCol =
    case tipe of
        TRecord record row col ->
            toTRecordReport source context record row col

        TTuple tuple row col ->
            toTTupleReport source context tuple row col

        TStart row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        TSpace space row col ->
            toSpaceReport source space row col

        TIndentStart row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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


toTRecordReport : Code.Source -> TContext -> TRecord -> Row -> Col -> Report.Report
toTRecordReport source context record startRow startCol =
    case record of
        TRecordOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        TRecordEnd row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        TRecordField row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        TRecordColon row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        TRecordType tipe row col ->
            toTypeReport source context tipe row col

        TRecordSpace space row col ->
            toSpaceReport source space row col

        TRecordIndentOpen row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        TRecordIndentEnd row col ->
            case Code.nextLineStartsWithCloseCurly source row of
                Just ( curlyRow, curlyCol ) ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position curlyRow curlyCol)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        TRecordIndentField row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        TRecordIndentColon row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        TRecordIndentType row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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


toTTupleReport : Code.Source -> TContext -> TTuple -> Row -> Col -> Report.Report
toTTupleReport source context tuple startRow startCol =
    case tuple of
        TTupleOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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
                        surroundings : A.Region
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region : A.Region
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

        TTupleEnd row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        TTupleType tipe row col ->
            toTypeReport source context tipe row col

        TTupleSpace space row col ->
            toSpaceReport source space row col

        TTupleIndentType1 row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        TTupleIndentTypeN row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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

        TTupleIndentEnd row col ->
            let
                surroundings : A.Region
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region : A.Region
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


errorCodec : Codec e Error
errorCodec =
    Serialize.customType
        (\moduleNameUnspecifiedEncoder moduleNameMismatchEncoder unexpectedPortEncoder noPortsEncoder noPortsInPackageEncoder noPortModulesInPackageEncoder noEffectsOutsideKernelEncoder parseErrorEncoder value ->
            case value of
                ModuleNameUnspecified name ->
                    moduleNameUnspecifiedEncoder name

                ModuleNameMismatch expectedName actualName ->
                    moduleNameMismatchEncoder expectedName actualName

                UnexpectedPort region ->
                    unexpectedPortEncoder region

                NoPorts region ->
                    noPortsEncoder region

                NoPortsInPackage name ->
                    noPortsInPackageEncoder name

                NoPortModulesInPackage region ->
                    noPortModulesInPackageEncoder region

                NoEffectsOutsideKernel region ->
                    noEffectsOutsideKernelEncoder region

                ParseError modul ->
                    parseErrorEncoder modul
        )
        |> Serialize.variant1 ModuleNameUnspecified ModuleName.rawCodec
        |> Serialize.variant2 ModuleNameMismatch ModuleName.rawCodec (A.locatedCodec ModuleName.rawCodec)
        |> Serialize.variant1 UnexpectedPort A.regionCodec
        |> Serialize.variant1 NoPorts A.regionCodec
        |> Serialize.variant1 NoPortsInPackage (A.locatedCodec Serialize.string)
        |> Serialize.variant1 NoPortModulesInPackage A.regionCodec
        |> Serialize.variant1 NoEffectsOutsideKernel A.regionCodec
        |> Serialize.variant1 ParseError moduleCodec
        |> Serialize.finishCustomType


spaceCodec : Codec e Space
spaceCodec =
    Serialize.customType
        (\hasTabEncoder endlessMultiCommentEncoder value ->
            case value of
                HasTab ->
                    hasTabEncoder

                EndlessMultiComment ->
                    endlessMultiCommentEncoder
        )
        |> Serialize.variant0 HasTab
        |> Serialize.variant0 EndlessMultiComment
        |> Serialize.finishCustomType


moduleCodec : Codec e Module
moduleCodec =
    Serialize.customType
        (\moduleSpaceEncoder moduleBadEndEncoder moduleProblemEncoder moduleNameEncoder moduleExposingEncoder portModuleProblemEncoder portModuleNameEncoder portModuleExposingEncoder effectEncoder freshLineEncoder importStartEncoder importNameEncoder importAsEncoder importAliasEncoder importExposingEncoder importExposingListEncoder importEndEncoder importIndentNameEncoder importIndentAliasEncoder importIndentExposingListEncoder infixEncoder declarationsEncoder value ->
            case value of
                ModuleSpace space row col ->
                    moduleSpaceEncoder space row col

                ModuleBadEnd row col ->
                    moduleBadEndEncoder row col

                ModuleProblem row col ->
                    moduleProblemEncoder row col

                ModuleName row col ->
                    moduleNameEncoder row col

                ModuleExposing exposing_ row col ->
                    moduleExposingEncoder exposing_ row col

                PortModuleProblem row col ->
                    portModuleProblemEncoder row col

                PortModuleName row col ->
                    portModuleNameEncoder row col

                PortModuleExposing exposing_ row col ->
                    portModuleExposingEncoder exposing_ row col

                Effect row col ->
                    effectEncoder row col

                FreshLine row col ->
                    freshLineEncoder row col

                ImportStart row col ->
                    importStartEncoder row col

                ImportName row col ->
                    importNameEncoder row col

                ImportAs row col ->
                    importAsEncoder row col

                ImportAlias row col ->
                    importAliasEncoder row col

                ImportExposing row col ->
                    importExposingEncoder row col

                ImportExposingList exposing_ row col ->
                    importExposingListEncoder exposing_ row col

                ImportEnd row col ->
                    importEndEncoder row col

                ImportIndentName row col ->
                    importIndentNameEncoder row col

                ImportIndentAlias row col ->
                    importIndentAliasEncoder row col

                ImportIndentExposingList row col ->
                    importIndentExposingListEncoder row col

                Infix row col ->
                    infixEncoder row col

                Declarations decl row col ->
                    declarationsEncoder decl row col
        )
        |> Serialize.variant3 ModuleSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 ModuleBadEnd Serialize.int Serialize.int
        |> Serialize.variant2 ModuleProblem Serialize.int Serialize.int
        |> Serialize.variant2 ModuleName Serialize.int Serialize.int
        |> Serialize.variant3 ModuleExposing exposingCodec Serialize.int Serialize.int
        |> Serialize.variant2 PortModuleProblem Serialize.int Serialize.int
        |> Serialize.variant2 PortModuleName Serialize.int Serialize.int
        |> Serialize.variant3 PortModuleExposing exposingCodec Serialize.int Serialize.int
        |> Serialize.variant2 Effect Serialize.int Serialize.int
        |> Serialize.variant2 FreshLine Serialize.int Serialize.int
        |> Serialize.variant2 ImportStart Serialize.int Serialize.int
        |> Serialize.variant2 ImportName Serialize.int Serialize.int
        |> Serialize.variant2 ImportAs Serialize.int Serialize.int
        |> Serialize.variant2 ImportAlias Serialize.int Serialize.int
        |> Serialize.variant2 ImportExposing Serialize.int Serialize.int
        |> Serialize.variant3 ImportExposingList exposingCodec Serialize.int Serialize.int
        |> Serialize.variant2 ImportEnd Serialize.int Serialize.int
        |> Serialize.variant2 ImportIndentName Serialize.int Serialize.int
        |> Serialize.variant2 ImportIndentAlias Serialize.int Serialize.int
        |> Serialize.variant2 ImportIndentExposingList Serialize.int Serialize.int
        |> Serialize.variant2 Infix Serialize.int Serialize.int
        |> Serialize.variant3 Declarations declCodec Serialize.int Serialize.int
        |> Serialize.finishCustomType


exposingCodec : Codec e Exposing
exposingCodec =
    Serialize.customType
        (\exposingSpaceEncoder exposingStartEncoder exposingValueEncoder exposingOperatorEncoder exposingOperatorReservedEncoder exposingOperatorRightParenEncoder exposingTypePrivacyEncoder exposingEndEncoder exposingIndentEndEncoder exposingIndentValueEncoder value ->
            case value of
                ExposingSpace space row col ->
                    exposingSpaceEncoder space row col

                ExposingStart row col ->
                    exposingStartEncoder row col

                ExposingValue row col ->
                    exposingValueEncoder row col

                ExposingOperator row col ->
                    exposingOperatorEncoder row col

                ExposingOperatorReserved op row col ->
                    exposingOperatorReservedEncoder op row col

                ExposingOperatorRightParen row col ->
                    exposingOperatorRightParenEncoder row col

                ExposingTypePrivacy row col ->
                    exposingTypePrivacyEncoder row col

                ExposingEnd row col ->
                    exposingEndEncoder row col

                ExposingIndentEnd row col ->
                    exposingIndentEndEncoder row col

                ExposingIndentValue row col ->
                    exposingIndentValueEncoder row col
        )
        |> Serialize.variant3 ExposingSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 ExposingStart Serialize.int Serialize.int
        |> Serialize.variant2 ExposingValue Serialize.int Serialize.int
        |> Serialize.variant2 ExposingOperator Serialize.int Serialize.int
        |> Serialize.variant3 ExposingOperatorReserved Compiler.Parse.Symbol.badOperatorCodec Serialize.int Serialize.int
        |> Serialize.variant2 ExposingOperatorRightParen Serialize.int Serialize.int
        |> Serialize.variant2 ExposingTypePrivacy Serialize.int Serialize.int
        |> Serialize.variant2 ExposingEnd Serialize.int Serialize.int
        |> Serialize.variant2 ExposingIndentEnd Serialize.int Serialize.int
        |> Serialize.variant2 ExposingIndentValue Serialize.int Serialize.int
        |> Serialize.finishCustomType


declCodec : Codec e Decl
declCodec =
    Serialize.customType
        (\declStartEncoder declSpaceEncoder portCodecEncoder declTypeCodecEncoder declDefCodecEncoder declFreshLineAfterDocCommentEncoder value ->
            case value of
                DeclStart row col ->
                    declStartEncoder row col

                DeclSpace space row col ->
                    declSpaceEncoder space row col

                Port port_ row col ->
                    portCodecEncoder port_ row col

                DeclType declType row col ->
                    declTypeCodecEncoder declType row col

                DeclDef name declDef row col ->
                    declDefCodecEncoder name declDef row col

                DeclFreshLineAfterDocComment row col ->
                    declFreshLineAfterDocCommentEncoder row col
        )
        |> Serialize.variant2 DeclStart Serialize.int Serialize.int
        |> Serialize.variant3 DeclSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant3 Port portCodec Serialize.int Serialize.int
        |> Serialize.variant3 DeclType declTypeCodec Serialize.int Serialize.int
        |> Serialize.variant4 DeclDef Serialize.string declDefCodec Serialize.int Serialize.int
        |> Serialize.variant2 DeclFreshLineAfterDocComment Serialize.int Serialize.int
        |> Serialize.finishCustomType


portCodec : Codec e Port
portCodec =
    Serialize.customType
        (\portSpaceEncoder portNameEncoder portColonEncoder portTypeEncoder portIndentNameEncoder portIndentColonEncoder portIndentTypeEncoder value ->
            case value of
                PortSpace space row col ->
                    portSpaceEncoder space row col

                PortName row col ->
                    portNameEncoder row col

                PortColon row col ->
                    portColonEncoder row col

                PortType tipe row col ->
                    portTypeEncoder tipe row col

                PortIndentName row col ->
                    portIndentNameEncoder row col

                PortIndentColon row col ->
                    portIndentColonEncoder row col

                PortIndentType row col ->
                    portIndentTypeEncoder row col
        )
        |> Serialize.variant3 PortSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 PortName Serialize.int Serialize.int
        |> Serialize.variant2 PortColon Serialize.int Serialize.int
        |> Serialize.variant3 PortType typeCodec Serialize.int Serialize.int
        |> Serialize.variant2 PortIndentName Serialize.int Serialize.int
        |> Serialize.variant2 PortIndentColon Serialize.int Serialize.int
        |> Serialize.variant2 PortIndentType Serialize.int Serialize.int
        |> Serialize.finishCustomType


declTypeCodec : Codec e DeclType
declTypeCodec =
    Serialize.customType
        (\dT_SpaceEncoder dT_NameEncoder dT_AliasEncoder dT_UnionEncoder dT_IndentNameEncoder value ->
            case value of
                DT_Space space row col ->
                    dT_SpaceEncoder space row col

                DT_Name row col ->
                    dT_NameEncoder row col

                DT_Alias typeAlias row col ->
                    dT_AliasEncoder typeAlias row col

                DT_Union customType row col ->
                    dT_UnionEncoder customType row col

                DT_IndentName row col ->
                    dT_IndentNameEncoder row col
        )
        |> Serialize.variant3 DT_Space spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 DT_Name Serialize.int Serialize.int
        |> Serialize.variant3 DT_Alias typeAliasCodec Serialize.int Serialize.int
        |> Serialize.variant3 DT_Union customTypeCodec Serialize.int Serialize.int
        |> Serialize.variant2 DT_IndentName Serialize.int Serialize.int
        |> Serialize.finishCustomType


declDefCodec : Codec e DeclDef
declDefCodec =
    Serialize.customType
        (\declDefSpaceEncoder declDefEqualsEncoder declDefTypeEncoder declDefArgEncoder declDefBodyEncoder declDefNameRepeatEncoder declDefNameMatchEncoder declDefIndentTypeEncoder declDefIndentEqualsEncoder declDefIndentBodyEncoder value ->
            case value of
                DeclDefSpace space row col ->
                    declDefSpaceEncoder space row col

                DeclDefEquals row col ->
                    declDefEqualsEncoder row col

                DeclDefType tipe row col ->
                    declDefTypeEncoder tipe row col

                DeclDefArg pattern row col ->
                    declDefArgEncoder pattern row col

                DeclDefBody expr row col ->
                    declDefBodyEncoder expr row col

                DeclDefNameRepeat row col ->
                    declDefNameRepeatEncoder row col

                DeclDefNameMatch name row col ->
                    declDefNameMatchEncoder name row col

                DeclDefIndentType row col ->
                    declDefIndentTypeEncoder row col

                DeclDefIndentEquals row col ->
                    declDefIndentEqualsEncoder row col

                DeclDefIndentBody row col ->
                    declDefIndentBodyEncoder row col
        )
        |> Serialize.variant3 DeclDefSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 DeclDefEquals Serialize.int Serialize.int
        |> Serialize.variant3 DeclDefType typeCodec Serialize.int Serialize.int
        |> Serialize.variant3 DeclDefArg patternCodec Serialize.int Serialize.int
        |> Serialize.variant3 DeclDefBody exprCodec Serialize.int Serialize.int
        |> Serialize.variant2 DeclDefNameRepeat Serialize.int Serialize.int
        |> Serialize.variant3 DeclDefNameMatch Serialize.string Serialize.int Serialize.int
        |> Serialize.variant2 DeclDefIndentType Serialize.int Serialize.int
        |> Serialize.variant2 DeclDefIndentEquals Serialize.int Serialize.int
        |> Serialize.variant2 DeclDefIndentBody Serialize.int Serialize.int
        |> Serialize.finishCustomType


typeCodec : Codec e Type
typeCodec =
    Serialize.customType
        (\tRecordCodecEncoder tTupleCodecEncoder tStartEncoder tSpaceEncoder tIndentStartEncoder value ->
            case value of
                TRecord record row col ->
                    tRecordCodecEncoder record row col

                TTuple tuple row col ->
                    tTupleCodecEncoder tuple row col

                TStart row col ->
                    tStartEncoder row col

                TSpace space row col ->
                    tSpaceEncoder space row col

                TIndentStart row col ->
                    tIndentStartEncoder row col
        )
        |> Serialize.variant3 TRecord tRecordCodec Serialize.int Serialize.int
        |> Serialize.variant3 TTuple (Serialize.lazy (\() -> tTupleCodec)) Serialize.int Serialize.int
        |> Serialize.variant2 TStart Serialize.int Serialize.int
        |> Serialize.variant3 TSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 TIndentStart Serialize.int Serialize.int
        |> Serialize.finishCustomType


patternCodec : Codec e Pattern
patternCodec =
    Serialize.customType
        (\pRecordCodecEncoder pTupleCodecEncoder pListCodecEncoder pStartEncoder pCharEncoder pStringEncoder pNumberEncoder pFloatEncoder pAliasEncoder pWildcardNotVarEncoder pSpaceEncoder pIndentStartEncoder pIndentAliasEncoder value ->
            case value of
                PRecord record row col ->
                    pRecordCodecEncoder record row col

                PTuple tuple row col ->
                    pTupleCodecEncoder tuple row col

                PList list row col ->
                    pListCodecEncoder list row col

                PStart row col ->
                    pStartEncoder row col

                PChar char row col ->
                    pCharEncoder char row col

                PString string row col ->
                    pStringEncoder string row col

                PNumber number row col ->
                    pNumberEncoder number row col

                PFloat width row col ->
                    pFloatEncoder width row col

                PAlias row col ->
                    pAliasEncoder row col

                PWildcardNotVar name width row col ->
                    pWildcardNotVarEncoder name width row col

                PSpace space row col ->
                    pSpaceEncoder space row col

                PIndentStart row col ->
                    pIndentStartEncoder row col

                PIndentAlias row col ->
                    pIndentAliasEncoder row col
        )
        |> Serialize.variant3 PRecord pRecordCodec Serialize.int Serialize.int
        |> Serialize.variant3 PTuple (Serialize.lazy (\() -> pTupleCodec)) Serialize.int Serialize.int
        |> Serialize.variant3 PList pListCodec Serialize.int Serialize.int
        |> Serialize.variant2 PStart Serialize.int Serialize.int
        |> Serialize.variant3 PChar charCodec Serialize.int Serialize.int
        |> Serialize.variant3 PString string_Codec Serialize.int Serialize.int
        |> Serialize.variant3 PNumber numberCodec Serialize.int Serialize.int
        |> Serialize.variant3 PFloat Serialize.int Serialize.int Serialize.int
        |> Serialize.variant2 PAlias Serialize.int Serialize.int
        |> Serialize.variant4 PWildcardNotVar Serialize.string Serialize.int Serialize.int Serialize.int
        |> Serialize.variant3 PSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 PIndentStart Serialize.int Serialize.int
        |> Serialize.variant2 PIndentAlias Serialize.int Serialize.int
        |> Serialize.finishCustomType


exprCodec : Codec e Expr
exprCodec =
    Serialize.customType
        (\letCodecEncoder caseCodecEncoder ifCodecEncoder listCodecEncoder recordCodecEncoder tupleCodecEncoder funcCodecEncoder dotEncoder accessEncoder operatorRightEncoder operatorReservedEncoder startEncoder charCodecEncoder string_Encoder numberCodecEncoder spaceCodecEncoder endlessShaderEncoder shaderProblemEncoder indentOperatorRightEncoder value ->
            case value of
                Let let_ row col ->
                    letCodecEncoder let_ row col

                Case case_ row col ->
                    caseCodecEncoder case_ row col

                If if_ row col ->
                    ifCodecEncoder if_ row col

                List list row col ->
                    listCodecEncoder list row col

                Record record row col ->
                    recordCodecEncoder record row col

                Tuple tuple row col ->
                    tupleCodecEncoder tuple row col

                Func func row col ->
                    funcCodecEncoder func row col

                Dot row col ->
                    dotEncoder row col

                Access row col ->
                    accessEncoder row col

                OperatorRight op row col ->
                    operatorRightEncoder op row col

                OperatorReserved operator row col ->
                    operatorReservedEncoder operator row col

                Start row col ->
                    startEncoder row col

                Char char row col ->
                    charCodecEncoder char row col

                String_ string row col ->
                    string_Encoder string row col

                Number number row col ->
                    numberCodecEncoder number row col

                Space space row col ->
                    spaceCodecEncoder space row col

                EndlessShader row col ->
                    endlessShaderEncoder row col

                ShaderProblem problem row col ->
                    shaderProblemEncoder problem row col

                IndentOperatorRight op row col ->
                    indentOperatorRightEncoder op row col
        )
        |> Serialize.variant3 Let (Serialize.lazy (\() -> letCodec)) Serialize.int Serialize.int
        |> Serialize.variant3 Case caseCodec Serialize.int Serialize.int
        |> Serialize.variant3 If (Serialize.lazy (\() -> ifCodec)) Serialize.int Serialize.int
        |> Serialize.variant3 List (Serialize.lazy (\() -> listCodec)) Serialize.int Serialize.int
        |> Serialize.variant3 Record (Serialize.lazy (\() -> recordCodec)) Serialize.int Serialize.int
        |> Serialize.variant3 Tuple (Serialize.lazy (\() -> tupleCodec)) Serialize.int Serialize.int
        |> Serialize.variant3 Func (Serialize.lazy (\() -> funcCodec)) Serialize.int Serialize.int
        |> Serialize.variant2 Dot Serialize.int Serialize.int
        |> Serialize.variant2 Access Serialize.int Serialize.int
        |> Serialize.variant3 OperatorRight Serialize.string Serialize.int Serialize.int
        |> Serialize.variant3 OperatorReserved Compiler.Parse.Symbol.badOperatorCodec Serialize.int Serialize.int
        |> Serialize.variant2 Start Serialize.int Serialize.int
        |> Serialize.variant3 Char charCodec Serialize.int Serialize.int
        |> Serialize.variant3 String_ string_Codec Serialize.int Serialize.int
        |> Serialize.variant3 Number numberCodec Serialize.int Serialize.int
        |> Serialize.variant3 Space spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 EndlessShader Serialize.int Serialize.int
        |> Serialize.variant3 ShaderProblem Serialize.string Serialize.int Serialize.int
        |> Serialize.variant3 IndentOperatorRight Serialize.string Serialize.int Serialize.int
        |> Serialize.finishCustomType


letCodec : Codec e Let
letCodec =
    Serialize.customType
        (\letSpaceEncoder letInEncoder letDefAlignmentEncoder letDefNameEncoder letDefEncoder letDestructEncoder letBodyEncoder letIndentDefEncoder letIndentInEncoder letIndentBodyEncoder value ->
            case value of
                LetSpace space row col ->
                    letSpaceEncoder space row col

                LetIn row col ->
                    letInEncoder row col

                LetDefAlignment int row col ->
                    letDefAlignmentEncoder int row col

                LetDefName row col ->
                    letDefNameEncoder row col

                LetDef name def row col ->
                    letDefEncoder name def row col

                LetDestruct destruct row col ->
                    letDestructEncoder destruct row col

                LetBody expr row col ->
                    letBodyEncoder expr row col

                LetIndentDef row col ->
                    letIndentDefEncoder row col

                LetIndentIn row col ->
                    letIndentInEncoder row col

                LetIndentBody row col ->
                    letIndentBodyEncoder row col
        )
        |> Serialize.variant3 LetSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 LetIn Serialize.int Serialize.int
        |> Serialize.variant3 LetDefAlignment Serialize.int Serialize.int Serialize.int
        |> Serialize.variant2 LetDefName Serialize.int Serialize.int
        |> Serialize.variant4 LetDef Serialize.string defCodec Serialize.int Serialize.int
        |> Serialize.variant3 LetDestruct destructCodec Serialize.int Serialize.int
        |> Serialize.variant3 LetBody exprCodec Serialize.int Serialize.int
        |> Serialize.variant2 LetIndentDef Serialize.int Serialize.int
        |> Serialize.variant2 LetIndentIn Serialize.int Serialize.int
        |> Serialize.variant2 LetIndentBody Serialize.int Serialize.int
        |> Serialize.finishCustomType


caseCodec : Codec e Case
caseCodec =
    Serialize.customType
        (\caseSpaceEncoder caseOfEncoder casePatternEncoder caseArrowEncoder caseExprEncoder caseBranchEncoder caseIndentOfEncoder caseIndentExprEncoder caseIndentPatternEncoder caseIndentArrowEncoder caseIndentBranchEncoder casePatternAlignmentEncoder value ->
            case value of
                CaseSpace space row col ->
                    caseSpaceEncoder space row col

                CaseOf row col ->
                    caseOfEncoder row col

                CasePattern pattern row col ->
                    casePatternEncoder pattern row col

                CaseArrow row col ->
                    caseArrowEncoder row col

                CaseExpr expr row col ->
                    caseExprEncoder expr row col

                CaseBranch expr row col ->
                    caseBranchEncoder expr row col

                CaseIndentOf row col ->
                    caseIndentOfEncoder row col

                CaseIndentExpr row col ->
                    caseIndentExprEncoder row col

                CaseIndentPattern row col ->
                    caseIndentPatternEncoder row col

                CaseIndentArrow row col ->
                    caseIndentArrowEncoder row col

                CaseIndentBranch row col ->
                    caseIndentBranchEncoder row col

                CasePatternAlignment indent row col ->
                    casePatternAlignmentEncoder indent row col
        )
        |> Serialize.variant3 CaseSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 CaseOf Serialize.int Serialize.int
        |> Serialize.variant3 CasePattern patternCodec Serialize.int Serialize.int
        |> Serialize.variant2 CaseArrow Serialize.int Serialize.int
        |> Serialize.variant3 CaseExpr (Serialize.lazy (\() -> exprCodec)) Serialize.int Serialize.int
        |> Serialize.variant3 CaseBranch (Serialize.lazy (\() -> exprCodec)) Serialize.int Serialize.int
        |> Serialize.variant2 CaseIndentOf Serialize.int Serialize.int
        |> Serialize.variant2 CaseIndentExpr Serialize.int Serialize.int
        |> Serialize.variant2 CaseIndentPattern Serialize.int Serialize.int
        |> Serialize.variant2 CaseIndentArrow Serialize.int Serialize.int
        |> Serialize.variant2 CaseIndentBranch Serialize.int Serialize.int
        |> Serialize.variant3 CasePatternAlignment Serialize.int Serialize.int Serialize.int
        |> Serialize.finishCustomType


ifCodec : Codec e If
ifCodec =
    Serialize.customType
        (\ifSpaceEncoder ifThenEncoder ifElseEncoder ifElseBranchStartEncoder ifConditionEncoder ifThenBranchEncoder ifElseBranchEncoder ifIndentConditionEncoder ifIndentThenEncoder ifIndentThenBranchEncoder ifIndentElseBranchEncoder ifIndentElseEncoder value ->
            case value of
                IfSpace space row col ->
                    ifSpaceEncoder space row col

                IfThen row col ->
                    ifThenEncoder row col

                IfElse row col ->
                    ifElseEncoder row col

                IfElseBranchStart row col ->
                    ifElseBranchStartEncoder row col

                IfCondition expr row col ->
                    ifConditionEncoder expr row col

                IfThenBranch expr row col ->
                    ifThenBranchEncoder expr row col

                IfElseBranch expr row col ->
                    ifElseBranchEncoder expr row col

                IfIndentCondition row col ->
                    ifIndentConditionEncoder row col

                IfIndentThen row col ->
                    ifIndentThenEncoder row col

                IfIndentThenBranch row col ->
                    ifIndentThenBranchEncoder row col

                IfIndentElseBranch row col ->
                    ifIndentElseBranchEncoder row col

                IfIndentElse row col ->
                    ifIndentElseEncoder row col
        )
        |> Serialize.variant3 IfSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 IfThen Serialize.int Serialize.int
        |> Serialize.variant2 IfElse Serialize.int Serialize.int
        |> Serialize.variant2 IfElseBranchStart Serialize.int Serialize.int
        |> Serialize.variant3 IfCondition exprCodec Serialize.int Serialize.int
        |> Serialize.variant3 IfThenBranch exprCodec Serialize.int Serialize.int
        |> Serialize.variant3 IfElseBranch exprCodec Serialize.int Serialize.int
        |> Serialize.variant2 IfIndentCondition Serialize.int Serialize.int
        |> Serialize.variant2 IfIndentThen Serialize.int Serialize.int
        |> Serialize.variant2 IfIndentThenBranch Serialize.int Serialize.int
        |> Serialize.variant2 IfIndentElseBranch Serialize.int Serialize.int
        |> Serialize.variant2 IfIndentElse Serialize.int Serialize.int
        |> Serialize.finishCustomType


listCodec : Codec e List_
listCodec =
    Serialize.customType
        (\listSpaceEncoder listOpenEncoder listExprEncoder listEndEncoder listIndentOpenEncoder listIndentEndEncoder listIndentExprEncoder value ->
            case value of
                ListSpace space row col ->
                    listSpaceEncoder space row col

                ListOpen row col ->
                    listOpenEncoder row col

                ListExpr expr row col ->
                    listExprEncoder expr row col

                ListEnd row col ->
                    listEndEncoder row col

                ListIndentOpen row col ->
                    listIndentOpenEncoder row col

                ListIndentEnd row col ->
                    listIndentEndEncoder row col

                ListIndentExpr row col ->
                    listIndentExprEncoder row col
        )
        |> Serialize.variant3 ListSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 ListOpen Serialize.int Serialize.int
        |> Serialize.variant3 ListExpr exprCodec Serialize.int Serialize.int
        |> Serialize.variant2 ListEnd Serialize.int Serialize.int
        |> Serialize.variant2 ListIndentOpen Serialize.int Serialize.int
        |> Serialize.variant2 ListIndentEnd Serialize.int Serialize.int
        |> Serialize.variant2 ListIndentExpr Serialize.int Serialize.int
        |> Serialize.finishCustomType


recordCodec : Codec e Record
recordCodec =
    Serialize.customType
        (\recordOpenEncoder recordEndEncoder recordFieldEncoder recordEqualsEncoder recordExprEncoder recordSpaceEncoder recordIndentOpenEncoder recordIndentEndEncoder recordIndentFieldEncoder recordIndentEqualsEncoder recordIndentExprEncoder value ->
            case value of
                RecordOpen row col ->
                    recordOpenEncoder row col

                RecordEnd row col ->
                    recordEndEncoder row col

                RecordField row col ->
                    recordFieldEncoder row col

                RecordEquals row col ->
                    recordEqualsEncoder row col

                RecordExpr expr row col ->
                    recordExprEncoder expr row col

                RecordSpace space row col ->
                    recordSpaceEncoder space row col

                RecordIndentOpen row col ->
                    recordIndentOpenEncoder row col

                RecordIndentEnd row col ->
                    recordIndentEndEncoder row col

                RecordIndentField row col ->
                    recordIndentFieldEncoder row col

                RecordIndentEquals row col ->
                    recordIndentEqualsEncoder row col

                RecordIndentExpr row col ->
                    recordIndentExprEncoder row col
        )
        |> Serialize.variant2 RecordOpen Serialize.int Serialize.int
        |> Serialize.variant2 RecordEnd Serialize.int Serialize.int
        |> Serialize.variant2 RecordField Serialize.int Serialize.int
        |> Serialize.variant2 RecordEquals Serialize.int Serialize.int
        |> Serialize.variant3 RecordExpr exprCodec Serialize.int Serialize.int
        |> Serialize.variant3 RecordSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 RecordIndentOpen Serialize.int Serialize.int
        |> Serialize.variant2 RecordIndentEnd Serialize.int Serialize.int
        |> Serialize.variant2 RecordIndentField Serialize.int Serialize.int
        |> Serialize.variant2 RecordIndentEquals Serialize.int Serialize.int
        |> Serialize.variant2 RecordIndentExpr Serialize.int Serialize.int
        |> Serialize.finishCustomType


tupleCodec : Codec e Tuple
tupleCodec =
    Serialize.customType
        (\tupleExprEncoder tupleSpaceEncoder tupleEndEncoder tupleOperatorCloseEncoder tupleOperatorReservedEncoder tupleIndentExpr1Encoder tupleIndentExprNEncoder tupleIndentEndEncoder value ->
            case value of
                TupleExpr expr row col ->
                    tupleExprEncoder expr row col

                TupleSpace space row col ->
                    tupleSpaceEncoder space row col

                TupleEnd row col ->
                    tupleEndEncoder row col

                TupleOperatorClose row col ->
                    tupleOperatorCloseEncoder row col

                TupleOperatorReserved operator row col ->
                    tupleOperatorReservedEncoder operator row col

                TupleIndentExpr1 row col ->
                    tupleIndentExpr1Encoder row col

                TupleIndentExprN row col ->
                    tupleIndentExprNEncoder row col

                TupleIndentEnd row col ->
                    tupleIndentEndEncoder row col
        )
        |> Serialize.variant3 TupleExpr exprCodec Serialize.int Serialize.int
        |> Serialize.variant3 TupleSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 TupleEnd Serialize.int Serialize.int
        |> Serialize.variant2 TupleOperatorClose Serialize.int Serialize.int
        |> Serialize.variant3 TupleOperatorReserved Compiler.Parse.Symbol.badOperatorCodec Serialize.int Serialize.int
        |> Serialize.variant2 TupleIndentExpr1 Serialize.int Serialize.int
        |> Serialize.variant2 TupleIndentExprN Serialize.int Serialize.int
        |> Serialize.variant2 TupleIndentEnd Serialize.int Serialize.int
        |> Serialize.finishCustomType


funcCodec : Codec e Func
funcCodec =
    Serialize.customType
        (\funcSpaceEncoder funcArgEncoder funcBodyEncoder funcArrowEncoder funcIndentArgEncoder funcIndentArrowEncoder funcIndentBodyEncoder value ->
            case value of
                FuncSpace space row col ->
                    funcSpaceEncoder space row col

                FuncArg pattern row col ->
                    funcArgEncoder pattern row col

                FuncBody expr row col ->
                    funcBodyEncoder expr row col

                FuncArrow row col ->
                    funcArrowEncoder row col

                FuncIndentArg row col ->
                    funcIndentArgEncoder row col

                FuncIndentArrow row col ->
                    funcIndentArrowEncoder row col

                FuncIndentBody row col ->
                    funcIndentBodyEncoder row col
        )
        |> Serialize.variant3 FuncSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant3 FuncArg patternCodec Serialize.int Serialize.int
        |> Serialize.variant3 FuncBody exprCodec Serialize.int Serialize.int
        |> Serialize.variant2 FuncArrow Serialize.int Serialize.int
        |> Serialize.variant2 FuncIndentArg Serialize.int Serialize.int
        |> Serialize.variant2 FuncIndentArrow Serialize.int Serialize.int
        |> Serialize.variant2 FuncIndentBody Serialize.int Serialize.int
        |> Serialize.finishCustomType


charCodec : Codec e Char
charCodec =
    Serialize.customType
        (\charEndlessEncoder charEscapeEncoder charNotStringEncoder value ->
            case value of
                CharEndless ->
                    charEndlessEncoder

                CharEscape escape ->
                    charEscapeEncoder escape

                CharNotString width ->
                    charNotStringEncoder width
        )
        |> Serialize.variant0 CharEndless
        |> Serialize.variant1 CharEscape escapeCodec
        |> Serialize.variant1 CharNotString Serialize.int
        |> Serialize.finishCustomType


string_Codec : Codec e String_
string_Codec =
    Serialize.customType
        (\stringEndless_SingleEncoder stringEndless_MultiEncoder stringEscapeEncoder value ->
            case value of
                StringEndless_Single ->
                    stringEndless_SingleEncoder

                StringEndless_Multi ->
                    stringEndless_MultiEncoder

                StringEscape escape ->
                    stringEscapeEncoder escape
        )
        |> Serialize.variant0 StringEndless_Single
        |> Serialize.variant0 StringEndless_Multi
        |> Serialize.variant1 StringEscape escapeCodec
        |> Serialize.finishCustomType


numberCodec : Codec e Number
numberCodec =
    Serialize.customType
        (\numberEndEncoder numberDotEncoder numberHexDigitEncoder numberNoLeadingZeroEncoder value ->
            case value of
                NumberEnd ->
                    numberEndEncoder

                NumberDot n ->
                    numberDotEncoder n

                NumberHexDigit ->
                    numberHexDigitEncoder

                NumberNoLeadingZero ->
                    numberNoLeadingZeroEncoder
        )
        |> Serialize.variant0 NumberEnd
        |> Serialize.variant1 NumberDot Serialize.int
        |> Serialize.variant0 NumberHexDigit
        |> Serialize.variant0 NumberNoLeadingZero
        |> Serialize.finishCustomType


escapeCodec : Codec e Escape
escapeCodec =
    Serialize.customType
        (\escapeUnknownEncoder badUnicodeFormatEncoder badUnicodeCodeEncoder badUnicodeLengthEncoder value ->
            case value of
                EscapeUnknown ->
                    escapeUnknownEncoder

                BadUnicodeFormat width ->
                    badUnicodeFormatEncoder width

                BadUnicodeCode width ->
                    badUnicodeCodeEncoder width

                BadUnicodeLength width numDigits badCode ->
                    badUnicodeLengthEncoder width numDigits badCode
        )
        |> Serialize.variant0 EscapeUnknown
        |> Serialize.variant1 BadUnicodeFormat Serialize.int
        |> Serialize.variant1 BadUnicodeCode Serialize.int
        |> Serialize.variant3 BadUnicodeLength Serialize.int Serialize.int Serialize.int
        |> Serialize.finishCustomType


defCodec : Codec e Def
defCodec =
    Serialize.customType
        (\defSpaceEncoder defTypeEncoder defNameRepeatEncoder defNameMatchEncoder defArgEncoder defEqualsEncoder defBodyEncoder defIndentEqualsEncoder defIndentTypeEncoder defIndentBodyEncoder defAlignmentEncoder value ->
            case value of
                DefSpace space row col ->
                    defSpaceEncoder space row col

                DefType tipe row col ->
                    defTypeEncoder tipe row col

                DefNameRepeat row col ->
                    defNameRepeatEncoder row col

                DefNameMatch name row col ->
                    defNameMatchEncoder name row col

                DefArg pattern row col ->
                    defArgEncoder pattern row col

                DefEquals row col ->
                    defEqualsEncoder row col

                DefBody expr row col ->
                    defBodyEncoder expr row col

                DefIndentEquals row col ->
                    defIndentEqualsEncoder row col

                DefIndentType row col ->
                    defIndentTypeEncoder row col

                DefIndentBody row col ->
                    defIndentBodyEncoder row col

                DefAlignment indent row col ->
                    defAlignmentEncoder indent row col
        )
        |> Serialize.variant3 DefSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant3 DefType typeCodec Serialize.int Serialize.int
        |> Serialize.variant2 DefNameRepeat Serialize.int Serialize.int
        |> Serialize.variant3 DefNameMatch Serialize.string Serialize.int Serialize.int
        |> Serialize.variant3 DefArg patternCodec Serialize.int Serialize.int
        |> Serialize.variant2 DefEquals Serialize.int Serialize.int
        |> Serialize.variant3 DefBody exprCodec Serialize.int Serialize.int
        |> Serialize.variant2 DefIndentEquals Serialize.int Serialize.int
        |> Serialize.variant2 DefIndentType Serialize.int Serialize.int
        |> Serialize.variant2 DefIndentBody Serialize.int Serialize.int
        |> Serialize.variant3 DefAlignment Serialize.int Serialize.int Serialize.int
        |> Serialize.finishCustomType


destructCodec : Codec e Destruct
destructCodec =
    Serialize.customType
        (\destructSpaceEncoder destructPatternEncoder destructEqualsEncoder destructBodyEncoder destructIndentEqualsEncoder destructIndentBodyEncoder value ->
            case value of
                DestructSpace space row col ->
                    destructSpaceEncoder space row col

                DestructPattern pattern row col ->
                    destructPatternEncoder pattern row col

                DestructEquals row col ->
                    destructEqualsEncoder row col

                DestructBody expr row col ->
                    destructBodyEncoder expr row col

                DestructIndentEquals row col ->
                    destructIndentEqualsEncoder row col

                DestructIndentBody row col ->
                    destructIndentBodyEncoder row col
        )
        |> Serialize.variant3 DestructSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant3 DestructPattern patternCodec Serialize.int Serialize.int
        |> Serialize.variant2 DestructEquals Serialize.int Serialize.int
        |> Serialize.variant3 DestructBody exprCodec Serialize.int Serialize.int
        |> Serialize.variant2 DestructIndentEquals Serialize.int Serialize.int
        |> Serialize.variant2 DestructIndentBody Serialize.int Serialize.int
        |> Serialize.finishCustomType


pRecordCodec : Codec e PRecord
pRecordCodec =
    Serialize.customType
        (\pRecordOpenEncoder pRecordEndEncoder pRecordFieldEncoder pRecordSpaceEncoder pRecordIndentOpenEncoder pRecordIndentEndEncoder pRecordIndentFieldEncoder value ->
            case value of
                PRecordOpen row col ->
                    pRecordOpenEncoder row col

                PRecordEnd row col ->
                    pRecordEndEncoder row col

                PRecordField row col ->
                    pRecordFieldEncoder row col

                PRecordSpace space row col ->
                    pRecordSpaceEncoder space row col

                PRecordIndentOpen row col ->
                    pRecordIndentOpenEncoder row col

                PRecordIndentEnd row col ->
                    pRecordIndentEndEncoder row col

                PRecordIndentField row col ->
                    pRecordIndentFieldEncoder row col
        )
        |> Serialize.variant2 PRecordOpen Serialize.int Serialize.int
        |> Serialize.variant2 PRecordEnd Serialize.int Serialize.int
        |> Serialize.variant2 PRecordField Serialize.int Serialize.int
        |> Serialize.variant3 PRecordSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 PRecordIndentOpen Serialize.int Serialize.int
        |> Serialize.variant2 PRecordIndentEnd Serialize.int Serialize.int
        |> Serialize.variant2 PRecordIndentField Serialize.int Serialize.int
        |> Serialize.finishCustomType


pTupleCodec : Codec e PTuple
pTupleCodec =
    Serialize.customType
        (\pTupleOpenEncoder pTupleEndEncoder pTupleExprEncoder pTupleSpaceEncoder pTupleIndentEndEncoder pTupleIndentExpr1Encoder pTupleIndentExprNEncoder value ->
            case value of
                PTupleOpen row col ->
                    pTupleOpenEncoder row col

                PTupleEnd row col ->
                    pTupleEndEncoder row col

                PTupleExpr pattern row col ->
                    pTupleExprEncoder pattern row col

                PTupleSpace space row col ->
                    pTupleSpaceEncoder space row col

                PTupleIndentEnd row col ->
                    pTupleIndentEndEncoder row col

                PTupleIndentExpr1 row col ->
                    pTupleIndentExpr1Encoder row col

                PTupleIndentExprN row col ->
                    pTupleIndentExprNEncoder row col
        )
        |> Serialize.variant2 PTupleOpen Serialize.int Serialize.int
        |> Serialize.variant2 PTupleEnd Serialize.int Serialize.int
        |> Serialize.variant3 PTupleExpr patternCodec Serialize.int Serialize.int
        |> Serialize.variant3 PTupleSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 PTupleIndentEnd Serialize.int Serialize.int
        |> Serialize.variant2 PTupleIndentExpr1 Serialize.int Serialize.int
        |> Serialize.variant2 PTupleIndentExprN Serialize.int Serialize.int
        |> Serialize.finishCustomType


pListCodec : Codec e PList
pListCodec =
    Serialize.customType
        (\pListOpenEncoder pListEndEncoder pListExprEncoder pListSpaceEncoder pListIndentOpenEncoder pListIndentEndEncoder pListIndentExprEncoder value ->
            case value of
                PListOpen row col ->
                    pListOpenEncoder row col

                PListEnd row col ->
                    pListEndEncoder row col

                PListExpr pattern row col ->
                    pListExprEncoder pattern row col

                PListSpace space row col ->
                    pListSpaceEncoder space row col

                PListIndentOpen row col ->
                    pListIndentOpenEncoder row col

                PListIndentEnd row col ->
                    pListIndentEndEncoder row col

                PListIndentExpr row col ->
                    pListIndentExprEncoder row col
        )
        |> Serialize.variant2 PListOpen Serialize.int Serialize.int
        |> Serialize.variant2 PListEnd Serialize.int Serialize.int
        |> Serialize.variant3 PListExpr (Serialize.lazy (\() -> patternCodec)) Serialize.int Serialize.int
        |> Serialize.variant3 PListSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 PListIndentOpen Serialize.int Serialize.int
        |> Serialize.variant2 PListIndentEnd Serialize.int Serialize.int
        |> Serialize.variant2 PListIndentExpr Serialize.int Serialize.int
        |> Serialize.finishCustomType


tRecordCodec : Codec e TRecord
tRecordCodec =
    Serialize.customType
        (\tRecordOpenEncoder tRecordEndEncoder tRecordFieldEncoder tRecordColonEncoder tRecordTypeEncoder tRecordSpaceEncoder tRecordIndentOpenEncoder tRecordIndentFieldEncoder tRecordIndentColonEncoder tRecordIndentTypeEncoder tRecordIndentEndEncoder value ->
            case value of
                TRecordOpen row col ->
                    tRecordOpenEncoder row col

                TRecordEnd row col ->
                    tRecordEndEncoder row col

                TRecordField row col ->
                    tRecordFieldEncoder row col

                TRecordColon row col ->
                    tRecordColonEncoder row col

                TRecordType tipe row col ->
                    tRecordTypeEncoder tipe row col

                TRecordSpace space row col ->
                    tRecordSpaceEncoder space row col

                TRecordIndentOpen row col ->
                    tRecordIndentOpenEncoder row col

                TRecordIndentField row col ->
                    tRecordIndentFieldEncoder row col

                TRecordIndentColon row col ->
                    tRecordIndentColonEncoder row col

                TRecordIndentType row col ->
                    tRecordIndentTypeEncoder row col

                TRecordIndentEnd row col ->
                    tRecordIndentEndEncoder row col
        )
        |> Serialize.variant2 TRecordOpen Serialize.int Serialize.int
        |> Serialize.variant2 TRecordEnd Serialize.int Serialize.int
        |> Serialize.variant2 TRecordField Serialize.int Serialize.int
        |> Serialize.variant2 TRecordColon Serialize.int Serialize.int
        |> Serialize.variant3 TRecordType (Serialize.lazy (\() -> typeCodec)) Serialize.int Serialize.int
        |> Serialize.variant3 TRecordSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 TRecordIndentOpen Serialize.int Serialize.int
        |> Serialize.variant2 TRecordIndentField Serialize.int Serialize.int
        |> Serialize.variant2 TRecordIndentColon Serialize.int Serialize.int
        |> Serialize.variant2 TRecordIndentType Serialize.int Serialize.int
        |> Serialize.variant2 TRecordIndentEnd Serialize.int Serialize.int
        |> Serialize.finishCustomType


tTupleCodec : Codec e TTuple
tTupleCodec =
    Serialize.customType
        (\tTupleOpenEncoder tTupleEndEncoder tTupleTypeEncoder tTupleSpaceEncoder tTupleIndentType1Encoder tTupleIndentTypeNEncoder tTupleIndentEndEncoder value ->
            case value of
                TTupleOpen row col ->
                    tTupleOpenEncoder row col

                TTupleEnd row col ->
                    tTupleEndEncoder row col

                TTupleType tipe row col ->
                    tTupleTypeEncoder tipe row col

                TTupleSpace space row col ->
                    tTupleSpaceEncoder space row col

                TTupleIndentType1 row col ->
                    tTupleIndentType1Encoder row col

                TTupleIndentTypeN row col ->
                    tTupleIndentTypeNEncoder row col

                TTupleIndentEnd row col ->
                    tTupleIndentEndEncoder row col
        )
        |> Serialize.variant2 TTupleOpen Serialize.int Serialize.int
        |> Serialize.variant2 TTupleEnd Serialize.int Serialize.int
        |> Serialize.variant3 TTupleType typeCodec Serialize.int Serialize.int
        |> Serialize.variant3 TTupleSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 TTupleIndentType1 Serialize.int Serialize.int
        |> Serialize.variant2 TTupleIndentTypeN Serialize.int Serialize.int
        |> Serialize.variant2 TTupleIndentEnd Serialize.int Serialize.int
        |> Serialize.finishCustomType


customTypeCodec : Codec e CustomType
customTypeCodec =
    Serialize.customType
        (\cT_SpaceEncoder cT_NameEncoder cT_EqualsEncoder cT_BarEncoder cT_VariantEncoder cT_VariantArgEncoder cT_IndentEqualsEncoder cT_IndentBarEncoder cT_IndentAfterBarEncoder cT_IndentAfterEqualsEncoder value ->
            case value of
                CT_Space space row col ->
                    cT_SpaceEncoder space row col

                CT_Name row col ->
                    cT_NameEncoder row col

                CT_Equals row col ->
                    cT_EqualsEncoder row col

                CT_Bar row col ->
                    cT_BarEncoder row col

                CT_Variant row col ->
                    cT_VariantEncoder row col

                CT_VariantArg tipe row col ->
                    cT_VariantArgEncoder tipe row col

                CT_IndentEquals row col ->
                    cT_IndentEqualsEncoder row col

                CT_IndentBar row col ->
                    cT_IndentBarEncoder row col

                CT_IndentAfterBar row col ->
                    cT_IndentAfterBarEncoder row col

                CT_IndentAfterEquals row col ->
                    cT_IndentAfterEqualsEncoder row col
        )
        |> Serialize.variant3 CT_Space spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 CT_Name Serialize.int Serialize.int
        |> Serialize.variant2 CT_Equals Serialize.int Serialize.int
        |> Serialize.variant2 CT_Bar Serialize.int Serialize.int
        |> Serialize.variant2 CT_Variant Serialize.int Serialize.int
        |> Serialize.variant3 CT_VariantArg typeCodec Serialize.int Serialize.int
        |> Serialize.variant2 CT_IndentEquals Serialize.int Serialize.int
        |> Serialize.variant2 CT_IndentBar Serialize.int Serialize.int
        |> Serialize.variant2 CT_IndentAfterBar Serialize.int Serialize.int
        |> Serialize.variant2 CT_IndentAfterEquals Serialize.int Serialize.int
        |> Serialize.finishCustomType


typeAliasCodec : Codec e TypeAlias
typeAliasCodec =
    Serialize.customType
        (\aliasSpaceEncoder aliasNameEncoder aliasEqualsEncoder aliasBodyEncoder aliasIndentEqualsEncoder aliasIndentBodyEncoder value ->
            case value of
                AliasSpace space row col ->
                    aliasSpaceEncoder space row col

                AliasName row col ->
                    aliasNameEncoder row col

                AliasEquals row col ->
                    aliasEqualsEncoder row col

                AliasBody tipe row col ->
                    aliasBodyEncoder tipe row col

                AliasIndentEquals row col ->
                    aliasIndentEqualsEncoder row col

                AliasIndentBody row col ->
                    aliasIndentBodyEncoder row col
        )
        |> Serialize.variant3 AliasSpace spaceCodec Serialize.int Serialize.int
        |> Serialize.variant2 AliasName Serialize.int Serialize.int
        |> Serialize.variant2 AliasEquals Serialize.int Serialize.int
        |> Serialize.variant3 AliasBody typeCodec Serialize.int Serialize.int
        |> Serialize.variant2 AliasIndentEquals Serialize.int Serialize.int
        |> Serialize.variant2 AliasIndentBody Serialize.int Serialize.int
        |> Serialize.finishCustomType
