module Terminal.Main exposing (main)

import Compiler.Elm.Version as V
import Compiler.Reporting.Doc as D
import System.IO as IO
import Task exposing (Task)
import Terminal.Bump as Bump
import Terminal.Diff as Diff
import Terminal.Format as Format
import Terminal.Init as Init
import Terminal.Install as Install
import Terminal.Make as Make
import Terminal.Publish as Publish
import Terminal.Repl as Repl
import Terminal.Terminal as Terminal
import Terminal.Terminal.Chomp as Chomp
import Terminal.Terminal.Helpers as Terminal
import Terminal.Terminal.Internal as Terminal
import Terminal.Test as Test
import Terminal.Uninstall as Uninstall
import Utils.Impure as Impure
import Utils.Task.Extra as Task


main : IO.Program
main =
    IO.run
        (app
            |> Task.bind
                (\() ->
                    Impure.task "exitWith"
                        []
                        (Impure.StringBody "0")
                        Impure.Crash
                )
        )


app : Task Never ()
app =
    Terminal.app intro
        outro
        [ repl
        , init
        , make
        , install
        , uninstall
        , bump
        , diff
        , publish
        , format
        , test
        ]


intro : D.Doc
intro =
    D.vcat
        [ D.fillSep
            [ D.fromChars "Hi,"
            , D.fromChars "thank"
            , D.fromChars "you"
            , D.fromChars "for"
            , D.fromChars "trying"
            , D.fromChars "out"
            , D.green (D.fromChars "Elm")
            , D.green (D.fromChars (V.toChars V.compiler))
                |> D.a (D.fromChars ".")
            , D.fromChars "I hope you like it!"
            ]
        , D.fromChars ""
        , D.black (D.fromChars "-------------------------------------------------------------------------------")
        , D.black (D.fromChars "I highly recommend working through <https://guide.elm-lang.org> to get started.")
        , D.black (D.fromChars "It teaches many important concepts, including how to use `elm` in the terminal.")
        , D.black (D.fromChars "-------------------------------------------------------------------------------")
        ]


outro : D.Doc
outro =
    D.fillSep <|
        (List.map D.fromChars <|
            String.words <|
                "Be sure to ask on the Elm slack if you run into trouble! Folks are friendly and happy to help out. They hang out there because it is fun, so be kind to get the best results!"
        )



-- INIT


init : Terminal.Command
init =
    let
        summary : String
        summary =
            "Start an Elm project. It creates a starter elm.json file and provides a link explaining what to do from there."

        details : String
        details =
            "The `init` command helps start Elm projects:"

        example : D.Doc
        example =
            reflow
                "It will ask permission to create an elm.json file, the one thing common to all Elm projects. It also provides a link explaining what to do from there."

        initFlags : Terminal.Flags
        initFlags =
            Terminal.flags
                |> Terminal.more (Terminal.onOff "package" "Creates a starter elm.json file for a package project.")
                |> Terminal.more (Terminal.onOff "yes" "Reply 'yes' to all automated prompts.")
    in
    Terminal.Command "init" (Terminal.Common summary) details example Terminal.noArgs initFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompExactly (Chomp.pure ())
                ]
                (Chomp.pure Init.Flags
                    |> Chomp.apply (Chomp.chompOnOffFlag "package")
                    |> Chomp.apply (Chomp.chompOnOffFlag "yes")
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags initFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Init.run args flags)



-- REPL


repl : Terminal.Command
repl =
    let
        summary : String
        summary =
            "Open up an interactive programming session. Type in Elm expressions like (2 + 2) or (String.length \"test\") and see if they equal four!"

        details : String
        details =
            "The `repl` command opens up an interactive programming session:"

        example : D.Doc
        example =
            reflow
                "Start working through <https://guide.elm-lang.org> to learn how to use this! It has a whole chapter that uses the REPL for everything, so that is probably the quickest way to get started."

        replFlags : Terminal.Flags
        replFlags =
            Terminal.flags
                |> Terminal.more (Terminal.flag "interpreter" interpreter "Path to a alternate JS interpreter, like node or nodejs.")
                |> Terminal.more (Terminal.onOff "no-colors" "Turn off the colors in the REPL. This can help if you are having trouble reading the values. Some terminals use a custom color scheme that diverges significantly from the standard ANSI colors, so another path may be to pick a more standard color scheme.")
    in
    Terminal.Command "repl" (Terminal.Common summary) details example Terminal.noArgs replFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompExactly (Chomp.pure ())
                ]
                (Chomp.pure Repl.Flags
                    |> Chomp.apply (Chomp.chompNormalFlag "interpreter" interpreter Just)
                    |> Chomp.apply (Chomp.chompOnOffFlag "no-colors")
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags replFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Repl.run args flags)


interpreter : Terminal.Parser
interpreter =
    Terminal.Parser
        { singular = "interpreter"
        , plural = "interpreters"
        , suggest = \_ -> Task.pure []
        , examples = \_ -> Task.pure [ "node", "nodejs" ]
        }



-- MAKE


make : Terminal.Command
make =
    let
        details : String
        details =
            "The `make` command compiles Guida (and Elm) code into JS or HTML:"

        example : D.Doc
        example =
            stack
                [ reflow "For example:"
                , D.indent 4 <| D.green (D.fromChars "guida make src/Main.guida")
                , reflow "This tries to compile an Guida (and Elm) file named src/Main.guida, generating an index.html file if possible."
                ]

        makeFlags : Terminal.Flags
        makeFlags =
            Terminal.flags
                |> Terminal.more (Terminal.onOff "debug" "Turn on the time-travelling debugger. It allows you to rewind and replay events. The events can be imported/exported into a file, which makes for very precise bug reports!")
                |> Terminal.more (Terminal.onOff "optimize" "Turn on optimizations to make code smaller and faster. For example, the compiler renames record fields to be as short as possible and unboxes values to reduce allocation.")
                |> Terminal.more (Terminal.onOff "sourcemaps" "Add source maps to resulting JavaScript code.")
                |> Terminal.more (Terminal.flag "output" Make.output "Specify the name of the resulting JS file. For example --output=assets/guida.js to generate the JS at assets/guida.js or --output=/dev/null to generate no output at all!")
                |> Terminal.more (Terminal.flag "report" Make.reportType "You can say --report=json to get error messages as JSON. This is only really useful if you are an editor plugin. Humans should avoid it!")
                |> Terminal.more (Terminal.flag "docs" Make.docsFile "Generate a JSON file of documentation for a package. Eventually it will be possible to preview docs with `reactor` because it is quite hard to deal with these JSON files directly.")
    in
    Terminal.Command "make" Terminal.Uncommon details example (Terminal.zeroOrMore Terminal.guidaOrElmFile) makeFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompMultiple (Chomp.pure identity) Terminal.guidaOrElmFile Terminal.parseGuidaOrElmFile
                ]
                (Chomp.pure Make.Flags
                    |> Chomp.apply (Chomp.chompOnOffFlag "debug")
                    |> Chomp.apply (Chomp.chompOnOffFlag "optimize")
                    |> Chomp.apply (Chomp.chompOnOffFlag "sourcemaps")
                    |> Chomp.apply (Chomp.chompNormalFlag "output" Make.output Make.parseOutput)
                    |> Chomp.apply (Chomp.chompNormalFlag "report" Make.reportType Make.parseReportType)
                    |> Chomp.apply (Chomp.chompNormalFlag "docs" Make.docsFile Make.parseDocsFile)
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags makeFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Make.run args flags)



-- INSTALL


install : Terminal.Command
install =
    let
        details : String
        details =
            "The `install` command fetches packages from <https://package.elm-lang.org> for use in your project:"

        example : D.Doc
        example =
            stack
                [ reflow
                    "For example, if you want to get packages for HTTP and JSON, you would say:"
                , D.indent 4 <|
                    D.green <|
                        D.vcat <|
                            [ D.fromChars "guida install elm/http"
                            , D.fromChars "guida install elm/json"
                            ]
                , reflow
                    "Notice that you must say the AUTHOR name and PROJECT name! After running those commands, you could say `import Http` or `import Json.Decode` in your code."
                , reflow
                    "What if two projects use different versions of the same package? No problem! Each project is independent, so there cannot be conflicts like that!"
                ]

        installArgs : Terminal.Args
        installArgs =
            Terminal.oneOf
                [ Terminal.require0
                , Terminal.require1 Terminal.package
                ]

        installFlags : Terminal.Flags
        installFlags =
            Terminal.flags
                |> Terminal.more (Terminal.onOff "test" "Install as a test-dependency.")
                |> Terminal.more (Terminal.onOff "yes" "Reply 'yes' to all automated prompts.")
    in
    Terminal.Command "install" Terminal.Uncommon details example installArgs installFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompExactly (Chomp.pure Install.NoArgs)
                , Chomp.chompExactly
                    (Chomp.pure Install.Install
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.package Terminal.parsePackage
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                    )
                ]
                (Chomp.pure Install.Flags
                    |> Chomp.apply (Chomp.chompOnOffFlag "test")
                    |> Chomp.apply (Chomp.chompOnOffFlag "yes")
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags installFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Install.run args flags)



-- UNINSTALL


uninstall : Terminal.Command
uninstall =
    let
        details : String
        details =
            "The `uninstall` command removes packages your project:"

        example : D.Doc
        example =
            stack
                [ reflow
                    "For example, if you want to remove the HTTP and JSON packages, you would say:"
                , D.indent 4 <|
                    D.green <|
                        D.vcat <|
                            [ D.fromChars "guida uninstall elm/http"
                            , D.fromChars "guida uninstall elm/json"
                            ]
                ]

        uninstallArgs : Terminal.Args
        uninstallArgs =
            Terminal.oneOf
                [ Terminal.require0
                , Terminal.require1 Terminal.package
                ]

        uninstallFlags : Terminal.Flags
        uninstallFlags =
            Terminal.flags
                |> Terminal.more (Terminal.onOff "yes" "Reply 'yes' to all automated prompts.")
    in
    Terminal.Command "uninstall" Terminal.Uncommon details example uninstallArgs uninstallFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompExactly (Chomp.pure Uninstall.NoArgs)
                , Chomp.chompExactly
                    (Chomp.pure Uninstall.Uninstall
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.package Terminal.parsePackage
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                    )
                ]
                (Chomp.pure Uninstall.Flags
                    |> Chomp.apply (Chomp.chompOnOffFlag "yes")
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags uninstallFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Uninstall.run args flags)



-- PUBLISH


publish : Terminal.Command
publish =
    let
        details : String
        details =
            "The `publish` command publishes your package on <https://package.elm-lang.org> so that anyone in the Elm community can use it."

        example : D.Doc
        example =
            stack
                [ reflow
                    "Think hard if you are ready to publish NEW packages though!"
                , reflow
                    "Part of what makes Elm great is the packages ecosystem. The fact that there is usually one option (usually very well done) makes it way easier to pick packages and become productive. So having a million packages would be a failure in Elm. We do not need twenty of everything, all coded in a single weekend."
                , reflow
                    "So as community members gain wisdom through experience, we want them to share that through thoughtful API design and excellent documentation. It is more about sharing ideas and insights than just sharing code! The first step may be asking for advice from people you respect, or in community forums. The second step may be using it at work to see if it is as nice as you think. Maybe it ends up as an experiment on GitHub only. Point is, try to be respectful of the community and package ecosystem!"
                , reflow
                    "Check out <https://package.elm-lang.org/help/design-guidelines> for guidance on how to create great packages!"
                ]
    in
    Terminal.Command "publish" Terminal.Uncommon details example Terminal.noArgs Terminal.noFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompExactly (Chomp.pure ())
                ]
                (Chomp.pure ()
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags Terminal.noFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Publish.run args flags)



-- BUMP


bump : Terminal.Command
bump =
    let
        details : String
        details =
            "The `bump` command figures out the next version number based on API changes:"

        example : D.Doc
        example =
            reflow
                "Say you just published version 1.0.0, but then decided to remove a function. I will compare the published API to what you have locally, figure out that it is a MAJOR change, and bump your version number to 2.0.0. I do this with all packages, so there cannot be MAJOR changes hiding in PATCH releases in Elm!"
    in
    Terminal.Command "bump" Terminal.Uncommon details example Terminal.noArgs Terminal.noFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompExactly (Chomp.pure ())
                ]
                (Chomp.pure ()
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags Terminal.noFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Bump.run args flags)



-- DIFF


diff : Terminal.Command
diff =
    let
        details : String
        details =
            "The `diff` command detects API changes:"

        example : D.Doc
        example =
            stack
                [ reflow
                    "For example, to see what changed in the HTML package between versions 1.0.0 and 2.0.0, you can say:"
                , D.indent 4 <| D.green <| D.fromChars "elm diff elm/html 1.0.0 2.0.0"
                , reflow
                    "Sometimes a MAJOR change is not actually very big, so this can help you plan your upgrade timelines."
                ]

        diffArgs : Terminal.Args
        diffArgs =
            Terminal.oneOf
                [ Terminal.require0
                , Terminal.require1 Terminal.version
                , Terminal.require2 Terminal.version Terminal.version
                , Terminal.require3 Terminal.package Terminal.version Terminal.version
                ]
    in
    Terminal.Command "diff" Terminal.Uncommon details example diffArgs Terminal.noFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompExactly (Chomp.pure Diff.CodeVsLatest)
                , Chomp.chompExactly
                    (Chomp.pure Diff.CodeVsExactly
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.version Terminal.parseVersion
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                    )
                , Chomp.chompExactly
                    (Chomp.pure Diff.LocalInquiry
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.version Terminal.parseVersion
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.version Terminal.parseVersion
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                    )
                , Chomp.chompExactly
                    (Chomp.pure Diff.GlobalInquiry
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.package Terminal.parsePackage
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.version Terminal.parseVersion
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                        |> Chomp.bind
                            (\func ->
                                Chomp.chompArg (List.length chunks) Terminal.version Terminal.parseVersion
                                    |> Chomp.fmap (\arg -> func arg)
                            )
                    )
                ]
                (Chomp.pure ()
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags Terminal.noFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Diff.run args flags)



-- FORMAT


format : Terminal.Command
format =
    let
        details : String
        details =
            "The `format` command formats Elm code in place."

        example : D.Doc
        example =
            stack
                [ reflow "For example:"
                , D.indent 4 <| D.green (D.fromChars "guida format src/Main.elm")
                , reflow "This tries to format an Elm file named src/Main.elm, formatting it in place."
                ]

        formatArgs : Terminal.Args
        formatArgs =
            Terminal.zeroOrMore Terminal.filePath

        formatFlags : Terminal.Flags
        formatFlags =
            Terminal.flags
                |> Terminal.more (Terminal.flag "output" output "Write output to FILE instead of overwriting the given source file.")
                |> Terminal.more (Terminal.onOff "yes" "Reply 'yes' to all automated prompts.")
                |> Terminal.more (Terminal.onOff "validate" "Check if files are formatted without changing them.")
                |> Terminal.more (Terminal.onOff "stdin" "Read from stdin, output to stdout.")
    in
    Terminal.Command "format" Terminal.Uncommon details example formatArgs formatFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompMultiple (Chomp.pure identity) Terminal.filePath Terminal.parseFilePath
                ]
                (Chomp.pure Format.Flags
                    |> Chomp.apply (Chomp.chompNormalFlag "output" output Just)
                    |> Chomp.apply (Chomp.chompOnOffFlag "yes")
                    |> Chomp.apply (Chomp.chompOnOffFlag "validate")
                    |> Chomp.apply (Chomp.chompOnOffFlag "stdin")
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags formatFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Format.run args flags)


output : Terminal.Parser
output =
    Terminal.Parser
        { singular = "output"
        , plural = "outputs"
        , suggest = \_ -> Task.pure []
        , examples = \_ -> Task.pure []
        }



-- TEST


test : Terminal.Command
test =
    let
        details : String
        details =
            "The `test` command runs tests."

        example : D.Doc
        example =
            stack
                [ reflow "For example:"
                , D.indent 4 <| D.green (D.fromChars "guida test")
                , reflow "Run tests in the tests/ folder."
                , D.indent 4 <| D.green (D.fromChars "guida test src/Main.guida")
                , reflow "Run tests in files matching the glob."
                ]

        testArgs : Terminal.Args
        testArgs =
            Terminal.zeroOrMore Terminal.filePath

        testFlags : Terminal.Flags
        testFlags =
            Terminal.flags
                |> Terminal.more (Terminal.flag "fuzz" int "Run with a specific fuzzer seed (default: random)")
                |> Terminal.more (Terminal.flag "seed" int "Define how many times each fuzz-test should run (default: 100)")
                |> Terminal.more (Terminal.flag "report" Test.format "Specify which format to use for reporting test results (choices: \"json\", \"junit\", \"console\", default: \"console\")")
    in
    Terminal.Command "test" Terminal.Uncommon details example testArgs testFlags <|
        \chunks ->
            Chomp.chomp Nothing
                chunks
                [ Chomp.chompMultiple (Chomp.pure identity) Terminal.filePath Terminal.parseFilePath
                ]
                (Chomp.pure Test.Flags
                    |> Chomp.apply (Chomp.chompNormalFlag "seed" int parseInt)
                    |> Chomp.apply (Chomp.chompNormalFlag "fuzz" int parseInt)
                    |> Chomp.apply (Chomp.chompNormalFlag "report" Test.format Test.parseReport)
                    |> Chomp.bind
                        (\value ->
                            Chomp.checkForUnknownFlags testFlags
                                |> Chomp.fmap (\_ -> value)
                        )
                )
                |> Tuple.second
                |> Result.map (\( args, flags ) -> Test.run args flags)


int : Terminal.Parser
int =
    Terminal.Parser
        { singular = "int"
        , plural = "ints"
        , suggest = \_ -> Task.pure []
        , examples = \_ -> Task.pure []
        }


parseInt : String -> Maybe Int
parseInt =
    String.toInt



-- HELPERS


stack : List D.Doc -> D.Doc
stack docs =
    D.vcat <| List.intersperse (D.fromChars "") docs


reflow : String -> D.Doc
reflow string =
    D.fillSep <| List.map D.fromChars <| String.words string
