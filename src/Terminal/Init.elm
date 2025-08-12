module Terminal.Init exposing
    ( Flags(..)
    , run
    )

import Basics.Extra exposing (flip)
import Builder.Deps.Registry as Registry
import Builder.Deps.Solver as Solver
import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Exit.Help as Help
import Builder.Stuff as Stuff
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Constraint as Con
import Compiler.Elm.Licenses as Licenses
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Reporting.Doc as D
import Data.Map as Dict exposing (Dict)
import System.IO as IO
import Task exposing (Task)
import Utils.Main as Utils
import Utils.Task.Extra as Task



-- RUN


type Flags
    = Flags Bool Bool


run : () -> Flags -> Task Never ()
run () (Flags package autoYes) =
    Reporting.attempt Exit.initToReport <|
        (Utils.dirDoesFileExist "elm.json"
            |> Task.bind
                (\exists ->
                    if exists then
                        Task.pure (Err Exit.InitAlreadyExists)

                    else
                        let
                            askQuestion : Task Never Bool
                            askQuestion =
                                if autoYes then
                                    Help.toStdout (information [ D.fromChars "" ])
                                        |> Task.fmap (\_ -> True)

                                else
                                    Reporting.ask
                                        (information
                                            [ D.fromChars "Knowing all that, would you like me to create an elm.json file now? [Y/n]: "
                                            ]
                                        )
                        in
                        askQuestion
                            |> Task.bind
                                (\approved ->
                                    if approved then
                                        init package

                                    else
                                        IO.putStrLn "Okay, I did not make any changes!"
                                            |> Task.fmap (\_ -> Ok ())
                                )
                )
        )


information : List D.Doc -> D.Doc
information question =
    D.stack
        (D.fillSep
            [ D.fromChars "Hello!"
            , D.fromChars "Elm"
            , D.fromChars "projects"
            , D.fromChars "always"
            , D.fromChars "start"
            , D.fromChars "with"
            , D.fromChars "an"
            , D.green (D.fromChars "elm.json")
            , D.fromChars "file."
            , D.fromChars "I"
            , D.fromChars "can"
            , D.fromChars "create"
            , D.fromChars "them!"
            ]
            :: D.reflow "Now you may be wondering, what will be in this file? How do I add Elm files to my project? How do I see it in the browser? How will my code grow? Do I need more directories? What about tests? Etc."
            :: D.fillSep
                [ D.fromChars "Check"
                , D.fromChars "out"
                , D.cyan (D.fromChars (D.makeLink "init"))
                , D.fromChars "for"
                , D.fromChars "all"
                , D.fromChars "the"
                , D.fromChars "answers!"
                ]
            :: question
        )



-- INIT


init : Bool -> Task Never (Result Exit.Init ())
init package =
    Solver.initEnv
        |> Task.bind
            (\eitherEnv ->
                case eitherEnv of
                    Err problem ->
                        Task.pure (Err (Exit.InitRegistryProblem problem))

                    Ok (Solver.Env cache _ connection registry) ->
                        verify cache connection registry defaults <|
                            \details ->
                                verify cache connection registry testDefaults <|
                                    \testDetails ->
                                        Utils.dirCreateDirectoryIfMissing True "src"
                                            |> Task.bind (\_ -> Utils.dirCreateDirectoryIfMissing True "tests")
                                            |> Task.bind (\_ -> File.writeUtf8 "tests/Example.elm" testExample)
                                            |> Task.bind
                                                (\_ ->
                                                    let
                                                        outline : Outline.Outline
                                                        outline =
                                                            if package then
                                                                let
                                                                    directs : Dict ( String, String ) Pkg.Name Con.Constraint
                                                                    directs =
                                                                        Dict.map
                                                                            (\pkg _ ->
                                                                                let
                                                                                    (Solver.Details vsn _) =
                                                                                        Utils.find identity pkg details
                                                                                in
                                                                                Con.untilNextMajor vsn
                                                                            )
                                                                            packageDefaults

                                                                    testDirects : Dict ( String, String ) Pkg.Name Con.Constraint
                                                                    testDirects =
                                                                        Dict.map
                                                                            (\pkg _ ->
                                                                                let
                                                                                    (Solver.Details vsn _) =
                                                                                        Utils.find identity pkg testDetails
                                                                                in
                                                                                Con.untilNextMajor vsn
                                                                            )
                                                                            packageTestDefaults
                                                                in
                                                                Outline.Pkg <|
                                                                    Outline.PkgOutline
                                                                        Pkg.dummyName
                                                                        Outline.defaultSummary
                                                                        Licenses.bsd3
                                                                        V.one
                                                                        (Outline.ExposedList [])
                                                                        directs
                                                                        testDirects
                                                                        Con.defaultElm

                                                            else
                                                                let
                                                                    solution : Dict ( String, String ) Pkg.Name V.Version
                                                                    solution =
                                                                        Dict.map (\_ (Solver.Details vsn _) -> vsn) details

                                                                    directs : Dict ( String, String ) Pkg.Name V.Version
                                                                    directs =
                                                                        Dict.intersection compare solution defaults

                                                                    indirects : Dict ( String, String ) Pkg.Name V.Version
                                                                    indirects =
                                                                        Dict.diff solution defaults

                                                                    testSolution : Dict ( String, String ) Pkg.Name V.Version
                                                                    testSolution =
                                                                        Dict.map (\_ (Solver.Details vsn _) -> vsn) testDetails

                                                                    testDirects : Dict ( String, String ) Pkg.Name V.Version
                                                                    testDirects =
                                                                        Dict.intersection compare testSolution testDefaults

                                                                    testIndirects : Dict ( String, String ) Pkg.Name V.Version
                                                                    testIndirects =
                                                                        Dict.diff testSolution testDefaults
                                                                            |> flip Dict.diff directs
                                                                            |> flip Dict.diff indirects
                                                                in
                                                                Outline.App <|
                                                                    Outline.AppOutline V.elmCompiler
                                                                        (NE.Nonempty (Outline.RelativeSrcDir "src") [])
                                                                        directs
                                                                        indirects
                                                                        testDirects
                                                                        testIndirects
                                                    in
                                                    Outline.write "." outline
                                                )
                                            |> Task.bind (\_ -> IO.putStrLn "Okay, I created it. Now read that link!")
                                            |> Task.fmap (\_ -> Ok ())
            )


verify : Stuff.PackageCache -> Solver.Connection -> Registry.Registry -> Dict ( String, String ) Pkg.Name Con.Constraint -> (Dict ( String, String ) Pkg.Name Solver.Details -> Task Never (Result Exit.Init ())) -> Task Never (Result Exit.Init ())
verify cache connection registry constraints callback =
    Solver.verify cache connection registry constraints
        |> Task.bind
            (\result ->
                case result of
                    Solver.SolverErr exit ->
                        Task.pure (Err (Exit.InitSolverProblem exit))

                    Solver.NoSolution ->
                        Task.pure (Err (Exit.InitNoSolution (Dict.keys compare constraints)))

                    Solver.NoOfflineSolution ->
                        Task.pure (Err (Exit.InitNoOfflineSolution (Dict.keys compare constraints)))

                    Solver.SolverOk details ->
                        callback details
            )


defaults : Dict ( String, String ) Pkg.Name Con.Constraint
defaults =
    Dict.fromList identity
        [ ( Pkg.core, Con.anything )
        , ( Pkg.browser, Con.anything )
        , ( Pkg.html, Con.anything )
        ]


testDefaults : Dict ( String, String ) Pkg.Name Con.Constraint
testDefaults =
    Dict.fromList identity
        [ ( Pkg.test, Con.anything )
        ]


packageDefaults : Dict ( String, String ) Pkg.Name Con.Constraint
packageDefaults =
    Dict.fromList identity
        [ ( Pkg.core, Con.anything )
        ]


packageTestDefaults : Dict ( String, String ) Pkg.Name Con.Constraint
packageTestDefaults =
    Dict.fromList identity
        [ ( Pkg.test, Con.anything )
        ]


testExample : String
testExample =
    """module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
"""
