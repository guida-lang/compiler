module Terminal.Init exposing
    ( Flags(..)
    , run
    )

import Builder.Deps.Solver as Solver
import Builder.Elm.Outline as Outline
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Constraint as Con
import Compiler.Elm.Licenses as Licenses
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Reporting.Doc as D
import Data.Map as Dict exposing (Dict)
import System.IO as IO exposing (IO)
import Utils.Main as Utils



-- RUN


type Flags
    = Flags Bool


run : () -> Flags -> IO ()
run () (Flags package) =
    Reporting.attempt Exit.initToReport <|
        (Utils.dirDoesFileExist "elm.json"
            |> IO.bind
                (\exists ->
                    if exists then
                        IO.pure (Err Exit.InitAlreadyExists)

                    else
                        Reporting.ask question
                            |> IO.bind
                                (\approved ->
                                    if approved then
                                        init package

                                    else
                                        IO.putStrLn "Okay, I did not make any changes!"
                                            |> IO.fmap (\_ -> Ok ())
                                )
                )
        )


question : D.Doc
question =
    D.stack
        [ D.fillSep
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
        , D.reflow "Now you may be wondering, what will be in this file? How do I add Elm files to my project? How do I see it in the browser? How will my code grow? Do I need more directories? What about tests? Etc."
        , D.fillSep
            [ D.fromChars "Check"
            , D.fromChars "out"
            , D.cyan (D.fromChars (D.makeLink "init"))
            , D.fromChars "for"
            , D.fromChars "all"
            , D.fromChars "the"
            , D.fromChars "answers!"
            ]
        , D.fromChars "Knowing all that, would you like me to create an elm.json file now? [Y/n]: "
        ]



-- INIT


init : Bool -> IO (Result Exit.Init ())
init package =
    Solver.initEnv
        |> IO.bind
            (\eitherEnv ->
                case eitherEnv of
                    Err problem ->
                        IO.pure (Err (Exit.InitRegistryProblem problem))

                    Ok (Solver.Env cache _ connection registry) ->
                        Solver.verify cache connection registry defaults
                            |> IO.bind
                                (\result ->
                                    case result of
                                        Solver.SolverErr exit ->
                                            IO.pure (Err (Exit.InitSolverProblem exit))

                                        Solver.NoSolution ->
                                            IO.pure (Err (Exit.InitNoSolution (Dict.keys compare defaults)))

                                        Solver.NoOfflineSolution ->
                                            IO.pure (Err (Exit.InitNoOfflineSolution (Dict.keys compare defaults)))

                                        Solver.SolverOk details ->
                                            Utils.dirCreateDirectoryIfMissing True "src"
                                                |> IO.bind
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
                                                                    in
                                                                    Outline.Pkg <|
                                                                        Outline.PkgOutline ( "author", "project" )
                                                                            "Update this with a brief description before publishing."
                                                                            Licenses.bsd3
                                                                            V.one
                                                                            (Outline.ExposedList [])
                                                                            directs
                                                                            Dict.empty
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
                                                                    in
                                                                    Outline.App <|
                                                                        Outline.AppOutline V.elmCompiler
                                                                            (NE.Nonempty (Outline.RelativeSrcDir "src") [])
                                                                            directs
                                                                            indirects
                                                                            Dict.empty
                                                                            Dict.empty
                                                        in
                                                        Outline.write "." outline
                                                    )
                                                |> IO.bind (\_ -> IO.putStrLn "Okay, I created it. Now read that link!")
                                                |> IO.fmap (\_ -> Ok ())
                                )
            )


defaults : Dict ( String, String ) Pkg.Name Con.Constraint
defaults =
    Dict.fromList identity
        [ ( Pkg.core, Con.anything )
        , ( Pkg.browser, Con.anything )
        , ( Pkg.html, Con.anything )
        ]


packageDefaults : Dict ( String, String ) Pkg.Name Con.Constraint
packageDefaults =
    Dict.fromList identity
        [ ( Pkg.core, Con.anything )
        ]
