module Terminal.Init exposing (run)

import Builder.Deps.Solver as Solver
import Builder.Elm.Outline as Outline
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Constraint as Con
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Reporting.Doc as D
import Data.Map as Dict exposing (Dict)
import System.IO as IO exposing (IO)
import Types as T
import Utils.Main as Utils



-- RUN


run : () -> () -> IO ()
run () () =
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
                                        init

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


init : IO (Result Exit.Init ())
init =
    Solver.initEnv
        |> IO.bind
            (\eitherEnv ->
                case eitherEnv of
                    Err problem ->
                        IO.pure (Err (Exit.InitRegistryProblem problem))

                    Ok (T.BDS_Env cache _ connection registry) ->
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
                                            let
                                                solution : Dict ( String, String ) T.CEP_Name T.CEV_Version
                                                solution =
                                                    Dict.map (\_ (Solver.Details vsn _) -> vsn) details

                                                directs : Dict ( String, String ) T.CEP_Name T.CEV_Version
                                                directs =
                                                    Dict.intersection compare solution defaults

                                                indirects : Dict ( String, String ) T.CEP_Name T.CEV_Version
                                                indirects =
                                                    Dict.diff solution defaults
                                            in
                                            Utils.dirCreateDirectoryIfMissing True "src"
                                                |> IO.bind
                                                    (\_ ->
                                                        Outline.write "." <|
                                                            Outline.App <|
                                                                Outline.AppOutline V.compiler (NE.Nonempty (Outline.RelativeSrcDir "src") []) directs indirects Dict.empty Dict.empty
                                                    )
                                                |> IO.bind (\_ -> IO.putStrLn "Okay, I created it. Now read that link!")
                                                |> IO.fmap (\_ -> Ok ())
                                )
            )


defaults : Dict ( String, String ) T.CEP_Name Con.Constraint
defaults =
    Dict.fromList identity
        [ ( Pkg.core, Con.anything )
        , ( Pkg.browser, Con.anything )
        , ( Pkg.html, Con.anything )
        ]
