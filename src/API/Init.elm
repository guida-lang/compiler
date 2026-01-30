module API.Init exposing
    ( Flags(..)
    , run
    )

import Basics.Extra exposing (flip)
import Builder.Deps.Registry as Registry
import Builder.Deps.Solver as Solver
import Builder.Deps.Website as Website
import Builder.File as File
import Builder.Guida.Outline as Outline
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.Data.NonEmptyList as NE
import Compiler.Generate.Target as Target
import Compiler.Guida.Constraint as Con
import Compiler.Guida.Licenses as Licenses
import Compiler.Guida.Package as Pkg
import Compiler.Guida.Version as V
import Data.Map as Dict exposing (Dict)
import Task exposing (Task)
import Utils.Main as Utils
import Utils.Task.Extra as Task



-- RUN


type Flags
    = Flags Bool


run : Flags -> Task Never (Result Exit.Init ())
run (Flags package) =
    Utils.dirDoesFileExist "guida.json"
        |> Task.bind
            (\exists ->
                if exists then
                    Task.pure (Err Exit.InitAlreadyExists)

                else
                    init package
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
                                            |> Task.bind (\_ -> File.writeUtf8 "tests/Example.guida" testExample)
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
                                                                    Outline.GuidaPkgOutline
                                                                        Pkg.dummyName
                                                                        Outline.defaultSummary
                                                                        Licenses.bsd3
                                                                        V.one
                                                                        (Outline.ExposedList [])
                                                                        directs
                                                                        testDirects
                                                                        Con.defaultGuida

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
                                                                    Outline.GuidaAppOutline V.compiler
                                                                        (NE.Nonempty (Outline.RelativeSrcDir "src") [])
                                                                        directs
                                                                        indirects
                                                                        testDirects
                                                                        testIndirects
                                                    in
                                                    Outline.write (Stuff.GuidaRoot ".") outline
                                                )
                                            |> Task.fmap (\_ -> Ok ())
            )


verify : Stuff.PackageCache -> Solver.Connection -> Registry.Registry -> Dict ( String, String ) Pkg.Name Con.Constraint -> (Dict ( String, String ) Pkg.Name Solver.Details -> Task Never (Result Exit.Init ())) -> Task Never (Result Exit.Init ())
verify cache connection registry constraints callback =
    Solver.verify Target.GuidaTarget cache connection registry constraints
        |> Task.bind
            (\result ->
                case result of
                    Solver.SolverErr exit ->
                        Task.pure (Err (Exit.InitSolverProblem exit))

                    Solver.NoSolution ->
                        Task.pure (Err (Exit.InitNoSolution (Dict.keys compare constraints)))

                    Solver.NoOfflineSolution ->
                        Task.io Website.domain
                            |> Task.fmap
                                (\registryDomain ->
                                    Err (Exit.InitNoOfflineSolution registryDomain (Dict.keys compare constraints))
                                )

                    Solver.SolverOk details ->
                        callback details
            )


defaults : Dict ( String, String ) Pkg.Name Con.Constraint
defaults =
    Dict.fromList identity
        [ ( Pkg.stdlib, Con.anything )
        ]


testDefaults : Dict ( String, String ) Pkg.Name Con.Constraint
testDefaults =
    Dict.empty


packageDefaults : Dict ( String, String ) Pkg.Name Con.Constraint
packageDefaults =
    Dict.fromList identity
        [ ( Pkg.stdlib, Con.anything )
        ]


packageTestDefaults : Dict ( String, String ) Pkg.Name Con.Constraint
packageTestDefaults =
    Dict.empty


testExample : String
testExample =
    """module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    todo "Implement our first test. See https://guida-lang.org/docs/1.0.0/commands/test for how to do this!"
"""
