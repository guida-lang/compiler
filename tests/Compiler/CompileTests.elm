module Compiler.CompileTests exposing (suite)

import Builder.Stuff as Stuff
import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.AST.Utils.Binop as Binop
import Compiler.Compile as Compile
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Generate.Target as Target
import Compiler.Guida.Interface as I
import Compiler.Guida.Package as Pkg
import Compiler.Parse.Module as M
import Compiler.Parse.SyntaxVersion as SV
import Compiler.Reporting.Annotation as A
import Data.Map as Dict
import Expect
import System.TypeCheck.IO as IO
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Compiler.Compile"
        [ Test.describe "canonicalize"
            [ Test.test "add" <|
                \_ ->
                    let
                        ifaces =
                            Dict.fromList identity
                                [ ( Name.platform, I.Interface Pkg.core Dict.empty (Dict.singleton identity "Program" (I.ClosedUnion (Can.Union [ "flags", "model", "msg" ] [ Can.Ctor "Program" Index.first 0 [] ] 1 Can.Enum))) Dict.empty Dict.empty )
                                , ( Name.platformCmd, I.Interface Pkg.core Dict.empty (Dict.singleton identity "Cmd" (I.ClosedUnion (Can.Union [ "msg" ] [ Can.Ctor "Cmd" Index.first 0 [] ] 1 Can.Enum))) Dict.empty Dict.empty )
                                , ( Name.platformSub, I.Interface Pkg.core Dict.empty (Dict.singleton identity "Sub" (I.ClosedUnion (Can.Union [ "msg" ] [ Can.Ctor "Sub" Index.first 0 [] ] 1 Can.Enum))) Dict.empty Dict.empty )
                                , ( Name.tuple, I.Interface Pkg.core Dict.empty Dict.empty Dict.empty Dict.empty )
                                , ( Name.char, I.Interface Pkg.core Dict.empty (Dict.singleton identity "Char" (I.ClosedUnion (Can.Union [] [ Can.Ctor "Char" Index.first 0 [] ] 1 Can.Enum))) Dict.empty Dict.empty )
                                , ( Name.string, I.Interface Pkg.core Dict.empty (Dict.singleton identity "String" (I.ClosedUnion (Can.Union [] [ Can.Ctor "String" Index.first 0 [] ] 1 Can.Enum))) Dict.empty Dict.empty )
                                , ( Name.result, I.Interface Pkg.core Dict.empty (Dict.singleton identity "Result" (I.OpenUnion (Can.Union [ "error", "value" ] [ Can.Ctor "Ok" Index.first 1 [ Can.TVar "value" ], Can.Ctor "Err" Index.second 1 [ Can.TVar "error" ] ] 2 Can.Normal))) Dict.empty Dict.empty )
                                , ( Name.maybe, I.Interface Pkg.core Dict.empty (Dict.singleton identity "Maybe" (I.OpenUnion (Can.Union [ "a" ] [ Can.Ctor "Just" Index.first 1 [ Can.TVar "a" ], Can.Ctor "Nothing" Index.second 0 [] ] 2 Can.Normal))) Dict.empty Dict.empty )
                                , ( Name.list, I.Interface Pkg.core Dict.empty Dict.empty Dict.empty (Dict.singleton identity "::" (I.Binop "cons" (Can.Forall (Dict.singleton identity "a" ()) (Can.TLambda (Can.TVar "a") (Can.TLambda (Can.TType (IO.Canonical Pkg.core "List") "List" [ Can.TVar "a" ]) (Can.TType (IO.Canonical Pkg.core "List") "List" [ Can.TVar "a" ])))) Binop.Right 5)) )
                                , ( Name.debug, I.Interface Pkg.core Dict.empty Dict.empty Dict.empty Dict.empty )
                                , ( Name.basics, I.Interface Pkg.core Dict.empty Dict.empty Dict.empty (Dict.singleton identity "++" (I.Binop "append" (Can.Forall (Dict.singleton identity "appendable" ()) (Can.TLambda (Can.TVar "appendable") (Can.TLambda (Can.TVar "appendable") (Can.TVar "appendable")))) Binop.Right 5)) )
                                ]
                    in
                    M.fromByteString Target.GuidaTarget SV.Guida M.Application """module Hello exposing (..)

helloWorld : String -> String -> String
helloWorld a b =
    a ++ b
                    """
                        |> Result.map (Compile.canonicalize Target.GuidaTarget (Stuff.GuidaRoot "") Pkg.dummyName ifaces)
                        |> Expect.equal
                            (Ok
                                ( []
                                , Ok
                                    (Can.Module
                                        (IO.Canonical Pkg.dummyName "Hello")
                                        (Can.ExportEverything (A.Region (A.Position 1 23) (A.Position 1 27)))
                                        (Src.NoDocs (A.Region (A.Position 1 27) (A.Position 3 1)) [])
                                        (Can.Declare
                                            (Can.TypedDef (A.at (A.Position 4 1) (A.Position 4 11) "helloWorld")
                                                Dict.empty
                                                [ ( A.at (A.Position 4 12) (A.Position 4 13) (Can.PVar "a"), Can.TType (IO.Canonical Pkg.core "String") "String" [] )
                                                , ( A.at (A.Position 4 14) (A.Position 4 15) (Can.PVar "b"), Can.TType (IO.Canonical ( "elm", "core" ) "String") "String" [] )
                                                ]
                                                (A.at (A.Position 5 5)
                                                    (A.Position 5 11)
                                                    (Can.Binop "++"
                                                        (IO.Canonical ( "elm", "core" ) "Basics")
                                                        "append"
                                                        (Can.Forall (Dict.singleton identity "appendable" ()) (Can.TLambda (Can.TVar "appendable") (Can.TLambda (Can.TVar "appendable") (Can.TVar "appendable"))))
                                                        (A.at (A.Position 5 5) (A.Position 5 6) (Can.VarLocal "a"))
                                                        (A.at (A.Position 5 10) (A.Position 5 11) (Can.VarLocal "b"))
                                                    )
                                                )
                                                (Can.TType (IO.Canonical Pkg.core "String") "String" [])
                                            )
                                            Can.SaveTheEnvironment
                                        )
                                        Dict.empty
                                        Dict.empty
                                        Dict.empty
                                        Can.NoEffects
                                    )
                                )
                            )
            ]
        ]
