module Compiler.Compile exposing
    ( Artifacts(..)
    , compile
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Module as Canonicalize
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Elm.Interface as I
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Nitpick.PatternMatches as PatternMatches
import Compiler.Optimize.Module as Optimize
import Compiler.Reporting.Error as E
import Compiler.Reporting.Render.Type.Localizer as Localizer
import Compiler.Reporting.Result as R
import Compiler.Type.Constrain.Module as Type
import Compiler.Type.Solve as Type
import Data.Map exposing (Dict)
import System.TypeCheck.IO as TypeCheck
import Task exposing (Task)
import Utils.Task.Extra as Task



-- COMPILE


type Artifacts
    = Artifacts Can.Module (Dict String Name Can.Annotation) Opt.LocalGraph


compile : Pkg.Name -> Dict String ModuleName.Raw I.Interface -> Src.Module -> Task Never (Result E.Error Artifacts)
compile pkg ifaces modul =
    Task.pure (canonicalize pkg ifaces modul)
        |> Task.fmap
            (\canonicalResult ->
                case canonicalResult of
                    Ok canonical ->
                        Result.map2 (\annotations () -> annotations)
                            (typeCheck modul canonical)
                            (nitpick canonical)
                            |> Result.andThen
                                (\annotations ->
                                    optimize modul annotations canonical
                                        |> Result.map (\objects -> Artifacts canonical annotations objects)
                                )

                    Err err ->
                        Err err
            )



-- PHASES


canonicalize : Pkg.Name -> Dict String ModuleName.Raw I.Interface -> Src.Module -> Result E.Error Can.Module
canonicalize pkg ifaces modul =
    case Tuple.second (R.run (Canonicalize.canonicalize pkg ifaces modul)) of
        Ok canonical ->
            Ok canonical

        Err errors ->
            Err (E.BadNames errors)


typeCheck : Src.Module -> Can.Module -> Result E.Error (Dict String Name Can.Annotation)
typeCheck modul canonical =
    case TypeCheck.unsafePerformIO (TypeCheck.bind Type.run (Type.constrain canonical)) of
        Ok annotations ->
            Ok annotations

        Err errors ->
            Err (E.BadTypes (Localizer.fromModule modul) errors)


nitpick : Can.Module -> Result E.Error ()
nitpick canonical =
    case PatternMatches.check canonical of
        Ok () ->
            Ok ()

        Err errors ->
            Err (E.BadPatterns errors)


optimize : Src.Module -> Dict String Name.Name Can.Annotation -> Can.Module -> Result E.Error Opt.LocalGraph
optimize modul annotations canonical =
    case Tuple.second (R.run (Optimize.optimize annotations canonical)) of
        Ok localGraph ->
            Ok localGraph

        Err errors ->
            Err (E.BadMains (Localizer.fromModule modul) errors)
