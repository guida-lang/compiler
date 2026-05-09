module Compiler.Compile exposing
    ( Artifacts(..)
    , compile
    )

import Builder.Stuff as Stuff
import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Module as Canonicalize
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.Interface as I
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Guida.Package as Pkg
import Compiler.Nitpick.PatternMatches as PatternMatches
import Compiler.Optimize.Module as Optimize
import Compiler.Reporting.Error as E
import Compiler.Reporting.Render.Type.Localizer as Localizer
import Compiler.Reporting.Result as R
import Compiler.Reporting.Warning as W
import Compiler.Type.Constrain.Module as Type
import Compiler.Type.Solve as Type
import Data.Map exposing (Dict)
import Data.Set as EverySet
import System.TypeCheck.IO as TypeCheck
import Task exposing (Task)
import Utils.Task.Extra as Task



-- COMPILE


type Artifacts
    = Artifacts Can.Module (Dict String Name Can.Annotation) Opt.LocalGraph


compile : Target -> Stuff.Root -> Pkg.Name -> Dict String ModuleName.Raw I.Interface -> Src.Module -> Task Never ( List W.Warning, Result E.Error Artifacts )
compile target root pkg ifaces modul =
    let
        ( canonicalWarnings, canonicalResult ) =
            canonicalize target root pkg ifaces modul
    in
    case canonicalResult of
        Ok canonical ->
            case
                Result.map2 (\annotations () -> annotations)
                    (typeCheck target modul canonical)
                    (nitpick target canonical)
            of
                Ok annotations ->
                    let
                        ( optWarnings, optResult ) =
                            optimize target modul annotations canonical
                    in
                    Task.pure
                        ( canonicalWarnings ++ optWarnings
                        , Result.map (\objects -> Artifacts canonical annotations objects) optResult
                        )

                Err err ->
                    Task.pure ( canonicalWarnings, Err err )

        Err err ->
            Task.pure ( canonicalWarnings, Err err )



-- PHASES


canonicalize : Target -> Stuff.Root -> Pkg.Name -> Dict String ModuleName.Raw I.Interface -> Src.Module -> ( List W.Warning, Result E.Error Can.Module )
canonicalize target root pkg ifaces modul =
    case R.runWithInfo EverySet.empty (Canonicalize.canonicalize target root pkg ifaces modul) of
        ( _, warnings, Ok canonical ) ->
            ( warnings, Ok canonical )

        ( _, warnings, Err errors ) ->
            ( warnings, Err (E.BadNames errors) )


typeCheck : Target -> Src.Module -> Can.Module -> Result E.Error (Dict String Name Can.Annotation)
typeCheck target modul canonical =
    case TypeCheck.unsafePerformIO (TypeCheck.bind (Type.run target) (Type.constrain target canonical)) of
        Ok annotations ->
            Ok annotations

        Err errors ->
            Err (E.BadTypes (Localizer.fromModule modul) errors)


nitpick : Target -> Can.Module -> Result E.Error ()
nitpick target canonical =
    case PatternMatches.check target canonical of
        Ok () ->
            Ok ()

        Err errors ->
            Err (E.BadPatterns errors)


optimize : Target -> Src.Module -> Dict String Name.Name Can.Annotation -> Can.Module -> ( List W.Warning, Result E.Error Opt.LocalGraph )
optimize target ((Src.Module syntaxVersion _ _ _ _ _ _ _ _ _) as modul) annotations canonical =
    case R.run (Optimize.optimize target syntaxVersion annotations canonical) of
        ( warnings, Ok localGraph ) ->
            ( warnings, Ok localGraph )

        ( warnings, Err errors ) ->
            ( warnings, Err (E.BadMains (Localizer.fromModule modul) errors) )
