module Compiler.Compile exposing
    ( Artifacts(..)
    , compile
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Module as Canonicalize
import Compiler.Data.Name as Name
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
import System.IO as IO exposing (IO)
import System.TypeCheck.IO as TypeCheck



-- COMPILE


type Artifacts
    = Artifacts Can.Module (Dict String Name.CDN_Name Can.CASTC_Annotation) Opt.LocalGraph


compile : Pkg.CEP_Name -> Dict String ModuleName.CEMN_Raw I.CEI_Interface -> Src.CASTS_Module -> IO (Result E.Error Artifacts)
compile pkg ifaces modul =
    IO.pure (canonicalize pkg ifaces modul)
        |> IO.fmap
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


canonicalize : Pkg.CEP_Name -> Dict String ModuleName.CEMN_Raw I.CEI_Interface -> Src.CASTS_Module -> Result E.Error Can.Module
canonicalize pkg ifaces modul =
    case Tuple.second (R.run (Canonicalize.canonicalize pkg ifaces modul)) of
        Ok canonical ->
            Ok canonical

        Err errors ->
            Err (E.BadNames errors)


typeCheck : Src.CASTS_Module -> Can.Module -> Result E.Error (Dict String Name.CDN_Name Can.CASTC_Annotation)
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


optimize : Src.CASTS_Module -> Dict String Name.CDN_Name Can.CASTC_Annotation -> Can.Module -> Result E.Error Opt.LocalGraph
optimize modul annotations canonical =
    case Tuple.second (R.run (Optimize.optimize annotations canonical)) of
        Ok localGraph ->
            Ok localGraph

        Err errors ->
            Err (E.BadMains (Localizer.fromModule modul) errors)
