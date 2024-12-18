module Compiler.Compile exposing
    ( Artifacts(..)
    , compile
    )

import Compiler.AST.Canonical as Can
import Compiler.Canonicalize.Module as Canonicalize
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
import Types as T



-- COMPILE


type Artifacts
    = Artifacts Can.Module (Dict String T.CDN_Name T.CASTC_Annotation) T.CASTO_LocalGraph


compile : T.CEP_Name -> Dict String T.CEMN_Raw T.CEI_Interface -> T.CASTS_Module -> IO (Result E.CRE_Error Artifacts)
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


canonicalize : T.CEP_Name -> Dict String T.CEMN_Raw T.CEI_Interface -> T.CASTS_Module -> Result E.CRE_Error Can.Module
canonicalize pkg ifaces modul =
    case Tuple.second (R.run (Canonicalize.canonicalize pkg ifaces modul)) of
        Ok canonical ->
            Ok canonical

        Err errors ->
            Err (E.CRE_BadNames errors)


typeCheck : T.CASTS_Module -> Can.Module -> Result E.CRE_Error (Dict String T.CDN_Name T.CASTC_Annotation)
typeCheck modul canonical =
    case TypeCheck.unsafePerformIO (TypeCheck.bind Type.run (Type.constrain canonical)) of
        Ok annotations ->
            Ok annotations

        Err errors ->
            Err (E.CRE_BadTypes (Localizer.fromModule modul) errors)


nitpick : Can.Module -> Result E.CRE_Error ()
nitpick canonical =
    case PatternMatches.check canonical of
        Ok () ->
            Ok ()

        Err errors ->
            Err (E.CRE_BadPatterns errors)


optimize : T.CASTS_Module -> Dict String T.CDN_Name T.CASTC_Annotation -> Can.Module -> Result E.CRE_Error T.CASTO_LocalGraph
optimize modul annotations canonical =
    case Tuple.second (R.run (Optimize.optimize annotations canonical)) of
        Ok localGraph ->
            Ok localGraph

        Err errors ->
            Err (E.CRE_BadMains (Localizer.fromModule modul) errors)
