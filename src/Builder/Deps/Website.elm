module Builder.Deps.Website exposing
    ( domain
    , metadata
    , route
    )

import Builder.Http as Http
import Compiler.Guida.Package as Pkg
import Compiler.Guida.Version as V
import System.Environment as Env
import Task exposing (Task)
import Utils.Main as Utils
import Utils.Task.Extra as Task


domain : Task Never String
domain =
    Env.lookupEnv "GUIDA_REGISTRY"
        |> Task.map (Maybe.withDefault "https://package.guida-lang.org")


route : String -> List ( String, String ) -> Task Never String
route path params =
    domain
        |> Task.map (\d -> Http.toUrl (d ++ path) params)


metadata : Pkg.Name -> V.Version -> String -> Task Never String
metadata name version file =
    domain
        |> Task.map (\d -> d ++ "/packages/" ++ Pkg.toUrl name ++ "/" ++ V.toChars version ++ "/" ++ file)
