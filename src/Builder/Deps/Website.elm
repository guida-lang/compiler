module Builder.Deps.Website exposing
    ( domain
    , metadata
    , route
    )

import Builder.Http as Http
import Compiler.Guida.Package as Pkg
import Compiler.Guida.Version as V
import Task exposing (Task)
import Utils.Main as Utils
import Utils.Task.Extra as Task


domain : Task Never String
domain =
    Utils.envLookupEnv "GUIDA_REGISTRY"
        |> Task.fmap (Maybe.withDefault "https://package.guida-lang.org")


route : String -> List ( String, String ) -> Task Never String
route path params =
    domain
        |> Task.fmap (\d -> Http.toUrl (d ++ path) params)


metadata : Pkg.Name -> V.Version -> String -> Task Never String
metadata name version file =
    domain
        |> Task.fmap (\d -> d ++ "/packages/" ++ Pkg.toUrl name ++ "/" ++ V.toChars version ++ "/" ++ file)
