module Builder.Deps.Website exposing
    ( metadata
    , route
    )

import Builder.Http as Http
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Task exposing (Task)
import Utils.Main as Utils
import Utils.Task.Extra as Task


domain : Task Never String
domain =
    Utils.envLookupEnv "GUIDA_REGISTRY"
        |> Task.fmap (Maybe.withDefault "https://package.elm-lang.org")


route : String -> List ( String, String ) -> Task Never String
route path params =
    domain
        |> Task.fmap (\d -> Http.toUrl (d ++ path) params)


metadata : Pkg.Name -> V.Version -> String -> Task Never String
metadata name version file =
    domain
        |> Task.fmap (\d -> d ++ "/packages/" ++ Pkg.toUrl name ++ "/" ++ V.toChars version ++ "/" ++ file)
