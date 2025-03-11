module Builder.Deps.Website exposing
    ( metadata
    , route
    )

import Builder.Http as Http
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import System.IO as IO exposing (IO)
import Utils.Main as Utils


domain : IO String
domain =
    Utils.envLookupEnv "GUIDA_REGISTRY"
        |> IO.fmap (Maybe.withDefault "https://package.elm-lang.org")


route : String -> List ( String, String ) -> IO String
route path params =
    domain
        |> IO.fmap (\d -> Http.toUrl (d ++ path) params)


metadata : Pkg.Name -> V.Version -> String -> IO String
metadata name version file =
    domain
        |> IO.fmap (\d -> d ++ "/packages/" ++ Pkg.toUrl name ++ "/" ++ V.toChars version ++ "/" ++ file)
