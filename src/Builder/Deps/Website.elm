module Builder.Deps.Website exposing
    ( metadata
    , route
    )

import Builder.Http as Http
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V


domain : String
domain =
    "https://package.elm-lang.org"


route : String -> List ( String, String ) -> String
route path params =
    Http.toUrl (domain ++ path) params


metadata : Pkg.Name -> V.Version -> String -> String
metadata name version file =
    domain ++ "/packages/" ++ Pkg.toUrl name ++ "/" ++ V.toChars version ++ "/" ++ file
