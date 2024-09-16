module Deps.Bump exposing (getPossibilities)

import Deps.Registry exposing (KnownVersions(..))
import Elm.Magnitude as M
import Elm.Version as V
import List.Extra
import Utils.Main as Utils



-- GET POSSIBILITIES


getPossibilities : KnownVersions -> List ( V.Version, V.Version, M.Magnitude )
getPossibilities (KnownVersions latest previous) =
    let
        allVersions =
            List.reverse (latest :: previous)

        minorPoints =
            List.filterMap List.Extra.last (Utils.listGroupBy sameMajor allVersions)

        patchPoints =
            List.filterMap List.Extra.last (Utils.listGroupBy sameMinor allVersions)
    in
    ( latest, V.bumpMajor latest, M.MAJOR )
        :: List.map (\v -> ( v, V.bumpMinor v, M.MINOR )) minorPoints
        ++ List.map (\v -> ( v, V.bumpPatch v, M.PATCH )) patchPoints


sameMajor : V.Version -> V.Version -> Bool
sameMajor (V.Version major1 _ _) (V.Version major2 _ _) =
    major1 == major2


sameMinor : V.Version -> V.Version -> Bool
sameMinor (V.Version major1 minor1 _) (V.Version major2 minor2 _) =
    major1 == major2 && minor1 == minor2
