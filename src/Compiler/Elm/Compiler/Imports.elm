module Compiler.Elm.Compiler.Imports exposing (defaults)

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name exposing (CDN_Name)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import System.TypeCheck.IO as IO



-- DEFAULTS


defaults : List Src.CASTS_Import
defaults =
    [ import_ ModuleName.basics Nothing Src.CASTS_Open
    , import_ ModuleName.debug Nothing closed
    , import_ ModuleName.list Nothing (operator "::")
    , import_ ModuleName.maybe Nothing (typeOpen Name.maybe)
    , import_ ModuleName.result Nothing (typeOpen Name.result)
    , import_ ModuleName.string Nothing (typeClosed Name.string)
    , import_ ModuleName.char Nothing (typeClosed Name.char)
    , import_ ModuleName.tuple Nothing closed
    , import_ ModuleName.platform Nothing (typeClosed Name.program)
    , import_ ModuleName.cmd (Just Name.cmd) (typeClosed Name.cmd)
    , import_ ModuleName.sub (Just Name.sub) (typeClosed Name.sub)
    ]


import_ : IO.CEMN_Canonical -> Maybe CDN_Name -> Src.CASTS_Exposing -> Src.CASTS_Import
import_ (IO.CEMN_Canonical _ name) maybeAlias exposing_ =
    Src.CASTS_Import (A.CRA_At A.zero name) maybeAlias exposing_



-- EXPOSING


closed : Src.CASTS_Exposing
closed =
    Src.CASTS_Explicit []


typeOpen : CDN_Name -> Src.CASTS_Exposing
typeOpen name =
    Src.CASTS_Explicit [ Src.CASTS_Upper (A.CRA_At A.zero name) (Src.CASTS_Public A.zero) ]


typeClosed : CDN_Name -> Src.CASTS_Exposing
typeClosed name =
    Src.CASTS_Explicit [ Src.CASTS_Upper (A.CRA_At A.zero name) Src.CASTS_Private ]


operator : CDN_Name -> Src.CASTS_Exposing
operator op =
    Src.CASTS_Explicit [ Src.CASTS_Operator A.zero op ]
