module Compiler.Elm.Compiler.Imports exposing (defaults)

import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Types as T



-- DEFAULTS


defaults : List T.CASTS_Import
defaults =
    [ import_ ModuleName.basics Nothing T.CASTS_Open
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


import_ : T.CEMN_Canonical -> Maybe T.CDN_Name -> T.CASTS_Exposing -> T.CASTS_Import
import_ (T.CEMN_Canonical _ name) maybeAlias exposing_ =
    T.CASTS_Import (T.CRA_At A.zero name) maybeAlias exposing_



-- EXPOSING


closed : T.CASTS_Exposing
closed =
    T.CASTS_Explicit []


typeOpen : T.CDN_Name -> T.CASTS_Exposing
typeOpen name =
    T.CASTS_Explicit [ T.CASTS_Upper (T.CRA_At A.zero name) (T.CASTS_Public A.zero) ]


typeClosed : T.CDN_Name -> T.CASTS_Exposing
typeClosed name =
    T.CASTS_Explicit [ T.CASTS_Upper (T.CRA_At A.zero name) T.CASTS_Private ]


operator : T.CDN_Name -> T.CASTS_Exposing
operator op =
    T.CASTS_Explicit [ T.CASTS_Operator A.zero op ]
