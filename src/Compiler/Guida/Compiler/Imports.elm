module Compiler.Guida.Compiler.Imports exposing (defaults)

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import System.TypeCheck.IO as IO



-- DEFAULTS


defaults : Target -> List (Src.C1 Src.Import)
defaults target =
    [ ( [], import_ (ModuleName.basics target) Nothing (Src.Open [] []) )
    , ( [], import_ (ModuleName.debug target) Nothing closed )
    , ( [], import_ (ModuleName.list target) Nothing (operator "::") )
    , ( [], import_ (ModuleName.maybe target) Nothing (typeOpen Name.maybe) )
    , ( [], import_ (ModuleName.result target) Nothing (typeOpen Name.result) )
    , ( [], import_ (ModuleName.string target) Nothing (typeClosed Name.string) )
    , ( [], import_ (ModuleName.char target) Nothing (typeClosed Name.char) )
    , ( [], import_ (ModuleName.tuple target) Nothing closed )
    , ( [], import_ (ModuleName.platform target) Nothing (typeClosed Name.program) )
    , ( [], import_ (ModuleName.cmd target) (Just Name.cmd) (typeClosed Name.cmd) )
    , ( [], import_ (ModuleName.sub target) (Just Name.sub) (typeClosed Name.sub) )
    ]


import_ : IO.Canonical -> Maybe Name -> Src.Exposing -> Src.Import
import_ (IO.Canonical _ name) maybeAlias exposing_ =
    Src.Import ( [], A.At A.zero name ) (Maybe.map (\alias_ -> ( ( [], [] ), alias_ )) maybeAlias) ( ( [], [] ), exposing_ )



-- EXPOSING


closed : Src.Exposing
closed =
    Src.Explicit (A.At A.zero [])


typeOpen : Name -> Src.Exposing
typeOpen name =
    Src.Explicit (A.At A.zero [ ( ( [], [] ), Src.Upper (A.At A.zero name) ( [], Src.Public A.zero ) ) ])


typeClosed : Name -> Src.Exposing
typeClosed name =
    Src.Explicit (A.At A.zero [ ( ( [], [] ), Src.Upper (A.At A.zero name) ( [], Src.Private ) ) ])


operator : Name -> Src.Exposing
operator op =
    Src.Explicit (A.At A.zero [ ( ( [], [] ), Src.Operator A.zero op ) ])
