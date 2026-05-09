module Compiler.Reporting.Report exposing
    ( Report(..)
    , WarningModuleReport
    )

import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D



-- BUILD REPORTS


type alias WarningModuleReport =
    { path : String
    , name : String
    , warnings : List Report
    }


type Report
    = Report String A.Region (List String) D.Doc
