module Compiler.Reporting.Report exposing (Report(..))

import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D



-- BUILD REPORTS


type Report
    = Report String A.CRA_Region (List String) D.Doc
