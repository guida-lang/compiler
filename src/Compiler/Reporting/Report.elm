module Compiler.Reporting.Report exposing (Report(..))

import Compiler.Reporting.Doc as D
import Types as T



-- BUILD REPORTS


type Report
    = Report String T.CRA_Region (List String) D.Doc
