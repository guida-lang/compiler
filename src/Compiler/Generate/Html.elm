module Compiler.Generate.Html exposing
    ( leadingLines
    , sandwich
    )

import Compiler.Data.Name exposing (Name)
import Compiler.Generate.Target as Target exposing (Target)


leadingLines : Int
leadingLines =
    2


sandwich : Target -> Name -> String -> String
sandwich target moduleName javascript =
    let
        ( id, namespace ) =
            case target of
                Target.GuidaTarget ->
                    ( "guida", "Guida" )

                Target.ElmTarget ->
                    ( "elm", "Elm" )
    in
    """<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <title>""" ++ moduleName ++ """</title>
  <style>body { padding: 0; margin: 0; }</style>
</head>

<body>

<pre id=\"""" ++ id ++ """"></pre>

<script>
try {
""" ++ javascript ++ """

  var app = """ ++ namespace ++ "." ++ moduleName ++ """.init({ node: document.getElementById(\"""" ++ id ++ """") });
}
catch (e)
{
  // display initialization errors (e.g. bad flags, infinite recursion)
  var header = document.createElement("h1");
  header.style.fontFamily = "monospace";
  header.innerText = "Initialization Error";
  var pre = document.getElementById(\"""" ++ id ++ """");
  document.body.insertBefore(header, pre);
  pre.innerText = e;
  throw e;
}
</script>

</body>
</html>"""
