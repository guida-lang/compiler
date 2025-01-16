module Compiler.Generate.Html exposing
    ( leadingLines
    , sandwich
    )

import Compiler.Data.Name exposing (Name)


leadingLines : Int
leadingLines =
    2


sandwich : Name -> String -> String
sandwich moduleName javascript =
    """<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <title>""" ++ moduleName ++ """</title>
  <style>body { padding: 0; margin: 0; }</style>
</head>

<body>

<pre id="elm"></pre>

<script>
try {
""" ++ javascript ++ """

  var app = Elm.""" ++ moduleName ++ """.init({ node: document.getElementById("elm") });
}
catch (e)
{
  // display initialization errors (e.g. bad flags, infinite recursion)
  var header = document.createElement("h1");
  header.style.fontFamily = "monospace";
  header.innerText = "Initialization Error";
  var pre = document.getElementById("elm");
  document.body.insertBefore(header, pre);
  pre.innerText = e;
  throw e;
}
</script>

</body>
</html>"""
