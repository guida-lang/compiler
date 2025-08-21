#!/bin/sh

# Ref.: https://github.com/elm/compiler/blob/master/hints/optimize.md

set -e

case $1 in
  "node")
    filepath="lib/guida.node"
    elm_entry="src/Node/Main.elm"
    ;;
  "browser")
    filepath="lib/guida.browser"
    elm_entry="src/Browser/Main.elm"
    ;;
  "bin")
    filepath="bin/guida"
    elm_entry="src/Terminal/Main.elm"
    ;;
  *)
    echo "Usage: $0 node|browser|bin"
    exit 1
    ;;
esac

js="$filepath.js"
min="$filepath.min.js"

guida make --optimize --output=$js $elm_entry
node scripts/replace-bytes-read-string.js $js

uglifyjs $js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output $min

echo "Initial size: $(cat $js | wc -c) bytes  ($js)"
echo "Minified size:$(cat $min | wc -c) bytes  ($min)"
echo "Gzipped size: $(cat $min | gzip -c | wc -c) bytes"