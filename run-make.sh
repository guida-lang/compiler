#!/usr/bin/env bash

clear_cache=true;

set -o errexit;
set -o nounset;

# Since guida-lang/stdlib is treated specially by the compiler (it's always
# inserted as a dependency even when not declared explicitly), we use
# a bit of a hack to make the tests run against the local source code
# rather than the guida-lang/stdlib source fetched from package.guida-lang.org.

# Create a local directory where the compiler will look for the
# guida-lang/stdlib source code:

export GUIDA_HOME="$PWD/.guida";
if [ "$clear_cache" = true ]; then
    rm -rf "$GUIDA_HOME" && mkdir -p "$GUIDA_HOME";
fi

rm -rf guida-stuff;

# Create a link to the git package
CORE_LINK="${GUIDA_HOME}/1.0.0/packages/guida-lang/stdlib/1.0.1"
CORE_GIT_DIR="/Users/decio/code/guida-lang/stdlib"

if [ "$clear_cache" = true ]; then
    echo;
    echo "Linking $CORE_LINK to $CORE_GIT_DIR"
    echo;
    mkdir -p "$(dirname $CORE_LINK)"
    ln -sv "${CORE_GIT_DIR}" "${CORE_LINK}"
    rm -vf "${CORE_GIT_DIR}"/*.dat "${CORE_GIT_DIR}"/doc*.json
fi
# Now we can run the make against the symlinked source code for real

echo;
echo "running make ...";
echo;

time ../compiler-master/bin/index.js make --output=bin/guida.js src/Terminal/Main.elm;
# time bin/index.js make --output=bin/guida.js src/Terminal/Main.elm;
node scripts/replacements.js bin/guida.js