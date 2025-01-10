# Clean all
rm -rf guida-stuff ~/.guida elm-stuff ~/.elm

echo "------------------"

# GUIDA

## Run initial guida
time ./bin/index.js make src/Terminal/Main.elm

## Clean local guida-stuff
rm -rf guida-stuff
time ./bin/index.js make src/Terminal/Main.elm

## No clean (guida)
time ./bin/index.js make src/Terminal/Main.elm

echo "------------------"

# ELM

## Run initial elm
time elm make src/Terminal/Main.elm

## Clean local elm-stuff
rm -rf elm-stuff
time elm make src/Terminal/Main.elm

## No clean (elm)
time elm make src/Terminal/Main.elm
