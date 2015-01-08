#!/bin/bash

set -e
set -v

if [ "$1" == "--clean" ]; then
  rm -Rf elm-stuff/build-artifacts
  elm-make Main.elm --output build/main.js
  rm -Rf elm-stuff/build-artifacts
fi

if [ ! -d node_modules/jsdom ]; then
  npm install jsdom
fi

mkdir -p build
elm-make TestRunner.elm --output build/test.js
./elm-io.sh build/test.js build/test.io.js
node build/test.io.js
