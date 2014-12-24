#!/bin/bash

set -e
set -v

if [ ! -d node_modules/jsdom ]; then npm install jsdom; fi

mkdir -p build
elm-make TestRunner.elm --output build/test.js
./elm-io.sh build/test.js build/test.io.js
node build/test.io.js
