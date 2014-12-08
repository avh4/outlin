#!/bin/bash

set -e

rm -f elm.js
find elm-stuff -name '*.elmi' | xargs rm
find elm-stuff -name '*.elmo' | xargs rm

elm-make Main.elm
