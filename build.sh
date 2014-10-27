#!/bin/bash

set -e

elm --make --bundle-runtime --only-js Main.elm
