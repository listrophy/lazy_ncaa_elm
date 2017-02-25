#!/bin/sh
elm-live src/Main.elm --output=elm.js --before-build=./compile-css.sh
