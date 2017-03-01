#!/bin/sh
elm-live src/Main.elm --output=elm.js --before-build=./compile-css.sh --after-build=./after-build.sh
# something like `elm-live Main.elm --output=../public/js/app.js --dir=../public  --open —pushstate`
