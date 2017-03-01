#!/bin/sh
elm-live src/Main.elm --output=elm.js --before-build=./before-build.sh --after-build=./after-build.sh
# something like `elm-live Main.elm --output=../public/js/app.js --dir=../public  --open —pushstate`
