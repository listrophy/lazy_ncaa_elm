#!/bin/sh

cp elm.js index.js
echo ";Elm.Main.embed(document.querySelector('div'));" >> index.js
