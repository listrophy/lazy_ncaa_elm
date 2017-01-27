module Main exposing (main)

import Html exposing (Html)
import Random

import Messages exposing (Msg(InitializeSeed))
import Models exposing (Model, model)
import Views exposing (view)
import Update exposing (update)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , subscriptions = always Sub.none
    , view = view
    }

init : (Model, Cmd Msg)
init =
  model ! [generateSeed]

generateSeed : Cmd Msg
generateSeed =
  Random.generate InitializeSeed
   (Random.int Random.minInt Random.maxInt)
