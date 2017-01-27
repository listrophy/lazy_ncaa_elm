module Rando exposing (Rando, init, step)

import Random exposing (Seed)

type alias Rando = Seed

init : Int -> Rando
init seed =
  Random.initialSeed seed

step : Rando -> (Float, Rando)
step rando =
  stepper rando

stepper : Seed -> (Float, Seed)
stepper =
  Random.step (Random.float 0 1)
