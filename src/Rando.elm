module Rando exposing (Rando, init, step)

import Random exposing (Seed)

type alias Rando = Seed

init : (Rando -> msg) -> Cmd msg
init msg =
  Random.int Random.minInt Random.maxInt
    |> Random.map Random.initialSeed
    |> Random.generate msg

step : Seed -> (Float, Rando)
step =
  Random.step generator

generator : Random.Generator Float
generator =
  Random.float 0.0 1.0
