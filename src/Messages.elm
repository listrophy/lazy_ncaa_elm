module Messages exposing (Msg(..))

import Time
import Random


type Msg
    = PickWinner Int Int
    | MouseEntered Int Int
    | MouseLeft Int Int
    | ClickRandomize
    | StartRandomizing Random.Seed
    | RandomlyChooseNextGame Time.Time
