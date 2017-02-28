module Messages exposing (Msg(..))

import Time
import Random

type Msg
  = NoOp
  | PickWinner Int Int
  | Randomize
  | MouseEntered Int Int
  | MouseLeft Int Int
  | ClickRandomize
  | StartRandomizing Random.Seed
  | RandomlyChooseNextGame Time.Time
