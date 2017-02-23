module Messages exposing (Msg(..))

import Models exposing (Appearance)

type Msg
  = NoOp
  | InitializeSeed Int
  | PickWinner Int Int
  | Randomize
  | MouseEntered Int Int
  | MouseLeft Int Int
