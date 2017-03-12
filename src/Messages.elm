module Messages exposing (Msg(..))

import Time
import Random
import Models


type Msg
    = PickWinner Int Int
    | MouseEntered Int Int
    | MouseLeft Int Int
    | ClickRandomize
    | ClearBracket
    | StartRandomizing Random.Seed
    | RandomlyChooseNextGame Time.Time
    | DismissModal
    | ShowModal Models.ModalType
