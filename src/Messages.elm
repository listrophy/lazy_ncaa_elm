module Messages exposing (Msg(..))

import Models
import Random
import Time


type Msg
    = PickWinner Int Int
    | MouseEntered Int Int
    | MouseLeft Int Int
    | ClickRandomize
    | ClearBracket
    | StartRandomizing Random.Seed
    | RandomlyChooseNextGame Time.Posix
    | DismissModal
    | ShowModal Models.ModalType
