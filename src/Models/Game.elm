module Models.Game exposing (..)

import Models.Team exposing (..)


type alias Game =
    { winner : Maybe Team
    , hovered : Bool
    , roundNum : Int
    , lineNum : Int
    }
