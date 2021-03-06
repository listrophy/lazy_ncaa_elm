module Models.Game exposing (Game, game)

import Models.Team exposing (..)


type alias Game =
    { winner : Maybe Team
    , hovered : Bool
    , ancestorHovered : Bool
    , roundNum : Int
    , lineNum : Int
    }


game : Int -> Int -> Game
game =
    Game Nothing False False
