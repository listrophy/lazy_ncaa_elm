module Models.Appearance exposing (..)

import Models.Team exposing (..)


type alias Appearance =
    { winner : Maybe Team
    , hovered : Bool
    , ancestorHovered : Bool
    }


setHover : Bool -> Appearance -> Appearance
setHover bool app =
    case app.winner of
        Nothing ->
            app

        Just _ ->
            { app | hovered = bool }


setAncestorHover : Bool -> Appearance -> Appearance
setAncestorHover bool app =
    { app | ancestorHovered = bool }


setWinner : Maybe Team -> Appearance -> Appearance
setWinner teamMaybe app =
    { app | winner = teamMaybe }


isUndecided : Appearance -> Bool
isUndecided app =
    app.winner == Nothing
