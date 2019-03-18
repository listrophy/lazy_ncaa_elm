module Models.Appearance exposing (Appearance, isUndecided, setAncestorHover, setHover, setWinner, sortAppearances)

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


sortAppearances : Appearance -> Appearance -> ( Appearance, Appearance )
sortAppearances appearance appearance2 =
    case ( appearance.winner, appearance2.winner ) of
        ( Just t1, Just t2 ) ->
            if t1.seed <= t2.seed then
                ( appearance, appearance2 )

            else
                ( appearance2, appearance )

        _ ->
            ( appearance, appearance2 )
