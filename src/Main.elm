module Main exposing (main)

import Html exposing (Html)
import AnimationFrame
import Messages exposing (Msg(..))
import Models exposing (Model, model, Randomizing(..))
import Update exposing (update)
import Views exposing (view)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    model ! []


subscriptions : Model -> Sub Msg
subscriptions { randomizing } =
    case randomizing of
        Halted ->
            Sub.none

        Starting ->
            Sub.none

        Randomizing rando ->
            AnimationFrame.diffs RandomlyChooseNextGame
