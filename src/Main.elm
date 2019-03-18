module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Styled exposing (toUnstyled)
import Messages exposing (Msg(..))
import Models exposing (Model, Randomizing(..), model)
import Update exposing (update)
import Views exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }


init : ( Model, Cmd Msg )
init =
    ( model
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions { randomizing } =
    case randomizing of
        Halted ->
            Sub.none

        Starting ->
            Sub.none

        Randomizing rando ->
            Browser.Events.onAnimationFrame RandomlyChooseNextGame
