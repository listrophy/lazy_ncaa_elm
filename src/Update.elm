module Update exposing (update)

import Messages exposing (Msg(..))
import Models exposing (Model)
import Rando

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    InitializeSeed seed ->
      let
          newModel =
              { model | rando = Just <| Rando.init seed }
      in
          (Models.randomizeBracket newModel, Cmd.none)
