module Update exposing (update)

import Messages exposing (Msg(..))
import Models exposing (Model, Appearance(..), Game, Team)
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

    PickWinner appearance ->
      case appearance of
        Seeded team ->
          (pickSeededWinner team model, Cmd.none)
        Winner game ->
          let _ = Debug.log "game" game
          in
            case game.winner of
              Nothing -> (model, Cmd.none)
              Just team ->
                (pickLaterWinner game model, Cmd.none)

pickSeededWinner : Team -> Model -> Model
pickSeededWinner team model =
  let
      _ = Debug.log "team" team
  in
    model

pickLaterWinner : Game -> Model -> Model
pickLaterWinner game model =
  model
