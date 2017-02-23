module Update exposing (update)

import Array exposing (Array)
import Messages exposing (Msg(..))
import Models exposing (Appearance(..), Game, Model, Round, Team)
import Monocle.Common as Monocle
import Monocle.Optional as Optional exposing (Optional)
import Rando

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({rando, tournament} as model) =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    InitializeSeed seed ->
      let
          newModel =
              { model | rando = Just <| Rando.init seed }
      in
          (Models.randomizeBracket newModel, Cmd.none)

    PickWinner roundNum lineNum ->
      {model | tournament = pickWinner tournament roundNum lineNum} ! []

    Randomize ->
      model ! []

    MouseEntered round line ->
      {model | hovered = Just (round, line)} ! []

    MouseLeft _ _ ->
      {model | hovered = Nothing} ! []

pickWinner : Array Round -> Int -> Int -> Array Round
pickWinner bracket roundNum lineNum =
  let
      maybePickedTeam =
        Array.get roundNum bracket
          |> Maybe.andThen (Array.get lineNum)
  in
     case maybePickedTeam of
       Nothing ->
         bracket

       Just (Seeded team) ->
         bracket
          |> modifyWinner (roundNum + 1) (lineNum // 2) (Just team)

       Just (Winner game) ->
         case game.winner of
           Nothing -> bracket
           Just team ->
             bracket
              |> modifyWinner (roundNum + 1) (lineNum // 2) (Just team)

modifyWinner : Int -> Int -> Maybe Team -> Array Round -> Array Round
modifyWinner roundNum lineNum maybeWinningTeam =
  let
      setWinner app =
        case app of
          Seeded _ -> app
          Winner game ->
            Winner {game | winner = maybeWinningTeam}
  in
    Optional.modify (bracketLineOptional roundNum lineNum)
      setWinner

bracketLineOptional : Int -> Int -> Optional (Array Round) Appearance
bracketLineOptional roundNum lineNum =
  let
      bracketToRound = Monocle.array roundNum
      roundToLine = Monocle.array lineNum
  in
     Optional.compose bracketToRound roundToLine
