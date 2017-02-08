module Update exposing (update)

import Array exposing (Array)
import Monocle.Optional as Optional exposing (Optional)
import Monocle.Common as Monocle

import Messages exposing (Msg(..))
import Models exposing (Model, Round, Appearance(..), Game, Team)
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
      ({model | tournament = pickWinner tournament roundNum lineNum}, Cmd.none)

pickWinner : Array Round -> Int -> Int -> Array Round
pickWinner bracket roundNum lineNum =
  let
      maybePickedTeam =
        Array.get roundNum bracket
          |> Maybe.andThen (Array.get lineNum)
      maybeNextRound =
        Array.get (roundNum + 1) bracket
      maybeNextSpot =
          Maybe.map (Array.get (lineNum // 2)) maybeNextRound
  in
     case maybePickedTeam of
       Nothing ->
         bracket

       Just (Seeded team) ->
         bracket
          |> modifyWinner (roundNum + 1) (lineNum // 2) team

       Just (Winner game) ->
         case game.winner of
           Nothing -> bracket
           Just team ->
             bracket
              |> modifyWinner (roundNum + 1) (lineNum // 2) team

modifyWinner : Int -> Int -> Team -> Array Round -> Array Round
modifyWinner roundNum lineNum winningTeam =
  let
      setWinner app =
        case app of
          Seeded _ -> app
          Winner game ->
            Winner {game | winner = Just winningTeam}
  in
    Optional.modify (bracketLineOptional roundNum lineNum)
      setWinner

bracketRoundOptional : Int -> Optional (Array Round) Round
bracketRoundOptional =
  Monocle.array

roundLineOptional : Int -> Optional Round Appearance
roundLineOptional =
  Monocle.array

bracketLineOptional : Int -> Int -> Optional (Array Round) Appearance
bracketLineOptional roundNum lineNum =
  Optional.compose (bracketRoundOptional roundNum) (roundLineOptional lineNum)
