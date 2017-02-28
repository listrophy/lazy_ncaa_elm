module Update exposing (update)

import Array exposing (Array)
import Messages exposing (Msg(..))
import Models exposing (Appearance(..), Game, Model, Round, Team, extractTeam)
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
      maybePickedTeam = (bracketLineOptional roundNum lineNum).getOption bracket
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
modifyWinner roundNum lineNum winningTeam roundArray =
  let
      optionalAppearance = bracketLineOptional roundNum lineNum

      previousWinner =
        roundArray
          |> optionalAppearance.getOption
          |> Maybe.andThen extractTeam

      setWinner app =
        case app of
          Seeded _ -> app
          Winner game ->
            Winner {game | winner = Just winningTeam}

      cleared =
        case previousWinner of
          Nothing -> roundArray
          Just prevWinner ->
            clearThisTeam roundNum lineNum prevWinner roundArray
  in
    cleared
      |> Optional.modify optionalAppearance setWinner

clearThisTeam : Int -> Int -> Team -> Array Round -> Array Round
clearThisTeam roundNum lineNum clearable roundArray =
  let
      _ = Debug.log "clearing roudNum, lineNum, team" (roundNum, lineNum, clearable)
      clearAppearance x =
        case x of
          Seeded _ -> x
          Winner g ->
            if g.winner == Just clearable then
              Winner {g | winner = Nothing}
            else
              Winner g
  in
    if roundNum >= Array.length roundArray then
      roundArray
    else
      roundArray
        |> Optional.modify (bracketLineOptional roundNum lineNum) clearAppearance
        |> clearThisTeam (roundNum + 1) (lineNum // 2) clearable


bracketLineOptional : Int -> Int -> Optional (Array Round) Appearance
bracketLineOptional roundNum lineNum =
  let
      bracketToRound = Monocle.array roundNum
      roundToLine = Monocle.array lineNum
  in
     Optional.compose bracketToRound roundToLine

justsAreEqual : Maybe a -> Maybe a -> Bool
justsAreEqual a b =
  case Maybe.map2 (==) a b of
    Nothing -> False
    Just b -> b
