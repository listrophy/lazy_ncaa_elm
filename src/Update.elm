module Update exposing (update)

import List.Extra as List
import Monocle.Optional as Optional exposing (Optional)

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

pickWinner : List Round -> Int -> Int -> List Round
pickWinner bracket roundNum lineNum =
  let
      maybePickedTeam =
        List.getAt roundNum bracket
          |> Maybe.andThen (List.getAt lineNum)
      maybeNextRound =
        List.getAt (roundNum + 1) bracket
      maybeNextSpot =
          Maybe.map (List.getAt (lineNum // 2)) maybeNextRound
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

modifyWinner : Int -> Int -> Team -> List Round -> List Round
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

bracketRoundOptional : Int -> Optional (List Round) Round
bracketRoundOptional =
  listOpt

roundLineOptional : Int -> Optional Round Appearance
roundLineOptional =
  listOpt

bracketLineOptional : Int -> Int -> Optional (List Round) Appearance
bracketLineOptional roundNum lineNum =
  Optional.compose (bracketRoundOptional roundNum) (roundLineOptional lineNum)

listOpt : Int -> Optional (List a) a
listOpt index =
  let
      getOption = List.getAt index
      set : a -> List a -> List a
      set x y =
        Maybe.withDefault y <| List.setAt index x y
  in
     Optional getOption set

     -- case maybePickedTeam of
     --   Nothing -> bracket
     --   Just pickedTeam ->
     --     case maybeNextSpot of
     --       Nothing -> bracket
     --       Just nextSpot ->
     --         let
     --             newNextRound =
     --               maybeNextRound
     --                 |> Maybe.andThen (List.setAt (lineNum // 2) (Just pickedTeam))
     --         in
     --            bracket
     --              |> List.setAt (roundNum + 1) newNextRound
     --              |> Maybe.withDefault bracket
