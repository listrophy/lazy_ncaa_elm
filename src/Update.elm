module Update exposing (update)

import Array exposing (Array)
import Array.Extra as Array
import Messages exposing (Msg(..))
import Models exposing (Model, Randomizing(..), clearAllWinners)
import Models.Appearance exposing (Appearance(..), extractTeam, mapSeedAndWinner, mapTeam, mapWinner, setWinner)
import Models.Bracket exposing (Round, Bracket, appAt)
import Models.Game exposing (Game)
import Models.Team exposing (Team)
import Monocle.Optional as Optional exposing (Optional)
import Rando exposing (Rando)

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({randomizing, bracket} as model) =
  case msg of
    ClickRandomize ->
      model ! [Rando.init StartRandomizing]

    StartRandomizing seed ->
      startRandomizing seed model ! []

    RandomlyChooseNextGame _ ->
      randomlyChooseNextGame model ! []

    PickWinner roundNum lineNum ->
      pickWinner model roundNum lineNum ! []

    MouseEntered round line ->
      {model | hovered = Just (round, line)} ! []

    MouseLeft _ _ ->
      {model | hovered = Nothing} ! []

startRandomizing : Rando -> Model -> Model
startRandomizing rando model =
  { model
  | bracket = clearAllWinners model.bracket
  , randomizing = Randomizing rando
  }

randomlyChooseNextGame : Model -> Model
randomlyChooseNextGame model =
  let
    nextUnchosenGame =
      model.bracket
        |> Array.detectIndex2d (mapSeedAndWinner (always False) (\g -> g.winner == Nothing))
  in
    case nextUnchosenGame of
      Nothing -> {model | randomizing = Halted}
      Just (roundNum, lineNum) ->
        {model | bracket = randomlyPickWinner roundNum lineNum model.bracket}

randomlyPickWinner : Int -> Int -> Bracket -> Bracket
randomlyPickWinner roundNum lineNum bracket =
  let
    appA = (appAt (roundNum - 1) (lineNum * 2)).getOption bracket
    appB = (appAt (roundNum - 1) (lineNum * 2 + 1)).getOption bracket
  in
    case (appA, appB) of
      (Just a, Just b) ->
        Optional.modify (appAt roundNum lineNum) (determiner a b) bracket
      _ -> bracket

determiner : Appearance -> Appearance -> dummy -> Appearance
determiner a b _ =
  let
    winner = Maybe.map2 strategy (extractTeam a) (extractTeam b)
  in
      Winner <| Game winner

strategy : Team -> Team -> Team
strategy a b =
  if a.seed <= b.seed then a else b

pickWinner : Model -> Int -> Int -> Model
pickWinner ({bracket} as model) roundNum lineNum =
  let
      teamChanger = mapTeam (assignWinnerInvalidatingPreviousWinner (roundNum + 1) (lineNum // 2))
      newBracket =
        (appAt roundNum lineNum).getOption model.bracket
          |> Maybe.map teamChanger
          |> Maybe.withDefault identity
  in
    {model | bracket = newBracket bracket}

assignWinnerInvalidatingPreviousWinner : Int -> Int -> Team -> Bracket -> Bracket
assignWinnerInvalidatingPreviousWinner roundNum lineNum winningTeam bracket =
  let
      optionalAppearance =
        appAt roundNum lineNum

      previousWinner =
        bracket
          |> optionalAppearance.getOption
          |> Maybe.andThen extractTeam

      cleared winner prevWinner_ =
        case prevWinner_ of
          Nothing -> identity
          Just prevWinner ->
            if prevWinner == winner then
              identity
            else
              clearThisTeam roundNum lineNum prevWinner
  in
    bracket
      |> cleared winningTeam previousWinner
      |> Optional.modify optionalAppearance (setWinner <| Just winningTeam)


clearThisTeam : Int -> Int -> Team -> Bracket -> Bracket
clearThisTeam roundNum lineNum clearable bracket =
  let
      clearAppearance =
        mapWinner (\g-> if g.winner == Just clearable then {g | winner = Nothing} else g)
  in
    if roundNum >= Array.length bracket then
      bracket
    else
      bracket
        |> Optional.modify (appAt roundNum lineNum) clearAppearance
        |> clearThisTeam (roundNum + 1) (lineNum // 2) clearable
