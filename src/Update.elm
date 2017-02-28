module Update exposing (update)

import Array exposing (Array)
import Messages exposing (Msg(..))
import Models exposing (Model, Randomizing(..), clearAllWinners)
import Models.Appearance exposing (Appearance(..), extractTeam, setWinner)
import Models.Bracket exposing (Round, appAt)
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
    detectIndex : (a -> Bool) -> Array a -> Maybe Int
    detectIndex f =
      Array.toIndexedList
        >> List.filter (\(i, a)-> f a)
        >> List.head
        >> Maybe.map Tuple.first

    detectIndex2d : (a -> Bool) -> Array (Array a) -> Maybe (Int, Int)
    detectIndex2d f =
      Array.toIndexedList
        >> List.filterMap (\(i, a)-> detectIndex f a |> Maybe.map (\j->(i,j)))
        >> List.head

    nextUnchosenGame =
      detectIndex2d (\app ->
          case app of
            Seeded _ -> False
            Winner g -> g.winner == Nothing
        ) model.bracket
  in
    case nextUnchosenGame of
      Nothing -> {model | randomizing = Halted}
      Just (roundNum, lineNum) ->
        {model | bracket = randomlyPickWinner roundNum lineNum model.bracket}

randomlyPickWinner : Int -> Int -> Array Round -> Array Round
randomlyPickWinner roundNum lineNum roundArray =
  let
    appA = (appAt (roundNum - 1) (lineNum * 2)).getOption roundArray
    appB = (appAt (roundNum - 1) (lineNum * 2 + 1)).getOption roundArray
  in
    case (appA, appB) of
      (Just a, Just b) ->
        Optional.modify (appAt roundNum lineNum) (determiner a b) roundArray
      _ -> roundArray

determiner : Appearance -> Appearance -> Appearance -> Appearance
determiner a b _ =
  let
      winner =
        case (a, b) of
          (Seeded aTeam, Seeded bTeam) ->
            Just <| strategy aTeam bTeam
          (Winner aGame, Winner bGame) ->
            Maybe.map2 strategy aGame.winner bGame.winner
          _ -> Nothing
  in
      Winner <| Game winner

strategy : Team -> Team -> Team
strategy a b =
  if a.seed <= b.seed then a else b

pickWinner : Model -> Int -> Int -> Model
pickWinner ({bracket} as model) roundNum lineNum =
  let
      maybePickedTeam = (appAt roundNum lineNum).getOption model.bracket
      newBracket =
         case maybePickedTeam of
           Nothing ->
             bracket

           Just (Seeded team) ->
             bracket
              |> cascadeWinner (roundNum + 1) (lineNum // 2) team

           Just (Winner game) ->
             case game.winner of
               Nothing -> bracket
               Just team ->
                 bracket
                   |> cascadeWinner (roundNum + 1) (lineNum // 2) team
  in
    {model | bracket = newBracket}

cascadeWinner : Int -> Int -> Team -> Array Round -> Array Round
cascadeWinner roundNum lineNum winningTeam roundArray =
  let
      optionalAppearance =
        appAt roundNum lineNum

      previousWinner =
        roundArray
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
    roundArray
      |> cleared winningTeam previousWinner
      |> Optional.modify optionalAppearance (setWinner <| Just winningTeam)


clearThisTeam : Int -> Int -> Team -> Array Round -> Array Round
clearThisTeam roundNum lineNum clearable roundArray =
  let
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
        |> Optional.modify (appAt roundNum lineNum) clearAppearance
        |> clearThisTeam (roundNum + 1) (lineNum // 2) clearable
