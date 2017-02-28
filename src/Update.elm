module Update exposing (update)

import Array exposing (Array)
import Messages exposing (Msg(..))
import Models exposing (Appearance(..), Game, Model, Randomizing(..), Round, Team, extractTeam, clearAllWinners)
import Monocle.Common as Monocle
import Monocle.Optional as Optional exposing (Optional)

import Rando exposing (Rando)

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({randomizing, tournament} as model) =
  case msg of
    NoOp ->
      model ! []

    ClickRandomize ->
      model ! [Rando.init StartRandomizing]

    StartRandomizing seed ->
      startRandomizing seed model ! []

    RandomlyChooseNextGame _ ->
      randomlyChooseNextGame model ! []

    PickWinner roundNum lineNum ->
      {model | tournament = pickWinner tournament roundNum lineNum} ! []

    Randomize ->
      model ! []

    MouseEntered round line ->
      {model | hovered = Just (round, line)} ! []

    MouseLeft _ _ ->
      {model | hovered = Nothing} ! []

startRandomizing : Rando -> Model -> Model
startRandomizing rando model =
  { model
  | tournament = clearAllWinners model.tournament
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
    detectIndex2d f a2 =
      a2
        |> Array.toIndexedList
        |> List.filterMap (\(i, a)-> detectIndex f a |> Maybe.map (\j->(i,j)))
        |> List.head

    detect : (a -> Bool) -> Array a -> Maybe a
    detect f = Array.filter f >> Array.get 0

    detectJust : Array (Maybe a) -> Maybe a
    detectJust =
      detect ((/=) Nothing)
        >> Maybe.withDefault Nothing

    detect2d : (a -> Bool) -> Array (Array a) -> Maybe a
    detect2d f a =
      Array.map (detect f) a
        |> detectJust

    nextUnchosenGame =
      detectIndex2d (\app ->
          case app of
            Seeded _ -> False
            Winner g -> g.winner == Nothing
        ) model.tournament
  in
    case Debug.log "next unchosen game" nextUnchosenGame of
      Nothing -> {model | randomizing = Halted}
      Just game ->
        {model | randomizing = Halted}

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
      optionalAppearance =
        bracketLineOptional roundNum lineNum

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
          Nothing -> identity
          Just prevWinner ->
            if prevWinner == winningTeam then
              identity
            else
              clearThisTeam roundNum lineNum prevWinner
  in
    roundArray
      |> cleared
      |> Optional.modify optionalAppearance setWinner


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
