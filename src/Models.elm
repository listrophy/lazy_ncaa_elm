module Models exposing (
  Model,
  Game,
  Team,
  Appearance(..),
  model,
  randomizeBracket,
  teamAt,
  extractTeam,
  Round, SubRound)

import Array exposing (Array)
import Rando exposing (Rando)

type alias Model =
   -- Ideally, rando is simply "Rando" instead of "Maybe Rando", but elm-reactor
   -- doesn't allow flags, so we have to go through a run-loop to generate our
   -- initial random value & seed.
  { rando : Maybe Rando
  , tournament : Array Round
  , hovered : Maybe (Int, Int)
  }

type alias Game =
  { winner : Maybe Team
  }

type Appearance
  = Winner Game
  | Seeded Team

type alias Round = Array Appearance
type alias SubRound = Round

type alias Team =
  { name : String
  , region : Int
  , seed : Int
  }

model : Model
model =
  { rando = Nothing
  , tournament = teamArray
  , hovered = Nothing
  }

teamAt : Model -> Int -> Int -> Maybe Team
teamAt model round line =
  model.tournament
    |> Array.get round
    |> Maybe.andThen (Array.get line)
    |> Maybe.andThen extractTeam

extractTeam : Appearance -> Maybe Team
extractTeam appearance =
  case appearance of
    Seeded team -> Just team
    Winner game -> game.winner

-- TODO: implement this
randomizeBracket : Model -> Model
randomizeBracket m =
  model

aTeam : Team
aTeam =
  Team "-" 1 1

teamArray : Array Round
teamArray =
  let
      firstRound =
        [ Team "Kansas"         1  1
        , Team "Austin Peay"    1 16
        , Team "Colorado"       1  8
        , Team "Connecticut"    1  9
        , Team "Maryland"       1  5
        , Team "S. Dakota St."  1 12
        , Team "California"     1  4
        , Team "Hawaii"         1 13
        , Team "Arizona"        1  6
        , Team "Witchita State" 1 11
        , Team "Miami (Fla)"    1  3
        , Team "Buffalo"        1 14
        , Team "Iowa"           1  7
        , Team "Temple"         1 10
        , Team "Villanova"      1  2
        , Team "UNC Asheville"  1 15

        , Team "Oregon"         4  1
        , Team "Holy Cross"     4 16
        , Team "Saint Joseph's" 4  8
        , Team "Cincinnati"     4  9
        , Team "Baylor"         4  5
        , Team "Yale"           4 12
        , Team "Duke"           4  4
        , Team "UNCW"           4 13
        , Team "Texas"          4  6
        , Team "UNI"            4 11
        , Team "Texas A&M"      4  3
        , Team "Green Bay"      4 14
        , Team "Oregon State"   4  7
        , Team "VCU"            4 10
        , Team "Oklahoma"       4  2
        , Team "CSU Bakers."    4 15

        , Team "North Carolina" 2  1
        , Team "FGCU"           2 16
        , Team "USC"            2  8
        , Team "Providence"     2  9
        , Team "Indiana"        2  5
        , Team "Chattanooga"    2 12
        , Team "Kentucky"       2  4
        , Team "Stony Brook"    2 13
        , Team "Notre Dame"     2  6
        , Team "Michigan"       2 11
        , Team "West Virginia"  2  3
        , Team "S.F. Austin"    2 14
        , Team "Wisconsin"      2  7
        , Team "Pittsburgh"     2 10
        , Team "Xavier"         2  2
        , Team "Weber State"    2 15

        , Team "Virginia"       3  1
        , Team "Hampton"        3 16
        , Team "Texas Tech"     3  8
        , Team "Butler"         3  9
        , Team "Purdue"         3  5
        , Team "Little Rock"    3 12
        , Team "Iowa State"     3  4
        , Team "Iona"           3 13
        , Team "Seton Hall"     3  6
        , Team "Gonzaga"        3 11
        , Team "Utah"           3  3
        , Team "Fresno State"   3 14
        , Team "Dayton"         3  7
        , Team "Syracuse"       3 10
        , Team "Michigan State" 3  2
        , Team "Middle Tenn."   3 15
        ]
        |> List.map Seeded
      builder l =
        case l of
          [] -> []
          x :: tl ->
            case List.length x of
              1 ->
                l
              n ->
                builder <| (List.repeat (n // 2) <| Winner <| Game Nothing) :: l
  in
     builder [firstRound]
      |> List.reverse
      |> List.map Array.fromList
      |> Array.fromList
