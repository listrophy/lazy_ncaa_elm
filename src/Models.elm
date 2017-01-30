module Models exposing (
  Model,
  Game,
  Team,
  Appearance(..),
  model,
  randomizeBracket,
  seedForRegionalGameLocation,
  UpDown(..))

import Dict exposing (Dict)
import Rando exposing (Rando)

type alias Model =
   -- Ideally, rando is simply "Rando" instead of "Maybe Rando", but elm-reactor
   -- doesn't allow flags, so we have to go through a run-loop to generate our
   -- initial random value & seed.
  { rando : Maybe Rando
  , bracket : Appearance
  }

type alias Game =
  { winner : Maybe Team
  , appearances : (Appearance, Appearance)
  , location : GameLocation
  }

type Appearance
  = Winner Game
  | Seeded Team

type alias Team =
  { name : String
  , seed : Int
  , region : Int
  }

type UpDown = Up | Dn

type alias GameLocation = List UpDown


model : Model
model =
  let
      appearances = (undecidedAppearance [Up], undecidedAppearance [Dn])
      bracket =
        { winner = Nothing
        , appearances = appearances
        , location = []
        }
  in
    { rando = Nothing
    , bracket = Winner bracket
    }

undecidedAppearance : GameLocation -> Appearance
undecidedAppearance gameLoc =
  case List.length gameLoc of
    6 ->
      let
        regionAndSeed = determineRegionAndSeed gameLoc
        team =
          Dict.get regionAndSeed teamList
            |> Maybe.withDefault (aTeam)
      in
        Seeded team

    _ ->
      let
          up = undecidedAppearance (Up :: gameLoc)
          dn = undecidedAppearance (Dn :: gameLoc)
          game = Game Nothing (up, dn) gameLoc
      in
        Winner game

determineRegionAndSeed : GameLocation -> (Int, Int)
determineRegionAndSeed loc =
  let
    revLoc = List.reverse loc
    championship = Maybe.withDefault Up <| List.head revLoc
    finalFour = Maybe.withDefault Up <| List.head <| List.drop 1 revLoc
    rest = List.drop 2 revLoc
    seed = seedForRegionalGameLocation rest
  in
    case (championship, finalFour) of
      (Up, Up) -> (1, seed)
      (Up, Dn) -> (4, seed)
      (Dn, Up) -> (2, seed)
      (Dn, Dn) -> (3, seed)

-- TODO: implement this
randomizeBracket : Model -> Model
randomizeBracket m =
  model

seedForRegionalGameLocation : GameLocation -> Int
seedForRegionalGameLocation upDownList =
  let
      powerer x m =
        case x of
          Dn ->
            (2 ^ (List.length m)) :: m
          Up ->
            0 :: m
  in
    upDownList
      |> List.reverse
      |> List.foldl powerer []
      |> List.foldl (+) 1
      |> seedLookup

seedLookup : Int -> Int
seedLookup int =
    case int of
      1 -> 1
      2 -> 16
      3 -> 8
      4 -> 9
      5 -> 5
      6 -> 12
      7 -> 4
      8 -> 13
      9 -> 6
      10 -> 11
      11 -> 3
      12 -> 14
      13 -> 7
      14 -> 10
      15 -> 2
      16 -> 15
      _ -> 0

positionLookup : Int -> GameLocation
positionLookup seed =
  case seed of
    1  -> [Up, Up, Up, Up]
    16 -> [Dn, Up, Up, Up]
    8  -> [Up, Dn, Up, Up]
    9  -> [Dn, Dn, Up, Up]
    5  -> [Up, Up, Dn, Up]
    12 -> [Dn, Up, Dn, Up]
    4  -> [Up, Dn, Dn, Up]
    13 -> [Dn, Dn, Dn, Up]
    6  -> [Up, Up, Up, Dn]
    11 -> [Dn, Up, Up, Dn]
    3  -> [Up, Dn, Up, Dn]
    14 -> [Dn, Dn, Up, Dn]
    7  -> [Up, Up, Dn, Dn]
    10 -> [Dn, Up, Dn, Dn]
    2  -> [Up, Dn, Dn, Dn]
    15 -> [Dn, Dn, Dn, Dn]
    _  -> []

aTeam : Team
aTeam =
  Team "-" 1 1

teamList : Dict (Int, Int) Team
teamList =
  [ ((1, 1), "Kansas")
  , ((1,16), "Austin Peay")
  , ((1, 8), "Colorado")
  , ((1, 9), "Connecticut")
  , ((1, 5), "Maryland")
  , ((1,12), "South Dakota St.")
  , ((1, 4), "California")
  , ((1,13), "Hawaii")
  , ((1, 6), "Arizona")
  , ((1,11), "Witchita State")
  , ((1, 3), "Miami (Fla)")
  , ((1,14), "Buffalo")
  , ((1, 7), "Iowa")
  , ((1,10), "Temple")
  , ((1, 2), "Villanova")
  , ((1,15), "UNC Asheville")

  , ((4, 1), "Oregon")
  , ((4,16), "Holy Cross")
  , ((4, 8), "Saint Joseph's")
  , ((4, 9), "Cincinnati")
  , ((4, 5), "Baylor")
  , ((4,12), "Yale")
  , ((4, 4), "Duke")
  , ((4,13), "UNCW")
  , ((4, 6), "Texas")
  , ((4,11), "UNI")
  , ((4, 3), "Texas A&M")
  , ((4,14), "Green Bay")
  , ((4, 7), "Oregon State")
  , ((4,10), "VCU")
  , ((4, 2), "Oklahoma")
  , ((4,15), "CSU Bakersfield")

  , ((2, 1), "North Carolina")
  , ((2,16), "FGCU")
  , ((2, 8), "USC")
  , ((2, 9), "Providence")
  , ((2, 5), "Indiana")
  , ((2,12), "Chattanooga")
  , ((2, 4), "Kentucky")
  , ((2,13), "Stony Brook")
  , ((2, 6), "Notre Dame")
  , ((2,11), "Michigan")
  , ((2, 3), "West Virginia")
  , ((2,14), "Steph. F. Austin")
  , ((2, 7), "Wisconsin")
  , ((2,10), "Pittsburgh")
  , ((2, 2), "Xavier")
  , ((2,15), "Weber State")

  , ((3, 1), "Virginia")
  , ((3,16), "Hampton")
  , ((3, 8), "Texas Tech")
  , ((3, 9), "Butler")
  , ((3, 5), "Purdue")
  , ((3,12), "Little Rock")
  , ((3, 4), "Iowa State")
  , ((3,13), "Iona")
  , ((3, 6), "Seton Hall")
  , ((3,11), "Gonzaga")
  , ((3, 3), "Utah")
  , ((3,14), "Fresno State")
  , ((3, 7), "Dayton")
  , ((3,10), "Syracuse")
  , ((3, 2), "Michigan State")
  , ((3,15), "Middle Tenn.")
  ]
  |> List.map (\((r,s),t) -> ((r,s), Team t s r))
  |> Dict.fromList
