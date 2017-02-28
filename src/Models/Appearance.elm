module Models.Appearance exposing (..)

import Models.Game exposing (..)
import Models.Team exposing (..)

type Appearance
  = Winner Game
  | Seeded Team

setWinner : Maybe Team -> Appearance -> Appearance
setWinner teamMaybe =
  mapWinner (\g -> {g | winner = teamMaybe})

mapWinner : (Game -> Game) -> Appearance -> Appearance
mapWinner f app =
  case app of
    Seeded _ -> app
    Winner g ->
      Winner <| f g

extractTeam : Appearance -> Maybe Team
extractTeam appearance =
  case appearance of
    Seeded team -> Just team
    Winner game -> game.winner
