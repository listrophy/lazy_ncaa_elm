module Models.Appearance exposing (..)

import Models.Game exposing (..)
import Models.Team exposing (..)

type Appearance
  = Winner Game
  | Seeded Team
