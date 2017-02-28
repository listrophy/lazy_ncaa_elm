module Models.Game exposing (..)

import Models.Team exposing (..)

type alias Game =
  { winner : Maybe Team
  }
