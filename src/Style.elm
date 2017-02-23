module Style exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li, main_, span)
import Css.Namespace exposing (namespace)

type CssClasses
  = Round
  | RoundN Int
  | Appearance
  | Spacer
  | Winner
  | GameSpacer
  | RightHalf
  | Team
  | Seed
  | Finals
  | Champion
  | FinalLeft
  | FinalRight
  | Randomizer
  | CurrentHover
  | AncestorHover

type CssIds
  = Tournament

fontSize_ : Px
fontSize_ = px 12

css : Stylesheet
css =
  (stylesheet << namespace "lazyNcaa") <|
    List.concat
      [ overallPage
      , overallTournament
      , roundColumns
      , appearances
      , rounding
      , spacers
      , seeds
      , hovering
      ]

overallPage : List Snippet
overallPage =
  [ body
      [ padding <| px 10
      , fontFamily sansSerif
      ]
  ]

overallTournament : List Snippet
overallTournament =
    [ main_
        [ displayFlex
        , flexDirection row
        ]
    , class RightHalf
        [ textAlign right
        ]
    ]

roundColumns : List Snippet
roundColumns =
  [ class Round
      [ displayFlex
      , flexDirection column
      , justifyContent center
      , width <| px 130
      , listStyle none
      , padding <| px 0
      ]
  ]

appearances : List Snippet
appearances =
  [ class Round
      [ children
          [ class Appearance
              [ backgroundColor appearanceBackground
              , fontSize fontSize_
              , lineHeight <| num 1.2
              , padding2 (px 1) radius
              , cursor pointer
              , minWidth (px 110)
              ]
          ]
      ]
  ]

rounding : List Snippet
rounding =
  [ class (RoundN 0)
      [ children
          [ class Appearance
              [ nthOfType "4n+2" -- top
                  [ borderRadius4 radius radius px0 radius
                  ]
              , nthOfType "4n"   -- bottom
                  [ borderRadius4 radius px0 radius radius
                  ]
              ]
          ]
      , withClass RightHalf
          [ children
              [ class Appearance
                  [ nthOfType "4n+2" -- top
                      [ borderRadius4 radius radius radius px0
                      ]
                  , nthOfType "4n"   -- bottom
                      [ borderRadius4 px0 radius radius radius
                      ]
                  ]
              ]
          ]
      ]
  , class Appearance
      [ nthOfType "4n+2" -- top
          [ borderRadius4 px0 radius px0 px0
          ]
      , nthOfType "4n"   -- bottom
          [ borderRadius4 px0 px0 radius px0
          ]
      ]
  , class RightHalf
      [ children
          [ class Appearance
              [ nthOfType "4n+2" -- top
                  [ borderRadius4 radius px0 px0 px0
                  ]
              , nthOfType "4n"   -- bottom
                  [ borderRadius4 px0 px0 px0 radius
                  ]
              ]
          ]
      ]
  ]

radius : Px
radius = px 3

px0 : Px
px0 = px 0

spacers : List Snippet
spacers =
  [ class Spacer
      [ flexGrow <| int 1
      , minHeight <| px 6
      , firstChild [ flexGrow <| num 0.5 ]
      , lastChild  [ flexGrow <| num 0.5 ]
      ]
  , class GameSpacer
      [ flexGrow <| num 1
      , minHeight <| px 2
      , borderRight3 (px 1) solid connectorColor
      ]
  , class RightHalf
      [ children
          [ class GameSpacer
              [ borderRightWidth <| px 0
              , borderLeft3 (px 1) solid connectorColor
              ]
          ]
      ]
  ]

seeds : List Snippet
seeds =
  [ class Seed
      [ fontSize <| px 9
      , width <| em 1.5
      , lineHeight fontSize_
      , display inlineBlock
      ]
  ]

hovering : List Snippet
hovering =
  [ class CurrentHover
      [ important <| backgroundColor <| hex "bbb"]
  , class AncestorHover
      [ important <| backgroundColor <| hex "ccc"]
  ]

connectorColor : Color
connectorColor = lightGray

appearanceBackground : Color
appearanceBackground = lightGray

lightGray : Color
lightGray = hex "eee"
