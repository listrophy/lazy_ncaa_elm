module Style exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, div, li, main_, span)
import Css.Namespace exposing (namespace)

type CssClasses
  = Round
  | RoundN Int
  | Appearance
  | NotYetChosen
  | Spacer
  | Winner
  | GameSpacer
  | RightHalf
  | Team
  | Seed
  | Finals
  | Champion
  | Finalist
  | FinalLeft
  | FinalRight
  | Branding
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
      , finalFour
      , hovering
      , randomize
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
      , flex3 (int 1) zero (px 110)
      , listStyle none
      , padding zero
      , minWidth <| px 110
      ]
  ]

appearances : List Snippet
appearances =
  [ class Round
      [ fontSize fontSize_
      ]
  , class Appearance
      [ backgroundColor appearanceBackground
      , lineHeight <| num 1.2
      , padding2 (px 1) radius
      , overflowX hidden
      , cursor pointer
      , withClass NotYetChosen [ cursor default ]
      ]
  ]

rounding : List Snippet
rounding =
  [ class (RoundN 0)
      [ descendants
          [ class Appearance
              [ nthOfType "4n+2" [ borderYYNY ] -- top
              , nthOfType "4n" [ borderYNYY ]   -- bottom
              ]
          ]
      , withClass RightHalf
          [ descendants
              [ class Appearance
                  [ nthOfType "4n+2" [ borderYYYN ] -- top
                  , nthOfType "4n" [ borderNYYY ]   -- bottom
                  ]
              ]
          ]
      ]
  , class Appearance
      [ nthOfType "4n+2" [ borderNYNN ] -- top
      , nthOfType "4n" [ borderNNYN ]   -- bottom
      ]
  , class RightHalf
      [ descendants
          [ class Appearance
              [ nthOfType "4n+2" [ borderYNNN ] -- top
              , nthOfType "4n" [ borderNNNY ]   -- bottom
              ]
          ]
      ]
  ]

radius : Px
radius = px 4

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
      [ descendants
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
      [ important <| hoverBackgroundCurrent ]
  , class AncestorHover
      [ important <| hoverBackgroundAncestor ]
  ]

finalFour : List Snippet
finalFour =
  [ class Finals
      [ flexBasis <| px 130
      , justifyContent spaceAround
      , children
          [ li
              [ maxWidth <| pct 80
              , minWidth <| pct 80
              ]
          , class FinalLeft [ alignSelf flexStart, borderNYYN ]
          , class FinalRight [ alignSelf flexEnd, textAlign right, borderYNNY ]
          , class Champion [ centerize, borderYYYY, cursor default ]
          , class Finalist []
          , class Branding [ centerize ]
          ]
      ]
  , class Randomizer
      [ centerize
      ]
  ]

centerize : Mixin
centerize =
  mixin
    [ alignSelf center
    , textAlign center
    ]

-- border radius

borderYYYY : Mixin
borderYYYY = borderRadius radius
borderNYYN : Mixin
borderNYYN = borderRadius4 px0 radius radius px0
borderYNNY : Mixin
borderYNNY = borderRadius4 radius px0 px0 radius
borderNYNN : Mixin
borderNYNN = borderRadius4 px0 radius px0 px0
borderNNYN : Mixin
borderNNYN = borderRadius4 px0 px0 radius px0
borderYYNY : Mixin
borderYYNY = borderRadius4 radius radius px0 radius
borderYNYY : Mixin
borderYNYY = borderRadius4 radius px0 radius radius
borderYYYN : Mixin
borderYYYN = borderRadius4 radius radius radius px0
borderNYYY : Mixin
borderNYYY = borderRadius4 px0 radius radius radius
borderYNNN : Mixin
borderYNNN = borderRadius4 radius px0 px0 px0
borderNNNY : Mixin
borderNNNY = borderRadius4 px0 px0 px0 radius

-- Colors

connectorColor : Color
connectorColor = lightGray

appearanceBackground : Color
appearanceBackground = lightGray

lightGray : Color
lightGray = hex "eee"

hoverBackgroundCurrent : Mixin
hoverBackgroundCurrent = backgroundColor <| hex "bbb"

hoverBackgroundAncestor : Mixin
hoverBackgroundAncestor = backgroundColor <| hex "ccc"

-- button

randomize : List Snippet
randomize =
  [ class Randomizer
      [ children
          [ div
              [ padding2 (px 12) (px 12)
              , cursor pointer
              , property "user-select" "none"
              , property "transition" "all 60ms ease-in-out"
              , textAlign center
              , whiteSpace noWrap
              , important <| textDecoration none
              , color <| hex "fff"
              , backgroundColor <| hex "2b90d9"
              , border2 zero none
              , borderRadius <| px 4
              , fontWeight <| int 700
              , lineHeight <| num 1.3
              , property "appearance" "none"
              , hover
                  [ property "transition" "all 60ms ease"
                  , opacity <| num 0.85
                  ]
              , active
                  [ property "transition" "all 60ms ease"
                  , opacity <| num 0.75
                  ]
              , focus
                  [ outline3 (px 1) dotted (hex "959595")
                  , outlineOffset <| px -4
                  ]
              ]
          ]
      ]
  ]