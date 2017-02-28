module Style exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, div, li, main_, span, footer)
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
      , borders
      , spacers
      , seeds
      , finalFour
      , hovering
      , randomize
      , footer_
      ]

overallPage : List Snippet
overallPage =
  [ body
      [ margin zero
      , fontFamily sansSerif
      , minHeight <| vh 100
      , backgroundColor grayA
      , children
          [ div
              [ backgroundImage <| url "/hoop.jpg"
              , backgroundRepeat noRepeat
              , backgroundColor bottomColorOfBackground
              , property "background-position" "center -40px"
              , minHeight <| vh 100
              , width <| px 1280
              , margin2 px0 auto
              , borderLeftWidth <| px 3
              , borderRightWidth <| px 3
              , borderTopWidth px0
              , borderBottomWidth px0
              , borderColor grayB
              , borderStyle solid
              ]
          ]
      ]
  ]

overallTournament : List Snippet
overallTournament =
    [ main_
        [ displayFlex
        , flexDirection row
        , padding <| px 10
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
      , marginTop zero
      , marginBottom zero
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
      , color appearanceColor
      , lineHeight <| num 1.2
      , minHeight <| px 14
      , padding2 (px 1) radius
      , overflowX hidden
      , cursor pointer
      , border3 (px 1) solid grayA
      , withClass NotYetChosen [ cursor default ]
      ]
  , class Champion
      [ fontSize <| px 18
      , minHeight <| px 22
      ]
  ]

rounding : List Snippet
rounding =
  [ class Appearance
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
  , class (RoundN 0)
      [ descendants
          [ class Appearance
              [ nthOfType "4n+2" [ borderYYNN ] -- top
              , nthOfType "4n" [ borderNNYY ]   -- bottom
              ]
          ]
      ]
  ]

borders : List Snippet
borders =
  [ class (RoundN 0)
      [ descendants
          [ class Appearance
              [ borderLeftWidth <| px 1
              ]
          ]
      , withClass RightHalf
          [ descendants
              [ class Appearance
                  [ borderRightWidth <| px 1
                  ]
              ]
          ]
      ]
  , class Appearance
      [ borderLeftWidth px0
      ]
  , class RightHalf
      [ descendants
          [ class Appearance
              [ borderLeftWidth <| px 1
              , borderRightWidth px0
              ]
          ]
      ]
  , class Finals
      [ children
          [ class Champion
              [ borderLeftWidth <| px 1 ]
          , class FinalRight
              [ borderLeftWidth <| px 1
              , borderRightWidth px0
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
      [ important <| hoverBackgroundCurrent
      , important <| hoverColorCurrent
      ]
  , class AncestorHover
      [ important <| hoverBackgroundAncestor
      ]
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
          , class Branding [ centerize, flexBasis <| px 150 ]
          ]
      ]
  , class Randomizer
      [ centerize
      , flexBasis <| px 100
      , displayFlex
      , flexDirection column
      , justifyContent center
      ]
  ]

footer_ : List Snippet
footer_ =
  [ footer
      [ textAlign center
      , fontSize <| pct 80
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
borderYYNN : Mixin
borderYYNN = borderRadius4 radius radius px0 px0
borderNNYY : Mixin
borderNNYY = borderRadius4 px0 px0 radius radius
borderYNNN : Mixin
borderYNNN = borderRadius4 radius px0 px0 px0
borderNNNY : Mixin
borderNNNY = borderRadius4 px0 px0 px0 radius

-- Colors

connectorColor : Color
connectorColor = grayA

appearanceBackground : Color
appearanceBackground = gray0
appearanceColor : Color
appearanceColor = grayB

lightGray : Color
lightGray = hex "eee"

hoverBackgroundCurrent : Mixin
hoverBackgroundCurrent = backgroundColor grayB
hoverColorCurrent : Mixin
hoverColorCurrent = color almostWhite


hoverBackgroundAncestor : Mixin
hoverBackgroundAncestor = backgroundColor grayA

gray0 : Color
gray0 = hex "c9c9c9"

grayA : Color
grayA = hex "a3a3a3"
grayB : Color
grayB = hex "555555"
grayC : Color
grayC = hex "1a1a1a"

bottomColorOfBackground : Color
bottomColorOfBackground = almostWhite

almostWhite : Color
almostWhite = hex "fbfbfb"

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
              , color <| almostWhite
              , backgroundColor <| grayB
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
