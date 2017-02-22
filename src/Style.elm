module Style exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li, main_, span)
import Css.Namespace exposing (namespace)

type CssClasses
  = Game
  | Round
  | RoundN Int
  | Spacer
  | Winner
  | GameTop
  | GameSpacer
  | GameBottom
  | RightHalf
  | Team
  | Round0
  | Seed
  | Finals
  | Champion
  | FinalLeft
  | FinalRight
  | Randomizer

type CssIds
  = Tournament

css : Stylesheet
css =
  (stylesheet << namespace "lazyNcaa") <|
    List.concat
      [ overallPage
      , overallTournament
      , roundColumns
      ]

overallPage : List Snippet
overallPage =
  [ body
      [ padding <| px 10
      , lineHeight <| em 1.4
      ]
  ]

overallTournament : List Snippet
overallTournament =
    [ main_
        [ displayFlex
        , flexDirection row
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
      , children
          [ class Spacer
              [ flexGrow <| int 1
              , firstChild [ flexGrow <| num 0.5]
              , lastChild  [ flexGrow <| num 0.5]
              ]
          ]
      ]
  ]
