module Views exposing (..)

import Array exposing (Array)
import Html exposing (Html, li, span, text)
import Html.CssHelpers
import Html.Events as E
import Html.Lazy
import List.Extra as List exposing (elemIndex)
import Messages exposing (Msg, Msg(..))
import Models exposing (Appearance, Appearance(..), Game, Model, Round, Team)
import Style as S

{ id, class, classList } =
  Html.CssHelpers.withNamespace "lazyNcaa"

type Side
  = Left
  | Right

type Column
  = RoundColumn Int Side (List Renderable)
  | FinalsColumn (Renderable, Renderable, Renderable)

type alias Renderable =
  { round : Int
  , line : Int
  , appearance : Appearance
  }

view : Model -> Html Msg
view model =
  Html.main_
    [ id S.Tournament ]
    ( tourney model.tournament )

tourney : Array Round -> List (Html Msg)
tourney =
  columnize >> List.map renderColumn

columnize : Array (Array Appearance) -> List Column
columnize =
  Array.toIndexedList
    >> List.map (\(roundNum, round)-> Array.indexedMap (Renderable roundNum) round |> Array.toList)
    >> renderablesToColumns 0

renderColumn : Column -> Html Msg
renderColumn =
  Html.Lazy.lazy renderColumn_

renderColumn_ : Column -> Html Msg
renderColumn_ column =
  case column of
    RoundColumn roundNum side renderables ->
      Html.ul
        [ classList
            [ (S.Round, True)
            , (S.RightHalf, isRight column)
            , (S.RoundN roundNum, True)
            ]
        ]
        (renderGenericColumn side renderables)

    FinalsColumn (leftFinalist, rightFinalist, champion) ->
      Html.ul
        [ class [ S.Round, S.Finals ] ]
        [ renderChampion champion
        , renderFinalist leftFinalist
        , renderFinalist rightFinalist
        , randomizeButton
        ]

renderGenericColumn : Side -> List Renderable -> List (Html Msg)
renderGenericColumn side renderables =
  case renderables of
    top :: bottom :: tl ->
      [ spacer
      , renderAppearance side top
      , gameSpacer
      , renderAppearance side bottom
      ] ++ renderGenericColumn side tl
    [] ->
      [spacer]
    _ ->
      []

renderChampion : Renderable -> Html Msg
renderChampion renderable =
  li
    []
    [ appearanceText renderable.appearance ]

renderFinalist : Renderable -> Html Msg
renderFinalist renderable =
  li
    [ E.onClick <| clickWinner renderable ]
    [ appearanceText renderable.appearance ]

renderablesToColumns : Int -> List (List Renderable) -> List Column
renderablesToColumns roundNum list =
  case list of
    [finalist1, finalist2] :: [champion] :: [] ->
      [FinalsColumn (finalist1, finalist2, champion)]

    hd :: tl ->
      let
          nextRound = renderablesToColumns (roundNum + 1) tl
          (left, right) = halve hd
          toColumn =
            RoundColumn roundNum
      in
          List.concat
            [ [toColumn Left left]
            , nextRound
            , [toColumn Right right]
            ]
    _ ->
      []

gameSpacer : Html Msg
gameSpacer =
  li [class [S.GameSpacer]] [ text " " ]

spacer : Html Msg
spacer =
  li [class [S.Spacer]] [ text " " ]

randomizeButton : Html Msg
randomizeButton =
  li
    [ class [S.Randomizer] ]
    [ Html.button [ E.onClick Randomize ] [ text "Randomize" ] ]

renderAppearance : Side -> Renderable -> Html Msg
renderAppearance side renderable =
  case renderable.appearance of
    Seeded team ->
      renderRound0Appearance side renderable team
    Winner game ->
      renderRoundNAppearance side renderable game

renderRound0Appearance : Side -> Renderable -> Team -> Html Msg
renderRound0Appearance side renderable team =
  case side of
    Left ->
      round0Elem renderable <|
        [ span [class [S.Seed]] [ text <| toString team.seed ]
        , span [class [S.Team]] [ teamText team ]
        ]
    Right ->
      round0Elem renderable <|
        [ span [class [S.Team]] [ teamText team ]
        , span [class [S.Seed]] [ text <| toString team.seed ]
        ]

round0Elem : Renderable -> List (Html Msg) -> Html Msg
round0Elem renderable =
  li
    [ E.onClick <| clickWinner renderable
    , class [S.Appearance]
    ]

renderRoundNAppearance : Side -> Renderable -> Game -> Html Msg
renderRoundNAppearance side renderable game =
  li
    [ class [ S.Appearance ]
    , E.onClick <| clickWinner renderable
    ]
    [ gameText game ]

gameText : Game -> Html a
gameText =
  text << Maybe.withDefault "-" << Maybe.map .name << .winner

teamText : Team -> Html a
teamText =
  text << .name

appearanceText : Appearance -> Html a
appearanceText appearance =
  case appearance of
    Seeded team -> teamText team
    Winner game -> gameText game

clickWinner : Renderable -> Msg
clickWinner {round, line} =
  PickWinner round line

halve : List a -> (List a, List a)
halve list =
  List.splitAt ((List.length list) // 2) list

isRight : Column -> Bool
isRight column =
  case column of
    RoundColumn _ Left _ -> False
    RoundColumn _ _ _ -> True
    _ -> False
