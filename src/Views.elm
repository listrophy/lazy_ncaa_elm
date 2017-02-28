module Views exposing (..)

import Array exposing (Array)
import Html exposing (Html, li, span, text, a)
import Html.Attributes as A
import Html.CssHelpers
import Html.Events as E
import Html.Lazy
import List.Extra as List exposing (elemIndex)
import Messages exposing (Msg(..))
import Models exposing (Appearance, Appearance(..), Game, Model, Round, Team, extractTeam, teamAt)
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
  Html.div []
    [ Html.main_
        [ id S.Tournament ]
        ( tourney model model.tournament)
    , footer model
    ]

footer : Model -> Html Msg
footer model =
  Html.footer []
    [ text "© 2017 Bendyworks, Inc. Code available on "
    , a
        [ A.href "https://github.com/listrophy/lazy_ncaa_elm"
        , A.target "_blank"
        ]
        [ text "GitHub" ]
    ]

tourney : Model -> Array Round -> List (Html Msg)
tourney model =
  columnize >> List.map (renderColumn model)

columnize : Array (Array Appearance) -> List Column
columnize =
  Array.toIndexedList
    >> List.map (\(roundNum, round)-> Array.indexedMap (Renderable roundNum) round |> Array.toList)
    >> renderablesToColumns 0

renderColumn : Model -> Column -> Html Msg
renderColumn model =
  Html.Lazy.lazy <| renderColumn_ model

renderColumn_ : Model -> Column -> Html Msg
renderColumn_ model column =
  case column of
    RoundColumn roundNum side renderables ->
      Html.ul
        [ classList
            [ (S.Round, True)
            , (S.RightHalf, isRightColumn column)
            , (S.RoundN roundNum, True)
            ]
        ]
        (renderGenericColumn side renderables model)

    FinalsColumn (leftFinalist, rightFinalist, champion) ->
      Html.ul
        [ class [ S.Round, S.Finals ] ]
        [ renderBranding
        , renderFinalist Left leftFinalist model
        , renderChampion champion model
        , renderFinalist Right rightFinalist model
        , randomizeButton
        ]

renderBranding : Html Msg
renderBranding =
  li
    [ class [ S.Branding ] ]
    [  ]

renderGenericColumn : Side -> List Renderable -> Model -> List (Html Msg)
renderGenericColumn side renderables model =
  case renderables of
    top :: bottom :: tl ->
      [ spacer
      , renderAppearance side top model
      , gameSpacer
      , renderAppearance side bottom model
      ] ++ renderGenericColumn side tl model
    [] ->
      [spacer]
    _ ->
      []

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

renderChampion : Renderable -> Model -> Html Msg
renderChampion renderable model =
  li
    [ classList
        [ (S.Champion, True)
        , (S.Appearance, True)
        , (S.CurrentHover, isHovering model renderable)
        ]
    , E.onMouseEnter <| MouseEntered renderable.round renderable.line
    , E.onMouseLeave <| MouseLeft renderable.round renderable.line
    ]
    [ appearanceText renderable.appearance ]

renderFinalist : Side -> Renderable -> Model -> Html Msg
renderFinalist side renderable model =
  let
      winner =
        extractTeam renderable.appearance
  in
    li
      [ E.onClick <| clickWinner renderable
      , E.onMouseEnter <| MouseEntered renderable.round renderable.line
      , E.onMouseLeave <| MouseLeft renderable.round renderable.line
      , classList
          [ (S.Appearance, True)
          , (S.Finalist, True)
          , (S.FinalLeft, not <| isRight side)
          , (S.FinalRight, isRight side)
          , (S.CurrentHover, isHovering model renderable)
          , (S.AncestorHover, isAncestorOfHover model renderable)
          , (S.NotYetChosen, Maybe.Nothing == winner)
          ]
      ]
      [ appearanceText renderable.appearance ]

renderAppearance : Side -> Renderable -> Model -> Html Msg
renderAppearance side renderable model =
  case renderable.appearance of
    Seeded team ->
      renderRound0Appearance side renderable team model
    Winner game ->
      renderRoundNAppearance side renderable game model

renderRound0Appearance : Side -> Renderable -> Team -> Model -> Html Msg
renderRound0Appearance side renderable team model =
  case side of
    Left ->
      round0Elem renderable model <|
        [ span [class [S.Seed]] [ text <| toString team.seed ]
        , span [class [S.Team]] [ teamText team ]
        ]
    Right ->
      round0Elem renderable model <|
        [ span [class [S.Team]] [ teamText team ]
        , span [class [S.Seed]] [ text <| toString team.seed ]
        ]

round0Elem : Renderable -> Model -> List (Html Msg) -> Html Msg
round0Elem renderable model =
  li
    [ E.onClick <| clickWinner renderable
    , E.onMouseEnter <| MouseEntered renderable.round renderable.line
    , E.onMouseLeave <| MouseLeft renderable.round renderable.line
    , classList
        [ (S.Appearance, True)
        , (S.CurrentHover, isHovering model renderable)
        , (S.AncestorHover, isAncestorOfHover model renderable)
        ]
    ]

renderRoundNAppearance : Side -> Renderable -> Game -> Model -> Html Msg
renderRoundNAppearance side renderable game model =
  li
    [ classList
        [ (S.Appearance, True)
        , (S.CurrentHover, isHovering model renderable)
        , (S.AncestorHover, isAncestorOfHover model renderable)
        , (S.NotYetChosen, Maybe.Nothing == game.winner)
        ]
    , E.onClick <| clickWinner renderable
    , E.onMouseEnter <| MouseEntered renderable.round renderable.line
    , E.onMouseLeave <| MouseLeft renderable.round renderable.line
    ]
    [ gameText game ]

isHovering : Model -> Renderable -> Bool
isHovering model renderable =
  case model.hovered of
    Nothing -> False
    Just (round, line) ->
      let
          tester = round == renderable.round && line == renderable.line
      in
        case renderable.appearance of
          Seeded _ -> tester
          Winner game ->
            case game.winner of
              Nothing -> False
              Just _ -> tester


isAncestorOfHover : Model -> Renderable -> Bool
isAncestorOfHover model renderable =
  case model.hovered of
    Nothing -> False
    Just (round, line) ->
      let
        hoveredTeam = teamAt model round line
        currentTeam = extractTeam renderable.appearance
      in
        (not <| isHovering model renderable) &&
          (currentTeam
            |> Maybe.map2 (==) hoveredTeam
            |> Maybe.withDefault False)


gameSpacer : Html Msg
gameSpacer =
  li [class [S.GameSpacer]] [ text nbsp ]

spacer : Html Msg
spacer =
  li [class [S.Spacer]] [ text nbsp ]

randomizeButton : Html Msg
randomizeButton =
  li
    [ class [S.Randomizer] ]
    [ Html.div [ E.onClick ClickRandomize ] [ text "Randomize" ] ]

gameText : Game -> Html a
gameText =
  text << Maybe.withDefault nbsp << Maybe.map .name << .winner

teamText : Team -> Html a
teamText =
  text << .name

nbsp : String
nbsp = " "

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

isRightColumn : Column -> Bool
isRightColumn column =
  case column of
    RoundColumn _ x _ -> isRight x
    _ -> False

isRight : Side -> Bool
isRight side =
  case side of
    Left -> False
    Right -> True
