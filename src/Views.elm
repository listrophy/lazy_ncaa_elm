module Views exposing (view)

import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import List.Extra as List

import Messages exposing (Msg(..))
import Models exposing (Appearance, Appearance(..), Game, Model, Team)
import TreeTransform

type Side
  = Left
  | Right

view : Model -> Html Msg
view model =
  let
      (left_, leftWinner, champ, rightWinner, right) = TreeTransform.transform model.bracket
      left = List.reverse left_
  in
    Html.main_
      [A.id "tournament"]
      ([ Html.node "link" [ A.href "/style.css", A.rel "stylesheet"] []
      ] ++ (
        (List.indexedMap (renderLevel Left) left)) ++
        (renderFinals leftWinner champ rightWinner) ++
        (List.indexedMap (renderLevel Right) right)
      )

renderFinals : Appearance -> Appearance -> Appearance -> List (Html Msg)
renderFinals left champ winner =
  [ Html.ul
      [ A.class "round round-5 finals"]
      [ Html.li
          [ A.class "champion" ]
          [ Html.text "-" ]
      , Html.li
          [ A.class "final-left" ]
          [ Html.text "-" ]
      , Html.li
          [ A.class "final-right" ]
          [ Html.text "-" ]
      , Html.li
          []
          []
      ]
  ]

renderLevel : Side -> Int -> List Appearance -> Html Msg
renderLevel side level matchups =
  let
    tupleize x =
      let
        a = List.getAt 0 x
        b = List.getAt 1 x
      in
        (a, b)

    chunks =
      matchups
        |> List.groupsOf 2
        |> List.map tupleize

    isRight =
      case side of
        Left -> False
        Right -> True

  in
    Html.ul
      [ A.classList
          [ ("round", True)
          , ("round-right", isRight)
          , ("round-" ++ (toString level), not isRight)
          , ("round-" ++ (toString <| level + 6), isRight)
          ]
      ]
      (spacer :: (List.concat <| List.map renderMatchup chunks))

spacer : Html Msg
spacer =
  Html.li [A.class "spacer"] [ Html.text " " ]

renderMatchup : (Maybe Appearance, Maybe Appearance) -> List (Html Msg)
renderMatchup apps =
  case apps of
    (Just up, Just dn) ->
      [ Html.li
          [ A.class "game game-top", E.onClick <| PickWinner up ]
          (teamNameForAppearance up)
      , Html.li
          [ A.class "game game-spacer", E.onClick <| PickWinner dn ]
          [ Html.div [A.class "team"] ( teamNameForAppearance dn ) ]
      , Html.li
          [ A.class "game game-bottom" ]
          [ ]
      , spacer
      ]
    _ -> []

teamNameForAppearance : Appearance -> List (Html Msg)
teamNameForAppearance app =
  case app of
    Winner game ->
      case game.winner of
        Just team -> [Html.text team.name]
        Nothing -> [Html.text "-"]

    Seeded x ->
      [ Html.span [A.class "seed"] [ Html.text <| toString x.seed ]
      , Html.text <| x.name
      ]
