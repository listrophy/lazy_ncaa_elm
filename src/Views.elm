module Views exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.CssHelpers
import Html.Events as E
import List.Extra as List
import Messages exposing (Msg(..))
import Models exposing (Appearance, Appearance(..), Game, Model, Round, Team)
import Style as S

{ id, class, classList } =
  Html.CssHelpers.withNamespace "lazyNcaa"


view : Model -> Html Msg
view model =
  Html.main_
    [ id S.Tournament ]
    ( tourney model.tournament )

tourney : Array Round -> List (Html Msg)
tourney =
  layout << groupIntoRounds << htmlizeAppearances

groupIntoRounds : List (List (Html Msg)) -> (List (List (Html Msg)), List (Html Msg), List (List (Html Msg)))
groupIntoRounds =
  leftRightBreak << List.reverse

layout : (List (List (Html Msg)), List (Html Msg), List (List (Html Msg))) -> List (Html Msg)
layout (left, finals, right) =
  let
    ul isRight num round =
      Html.ul
        [ classList
            [ (S.Round, True)
            , (S.RoundN num, True)
            , (S.RightHalf, isRight)
            ]
        ]
        (spacer :: round)
  in
    List.concat
       [ List.indexedMap (ul False) left
       , finalsHtml finals
       , List.reverse <| List.indexedMap (ul True) right
       ]

leftRightBreak : List (List a) -> (List (List a), List a, List (List a))
leftRightBreak bracket =
  case bracket of
    champion :: finalists :: rest ->
      let
          (left, right) = List.unzip <| List.map halve rest
      in
         (List.reverse left, List.concat [champion, finalists], List.reverse right)
    _ ->
      ([], [], [])

finalsHtml : List (Html Msg) -> List (Html Msg)
finalsHtml finals =
  case finals of
    champion :: left :: right :: [] ->
      [ Html.ul [ class [ S.Round, S.Finals ] ]
          [ champion
          , left
          , right
          , randomizeButton
          ]
      ]

    _ -> []

halve : List a -> (List a, List a)
halve list =
  List.splitAt ((List.length list) // 2) list

htmlizeAppearances : Array Round -> List (List (Html Msg))
htmlizeAppearances =
  Array.indexedMap htmlizeRound
    >> Array.toList

htmlizeRound : Int -> Round -> List (Html Msg)
htmlizeRound roundNum =
  if roundNum == 6 then
    htmlizeChampion
  else if roundNum == 5 then
    htmlizeFinals
  else
    htmlizeGenericRound roundNum

htmlizeChampion : Round -> List (Html Msg)
htmlizeChampion round =
  let
      team = Maybe.andThen extractTeam <| Array.get 0 round
      html x =
        [ Html.li
            [ class [S.Champion] ]
            [ Html.div
                [ class [S.Team] ]
                [ Html.text <| Maybe.withDefault "-" <| Maybe.map .name team ]
            ]
        ]
  in
     List.concat << Array.toList << Array.map html
      <| round

htmlizeFinals : Round -> List (Html Msg)
htmlizeFinals =
  let
      html i app =
        let
            team = extractTeam app
        in
          [ Html.li
              [ classList
                  [ (S.Finals, True)
                  , (S.FinalLeft, i == 0)
                  , (S.FinalRight, i /= 0)
                  ]
              , E.onClick <| PickWinner 5 i
              ]
              [ Html.div
                  [ class [S.Team] ]
                  [ Html.text <| Maybe.withDefault "-" <| Maybe.map .name team ]
              ]
          ]
  in
    List.concat << Array.toList << Array.indexedMap html

htmlizeGenericRound : Int -> Round -> List (Html Msg)
htmlizeGenericRound roundNum =
  List.concat << Array.toList << Array.indexedMap (htmlizeAppearance roundNum)

htmlizeAppearance : Int -> Int -> Appearance -> List (Html Msg)
htmlizeAppearance roundNum lineNum =
  case lineNum % 2 of
    0 -> htmlizeTopLine roundNum lineNum
    _ -> htmlizeBottomLine roundNum lineNum

htmlizeTopLine : Int -> Int -> Appearance -> List (Html Msg)
htmlizeTopLine roundNum lineNum app =
  let
      (teamName, seed) =
        case extractTeam app of
          Just team -> (team.name, toString team.seed)
          Nothing -> ("-", "")
      content =
        if roundNum == 0 then
          [ Html.span
              [ class [S.Seed] ]
              [ Html.text seed ]
          , Html.text teamName
          ]
        else
          [ Html.text teamName
          ]

  in
    [ Html.li
        [ class [S.Game, S.GameTop]
        , E.onClick <| PickWinner roundNum lineNum
        ]
        content
    ]

htmlizeBottomLine : Int -> Int -> Appearance -> List (Html Msg)
htmlizeBottomLine roundNum lineNum app =
  let
      (teamName, seed) =
        case extractTeam app of
          Just team -> (team.name, toString team.seed)
          Nothing -> ("-", "")
      content =
        if roundNum == 0 then
          [ Html.span
              [ class [S.Seed] ]
              [ Html.text seed ]
          , Html.text teamName
          ]
        else
          [ Html.text teamName
          ]
  in
    [ Html.li
        [ class [S.Game, S.GameSpacer]
        , E.onClick <| PickWinner roundNum lineNum
        ]
        [ Html.div
            [ class [S.Team] ]
            content
        ]
    , Html.li
        [ class [S.Game, S.GameBottom] ]
        []
    , spacer
    ]

extractTeam : Appearance -> Maybe Team
extractTeam app =
  case app of
    Seeded team ->
      Just team
    Winner game ->
      game.winner


spacer : Html Msg
spacer =
  Html.li [class [S.Spacer]] [ Html.text " " ]

randomizeButton : Html Msg
randomizeButton =
  Html.li
    [ class [S.Randomizer] ]
    [ Html.button [ E.onClick Randomize ] [ Html.text "Randomize" ] ]
