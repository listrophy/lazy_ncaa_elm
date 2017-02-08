module Views exposing (..)

import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import List.Extra as List

import Messages exposing (Msg(..))
import Models exposing (Appearance, Appearance(..), Game, Model, Team, Round)
-- import TreeTransform

type Side
  = Left
  | Right

view : Model -> Html Msg
view model =
  Html.main_
    [A.id "tournament"] <|
    [ Html.node "link" [ A.href "/style.css", A.rel "stylesheet"] []
    ] ++ tourney model.tournament

tourney : List Round -> List (Html Msg)
tourney =
  layout << htmlizeBracket

layout : List (List (Html Msg)) -> List (Html Msg)
layout bracket =
  let
      (left, finals, right) = leftRightBreak <| List.reverse bracket
  in
     List.concat
        [ List.indexedMap (\num round -> Html.ul [ A.class ("round round-" ++ (toString num)) ] (spacer :: round)) left
        , finalsHtml finals
        , List.indexedMap (\num round -> Html.ul [ A.class ("round round-right round-" ++ (toString num)) ] (spacer :: round)) right
        ]

leftRightBreak : List (List a) -> (List (List a), List a, List (List a))
leftRightBreak bracket =
  case bracket of
    champion :: finalists :: rest ->
      let
          (left, right) = List.unzip <| List.map halve rest
      in
         (List.reverse left, List.concat [champion, finalists], right)
    _ ->
      ([], [], [])

finalsHtml : List (Html Msg) -> List (Html Msg)
finalsHtml finals =
  case finals of
    champion :: left :: right :: [] ->
      [ Html.ul [ A.class "round finals" ]
          [ champion
          , left
          , right
          , spacer
          ]
      ]

    _ -> []

halve : List a -> (List a, List a)
halve list =
  List.splitAt ((List.length list) // 2) list

htmlizeBracket : List Round -> List (List (Html Msg))
htmlizeBracket =
  List.indexedMap htmlizeRound

htmlizeRound : Int -> Round -> List (Html Msg)
htmlizeRound roundNum =
  if roundNum == 6 then
    htmlizeChampion
  else if roundNum == 5 then
    htmlizeFinals
  else
    htmlizeGenericRound roundNum

htmlizeChampion : Round -> List (Html Msg)
htmlizeChampion =
  let
      html x =
        [ Html.li
            [ A.class "champion" ]
            [ Html.div
                [ A.class "team" ]
                [ Html.text "-" ]
            ]
        ]
  in
     List.concat << List.map html

htmlizeFinals : Round -> List (Html Msg)
htmlizeFinals =
  let
      html i app =
        let
            team = extractTeam app
        in
          [ Html.li
              [ A.class <| "final " ++ (if i == 0 then "final-left" else "final-right") ]
              [ Html.div
                  [ A.class "team" ]
                  [ Html.text <| Maybe.withDefault "-" <| Maybe.map .name team ]
              ]
          ]
  in
    List.concat << List.indexedMap html

htmlizeGenericRound : Int -> Round -> List (Html Msg)
htmlizeGenericRound roundNum =
  List.concat << List.indexedMap (htmlizeAppearance roundNum)

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
          [ Html.span [ A.class "seed" ] [ Html.text seed ]
          , Html.text teamName
          ]
        else
          [ Html.text teamName
          ]

  in
    [ Html.li
        [ A.class "game game-top", E.onClick <| PickWinner roundNum lineNum ]
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
          [ Html.span [ A.class "seed" ] [ Html.text seed ]
          , Html.text teamName
          ]
        else
          [ Html.text teamName
          ]
  in
    [ Html.li
        [ A.class "game game-spacer", E.onClick <| PickWinner roundNum lineNum ]
        [ Html.div
            [ A.class "team" ]
            content
        ]
    , Html.li [ A.class "game game-bottom" ] []
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
  Html.li [A.class "spacer"] [ Html.text " " ]
