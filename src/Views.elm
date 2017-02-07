module Views exposing (view)

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
  let
      _ = Debug.log "tournament" model.tournament
  in
    Html.main_
      [A.id "tournament"] <|
      [ Html.node "link" [ A.href "/style.css", A.rel "stylesheet"] []
      ] ++ tourney model.tournament

tourney : List Round -> List (Html Msg)
tourney =
  layout << htmlizeBracket
  --List.indexedMap renderRound

layout : List (List (Html Msg)) -> List (Html Msg)
layout bracket =
  bracket
    |> List.indexedMap (\num round -> Html.ul [ A.class ("round round-" ++ (toString num)) ] (spacer :: round))

htmlizeBracket : List Round -> List (List (Html Msg))
htmlizeBracket =
  List.indexedMap htmlizeRound

htmlizeRound : Int -> Round -> List (Html Msg)
htmlizeRound roundNum =
  List.concat << List.indexedMap htmlizeAppearance

htmlizeAppearance : Int -> Appearance -> List (Html Msg)
htmlizeAppearance lineNum =
  case lineNum % 2 of
    0 -> htmlizeTopLine
    _ -> htmlizeBottomLine

htmlizeTopLine : Appearance -> List (Html Msg)
htmlizeTopLine app =
  let
      teamName =
        case extractTeam app of
          Just team -> team.name
          Nothing -> "-"
  in
    [ Html.li
        [ A.class "game game-top" ]
        [ Html.text teamName
        ]
    ]

htmlizeBottomLine : Appearance -> List (Html Msg)
htmlizeBottomLine app =
  let
      (teamName, seed) =
        case extractTeam app of
          Just team -> (team.name, toString team.seed)
          Nothing -> ("-", "")
  in
    [ Html.li
        [ A.class "game game-spacer" ]
        [ Html.div
            [ A.class "team" ]
            [ Html.div
                [ A.class "seed" ]
                [ Html.text seed ]
            , Html.text teamName
            ]
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


-- renderRound : Int -> Round -> Html Msg
-- renderRound idx round =
--     Html.ul
--       [A.class <| "round round-" ++ (toString idx)] <|
--       (spacer :: (List.concat <| List.indexedMap renderTeam round))
-- 
-- renderTeam : Int -> Appearance -> List (Html Msg)
-- renderTeam idx app =
--   case idx % 2 of
--     1 ->
--       [ Html.li [ A.class "game game-bottom" ] []
--       , spacer
--       ]
--     _ ->
--       case app of
--         Seeded team ->
--           [ Html.li [ A.class "game game-top" ] []
--           , Html.li [ A.class "game game-spacer" ] []
--           ]
--         Winner game ->
--           [ Html.li [ A.class "game game-top" ] []
--           , Html.li [ A.class "game game-spacer" ] []
--           ]

spacer : Html Msg
spacer =
  Html.li [A.class "spacer"] [ Html.text " " ]

-- renderFinals : Appearance -> Appearance -> Appearance -> List (Html Msg)
-- renderFinals left champ winner =
--   [ Html.ul
--       [ A.class "round round-5 finals"]
--       [ Html.li
--           [ A.class "champion" ]
--           [ Html.text "-" ]
--       , Html.li
--           [ A.class "final-left" ]
--           [ Html.text "-" ]
--       , Html.li
--           [ A.class "final-right" ]
--           [ Html.text "-" ]
--       , Html.li
--           []
--           []
--       ]
--   ]
-- 
-- renderLevel : Side -> Int -> List Appearance -> Html Msg
-- renderLevel side level matchups =
--   let
--     tupleize x =
--       let
--         a = List.getAt 0 x
--         b = List.getAt 1 x
--       in
--         (a, b)
-- 
--     chunks =
--       matchups
--         |> List.groupsOf 2
--         |> List.map tupleize
-- 
--     isRight =
--       case side of
--         Left -> False
--         Right -> True
-- 
--   in
--     Html.ul
--       [ A.classList
--           [ ("round", True)
--           , ("round-right", isRight)
--           , ("round-" ++ (toString level), not isRight)
--           , ("round-" ++ (toString <| level + 6), isRight)
--           ]
--       ]
--       (spacer :: (List.concat <| List.map renderMatchup chunks))
-- 
-- spacer : Html Msg
-- spacer =
--   Html.li [A.class "spacer"] [ Html.text " " ]
-- 
-- renderMatchup : (Maybe Appearance, Maybe Appearance) -> List (Html Msg)
-- renderMatchup apps =
--   case apps of
--     (Just up, Just dn) ->
--       [ Html.li
--           [ A.class "game game-top", E.onClick <| PickWinner up ]
--           (teamNameForAppearance up)
--       , Html.li
--           [ A.class "game game-spacer", E.onClick <| PickWinner dn ]
--           [ Html.div [A.class "team"] ( teamNameForAppearance dn ) ]
--       , Html.li
--           [ A.class "game game-bottom" ]
--           [ ]
--       , spacer
--       ]
--     _ -> []
-- 
-- teamNameForAppearance : Appearance -> List (Html Msg)
-- teamNameForAppearance app =
--   case app of
--     Winner game ->
--       case game.winner of
--         Just team -> [Html.text team.name]
--         Nothing -> [Html.text "-"]
-- 
--     Seeded x ->
--       [ Html.span [A.class "seed"] [ Html.text <| toString x.seed ]
--       , Html.text <| x.name
--       ]
