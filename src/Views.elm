module Views exposing (view)

import Html exposing (Html)
import Html.Attributes as A
import List.Extra as List
import Messages exposing (Msg)
import Models exposing (Appearance, Appearance(..), Game, Model, Team)
import TreeTransform

import List.Extra as List

view : Model -> Html Msg
view model =
  let
      levels = TreeTransform.transform model.bracket
      -- _ = Debug.log "levels" <| levels
  in
    Html.main_ [A.id "tournament"]
      ([ Html.node "link" [ A.href "/style.css", A.rel "stylesheet"] []
      ] ++ (List.indexedMap renderLevel levels))

parseBracket : List (List Appearance) -> Appearance -> List (List Appearance)
parseBracket memo appearance =
  case appearance of
    Ignore -> []
    Seeded team ->
      [[appearance]]
    Winner game ->
      let
        (appUp, appDn) = game.appearances
      in
        [appearance] :: [[appUp, appDn]]
  -- [bracket] :: (Tuple.second <| mergeAppearance bracket)

mergeParsed : List (List Appearance) -> List (List Appearance) -> List (List Appearance)
mergeParsed appUp appDn =
  let
    headUp = Maybe.withDefault [Ignore] <| List.head appUp
    tailUp = Maybe.withDefault [] <| List.tail appUp
    headDn = Maybe.withDefault [Ignore] <| List.head appDn
    tailDn = Maybe.withDefault [] <| List.tail appDn
  in
    (headUp ++ headDn) :: (tailUp ++ tailDn)

mergeAppearance : Appearance -> (Appearance, List (List Appearance))
mergeAppearance app =
  case app of
    Seeded team ->
      (app, [])
    Winner game ->
      let
        (heads, rest) = mergeAppearances game.appearances
      in
        (app, heads :: rest)
    Ignore -> (app, [])

mergeAppearances : (Appearance, Appearance) -> (List Appearance, List (List Appearance))
mergeAppearances (upapp, dnapp) =
  let
    (uphead, uprest) = mergeAppearance upapp
    (dnhead, dnrest) = mergeAppearance dnapp
    head = [uphead, dnhead]
    rest = uprest ++ dnrest
  in
    (head, rest)


parseAppearance : List (List (Appearance)) -> Appearance -> List (List Appearance)
parseAppearance memo appearance =
  case appearance of
    Seeded team ->
      [[appearance]]
    Winner game ->
      let
        (up, dn) = game.appearances
        upapp = parseAppearance memo up
        dnapp = parseAppearance upapp dn
      in
        upapp ++ dnapp
    Ignore -> []

renderLevel : Int -> List Appearance -> Html Msg
renderLevel level matchups =
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
  in
    Html.ul [A.class <| "round round-" ++ toString level]
      (spacer :: (List.concat <| List.map renderMatchup chunks))

spacer : Html Msg
spacer =
  Html.li [A.class "spacer"] [ Html.text " " ]

renderMatchup : (Maybe Appearance, Maybe Appearance) -> List (Html Msg)
renderMatchup apps =
  case apps of
    (Just up, Just dn) ->
      [ Html.li [A.class "game game-top"] [ Html.text <| teamNameForAppearance up]
      , Html.li [A.class "game game-spacer"] [ Html.text " " ]
      , Html.li [A.class "game game-bottom"] [ Html.text <| teamNameForAppearance dn]
      , spacer
      ]
    _ -> []

teamNameForAppearance : Appearance -> String
teamNameForAppearance app =
  case app of
    -- Winner x -> Maybe.withDefault "-" <| Maybe.map .name x.winner
    Winner x -> toString x.location
    Seeded x -> x.name
    Ignore -> "?"

spacePrefix : Int -> String
spacePrefix depth =
  String.repeat (depth * 2) " "

-- getTeams : (Winner, Winner) -> (Game, Game)
-- getTeams (a_, b_) =
--   case a_ of
--     Winner a ->
--       case b_ of
--         Winner b ->
--           (a,b)
