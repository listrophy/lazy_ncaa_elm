module TreeTransform exposing (transform)

import List.Extra as List

import Models exposing (Appearance(..))

transform : Appearance -> List (List Appearance)
transform app =
  app :: helper [app]
    |> List.reverse
    |> chunk
    |> List.map List.reverse

helper : List Appearance -> List Appearance
helper appList =
  case appList of
    [] -> []

    hd :: tl ->
      case hd of
        Winner game ->
          let (up, dn) = game.appearances
          in
            hd :: helper (List.append tl [up, dn])
        Seeded team ->
          appList

chunk : List Appearance -> List (List Appearance)
chunk appList =
  let
    length = List.length appList
    grouped = List.groupsOf (length // 2) appList
  in
    case grouped of
      [] -> []
      hd :: tl ->
        tl
          |> List.head
          |> Maybe.withDefault []
          |> chunk
          |> List.append [hd]
