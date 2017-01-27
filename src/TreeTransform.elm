module TreeTransform exposing (transform)

import List.Extra as List

import Models exposing (Appearance(..))

transform : Appearance -> List (List Appearance)
transform app =
  let
    transformed = List.map List.reverse (chunk <| List.reverse <| app :: helper [app])
    _ = Debug.log "begin" <| List.length <| List.concat transformed
    _ = debugger transformed
  in
    transformed

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
        Ignore -> []

chunk : List Appearance -> List (List Appearance)
chunk appList =
  let
    length = List.length appList
    grouped = List.groupsOf (length // 2) appList
  in
    case grouped of
      [] -> []
      hd :: tl ->
        case tl of
          [x] -> List.append [hd] <| chunk x
          _ -> [] -- can't happen?

debugger : List (List Appearance) -> List String
debugger list =
  list
    |> List.map (\l-> List.map debugApp l)
    |> List.concat
    |> List.map (Debug.log ">")

debugApp : Appearance -> String
debugApp app =
  case app of
    Winner game -> toString game.location
    Seeded team -> (toString team.region) ++ ", " ++ (toString team.seed)
    Ignore -> "?"
