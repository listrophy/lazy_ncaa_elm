module TreeTransform exposing (transform)

import List.Extra as List

import Models exposing (Round, SubRound, Appearance(..))

transform : Appearance -> (List Round, Appearance, Appearance, Appearance, List Round)
transform app =
  case app of
    Winner game ->
      let
          (left, right) = game.appearances
          xformLeft = transformSubBracket left
          xformRight = transformSubBracket right
          noOp = ([], app, app, app, [])
      in
        case xformLeft of
          [winLeft] :: restLeft ->
            case xformRight of
              [winRight] :: restRight ->
                (restLeft, winLeft, app, winRight, restRight)
              _ -> noOp
          _ -> noOp

    Seeded team ->
      ([], app, app, app, [])

transformSubBracket : Appearance -> List Round
transformSubBracket app =
  case app of
    Seeded team ->
      [[app]]
    Winner game ->
      let
          (leftApp, rightApp) = game.appearances
          left = transformSubBracket leftApp
          right = transformSubBracket rightApp
          zipped = List.zip left right
          rest = List.map (\(r1,r2) -> List.append r1 r2) zipped
      in
         [app] :: rest
