module Models.Appearance exposing (..)

import Models.Game exposing (..)
import Models.Team exposing (..)


type Appearance
    = Winner Game
    | Seeded Team


setHover : Bool -> Appearance -> Appearance
setHover bool =
    mapSeedAndWinner (\t -> Seeded { t | hovered = bool }) (\g -> Winner { g | hovered = bool })


getHover : Appearance -> Bool
getHover =
    mapSeedAndWinner .hovered .hovered


setWinner : Maybe Team -> Appearance -> Appearance
setWinner teamMaybe =
    mapWinner (\g -> { g | winner = teamMaybe })


mapWinner : (Game -> Game) -> Appearance -> Appearance
mapWinner f app =
    case app of
        Seeded _ ->
            app

        Winner g ->
            Winner <| f g


mapTeam : (Team -> bracket -> bracket) -> Appearance -> bracket -> bracket
mapTeam f app =
    case app of
        Seeded t ->
            f t

        Winner g ->
            case g.winner of
                Nothing ->
                    identity

                Just t ->
                    f t


mapSeedAndWinner : (Team -> a) -> (Game -> a) -> Appearance -> a
mapSeedAndWinner fSeed fWinner app =
    case app of
        Seeded t ->
            fSeed t

        Winner g ->
            fWinner g


extractTeam : Appearance -> Maybe Team
extractTeam appearance =
    case appearance of
        Seeded team ->
            Just team

        Winner game ->
            game.winner
