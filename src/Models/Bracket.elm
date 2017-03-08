module Models.Bracket exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Monocle.Common as Monocle
import Monocle.Optional as Optional exposing (Optional)
import Models.Appearance exposing (..)
import Models.Team exposing (..)


type alias Round =
    Array Appearance


type alias SubRound =
    Round


type alias Bracket =
    Array Round


appAt : Int -> Int -> Optional Bracket Appearance
appAt roundNum lineNum =
    let
        bracketToRound =
            Monocle.array roundNum

        roundToLine =
            Monocle.array lineNum
    in
        Optional.compose bracketToRound roundToLine


clearTeamAt : Int -> Int -> Team -> Bracket -> Bracket
clearTeamAt roundNum lineNum clearable roundArray =
    roundArray


teamAt : Int -> Int -> Bracket -> Maybe Team
teamAt round line =
    (appAt round line).getOption
        >> Maybe.andThen .winner


round0line : Maybe Team -> Bracket -> Maybe Int
round0line team bracket =
    let
        round0 =
            Maybe.withDefault Array.empty <| Array.get 0 bracket
    in
        case team of
            Nothing ->
                Nothing

            Just t ->
                let
                    teamsAreEqual mA app =
                        case ( mA, app.winner ) of
                            ( Just a, Just b ) ->
                                a.name == b.name

                            _ ->
                                False
                in
                    Array.detectIndex (teamsAreEqual team) round0
