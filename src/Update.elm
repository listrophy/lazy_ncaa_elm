module Update exposing (update)

import Array exposing (Array)
import Array.Extra as Array
import Css.Elements exposing (line)
import Messages exposing (Msg(..))
import Models exposing (Model, Randomizing(..), clearAllWinners)
import Models.Appearance exposing (Appearance(..), extractTeam, mapSeedAndWinner, mapTeam, mapWinner, setHover, setWinner)
import Models.Bracket exposing (Bracket, Round, appAt)
import Models.Game exposing (Game)
import Models.Team exposing (Team)
import Monocle.Optional as Optional exposing (Optional)
import Rando exposing (Rando)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ randomizing, bracket } as model) =
    case msg of
        ClickRandomize ->
            model ! [ Rando.init StartRandomizing ]

        StartRandomizing seed ->
            startRandomizing seed model ! []

        RandomlyChooseNextGame _ ->
            randomlyChooseNextGame model ! []

        PickWinner roundNum lineNum ->
            pickWinner model roundNum lineNum ! []

        MouseEntered roundNum lineNum ->
            { model | bracket = Optional.modify (appAt roundNum lineNum) (setHover True) bracket } ! []

        MouseLeft roundNum lineNum ->
            { model | bracket = Optional.modify (appAt roundNum lineNum) (setHover False) bracket } ! []


startRandomizing : Rando -> Model -> Model
startRandomizing rando model =
    { model
        | bracket = clearAllWinners model.bracket
        , randomizing = Randomizing rando
    }


randomlyChooseNextGame : Model -> Model
randomlyChooseNextGame model =
    let
        nextUnchosenGame =
            model.bracket
                |> Array.detectIndex2d (mapSeedAndWinner (always False) (\g -> g.winner == Nothing))
    in
        case nextUnchosenGame of
            Nothing ->
                { model | randomizing = Halted }

            Just ( roundNum, lineNum ) ->
                { model | bracket = randomlyPickWinner roundNum lineNum model.bracket }


randomlyPickWinner : Int -> Int -> Bracket -> Bracket
randomlyPickWinner roundNum lineNum bracket =
    let
        appA =
            (appAt (roundNum - 1) (lineNum * 2)).getOption bracket

        appB =
            (appAt (roundNum - 1) (lineNum * 2 + 1)).getOption bracket
    in
        case ( appA, appB ) of
            ( Just a, Just b ) ->
                Optional.modify (appAt roundNum lineNum) (determiner a b) bracket

            _ ->
                bracket


determiner : Appearance -> Appearance -> dummy -> Appearance
determiner a b _ =
    let
        winner =
            Maybe.map2 strategy (extractTeam a) (extractTeam b)
    in
        Winner <| Game winner False 0 0


strategy : Team -> Team -> Team
strategy a b =
    if a.seed <= b.seed then
        a
    else
        b


pickWinner : Model -> Int -> Int -> Model
pickWinner ({ bracket } as model) roundNum lineNum =
    let
        teamChanger =
            mapTeam (assignWinnerInvalidatingPreviousWinner (roundNum + 1) (lineNum // 2))

        newBracket =
            (appAt roundNum lineNum).getOption model.bracket
                |> Maybe.map teamChanger
                |> Maybe.withDefault identity
    in
        { model | bracket = newBracket bracket }


assignWinnerInvalidatingPreviousWinner : Int -> Int -> Team -> Bracket -> Bracket
assignWinnerInvalidatingPreviousWinner roundNum lineNum winningTeam bracket =
    bracket
        |> vacateFutureWins roundNum lineNum winningTeam
        |> Optional.modify (appAt roundNum lineNum) (setWinner <| Just winningTeam)


vacateFutureWins : Int -> Int -> Team -> Bracket -> Bracket
vacateFutureWins roundNum lineNum winner bracket =
    bracket
        |> (appAt roundNum lineNum).getOption
        |> Maybe.andThen extractTeam
        |> Maybe.map
            (\x ->
                if x == winner then
                    bracket
                else
                    clearThisTeam roundNum lineNum x bracket
            )
        |> Maybe.withDefault bracket


clearThisTeam : Int -> Int -> Team -> Bracket -> Bracket
clearThisTeam roundNum lineNum clearable bracket =
    let
        clearAppearance =
            mapWinner
                (\g ->
                    if g.winner == Just clearable then
                        { g | winner = Nothing }
                    else
                        g
                )
    in
        if roundNum >= Array.length bracket then
            bracket
        else
            bracket
                |> Optional.modify (appAt roundNum lineNum) clearAppearance
                |> clearThisTeam (roundNum + 1) (lineNum // 2) clearable
