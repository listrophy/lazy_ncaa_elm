module Update exposing (update)

import Array exposing (Array)
import Array.Extra as Array
import Messages exposing (Msg(..))
import Models exposing (Model, Randomizing(..), clearAllWinners)
import Models.Appearance exposing (Appearance, Appearance(..), extractTeam, isUndecided, mapSeedAndWinner, mapTeam, mapWinner, setAncestorHover, setHover, setWinner)
import Models.Bracket exposing (Bracket, Round, appAt, round0line, teamAt)
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
            { model | bracket = setHovers roundNum lineNum True bracket } ! []

        MouseLeft roundNum lineNum ->
            { model | bracket = setHovers roundNum lineNum False bracket } ! []


setHovers : Int -> Int -> Bool -> Bracket -> Bracket
setHovers roundNum lineNum bool bracket =
    let
        app =
            appAt roundNum lineNum
    in
        bracket
            |> Optional.modify app (setHover bool)
            |> setAncestorHovers app bool


setAncestorHovers : Optional Bracket Appearance -> Bool -> Bracket -> Bracket
setAncestorHovers hoveredApp bool bracket =
    let
        currentHoveredTeam =
            Maybe.andThen extractTeam <| hoveredApp.getOption bracket

        round0 =
            Maybe.withDefault Array.empty <| Array.get 0 bracket

        teamName =
            Maybe.map .name currentHoveredTeam

        round0LineNum =
            Array.detectIndex
                (\app ->
                    case app of
                        Winner _ ->
                            False

                        Seeded t ->
                            Just t.name == teamName
                )
                round0
    in
        case round0LineNum of
            Nothing ->
                bracket

            Just num ->
                doSetAncestorHover 0 num teamName bool bracket


doSetAncestorHover : Int -> Int -> Maybe String -> Bool -> Bracket -> Bracket
doSetAncestorHover roundNum lineNum teamName bool bracket =
    let
        appOptional =
            appAt roundNum lineNum

        app =
            appOptional.getOption bracket

        currTeamName =
            Maybe.map .name <| Maybe.andThen extractTeam app
    in
        case app of
            Nothing ->
                bracket

            Just actualApp ->
                if Maybe.withDefault False <| Maybe.map2 (==) teamName currTeamName then
                    bracket
                        |> Optional.modify appOptional (setAncestorHover bool)
                        |> doSetAncestorHover (roundNum + 1) (lineNum // 2) teamName bool
                else
                    bracket


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
            Array.detectIndex2d isUndecided model.bracket
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


determiner : Appearance -> Appearance -> Appearance -> Appearance
determiner a b g =
    let
        winner =
            Maybe.map2 strategy (extractTeam a) (extractTeam b)
    in
        case g of
            Winner gg ->
                Winner { gg | winner = winner }

            Seeded t ->
                g


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

        hoveredBracket =
            (newBracket bracket)
                |> setHovers roundNum lineNum True
    in
        { model | bracket = hoveredBracket }


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
