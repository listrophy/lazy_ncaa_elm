module Update exposing (update)

import Array exposing (Array)
import Array.Extra as Array
import Messages exposing (Msg(..))
import Models exposing (Model, Randomizing(..), clearAllWinners)
import Models.Appearance exposing (Appearance, isUndecided, setAncestorHover, setHover, setWinner, sortAppearances)
import Models.Bracket exposing (Bracket, Round, appAt, round0line, teamAt)
import Models.Team exposing (Team)
import Monocle.Optional as Optional exposing (Optional)
import Rando exposing (Rando)
import Models.History exposing (probabilityForHigherSeed)


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
            Maybe.andThen .winner <| hoveredApp.getOption bracket

        round0 =
            Maybe.withDefault Array.empty <| Array.get 0 bracket

        teamName =
            Maybe.map .name currentHoveredTeam

        round0LineNum =
            Array.detectIndex
                (\app -> justsAreEqual teamName <| Maybe.map .name app.winner)
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
            Maybe.map .name <| Maybe.andThen .winner app
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
        case ( nextUnchosenGame, model.randomizing ) of
            ( Just ( roundNum, lineNum ), Randomizing rando ) ->
                let
                    ( rand, nextSeed ) =
                        Rando.step rando
                in
                    { model
                        | bracket = randomlyPickWinner rand roundNum lineNum model.bracket
                        , randomizing = Randomizing nextSeed
                    }

            _ ->
                { model | randomizing = Halted }


randomlyPickWinner : Float -> Int -> Int -> Bracket -> Bracket
randomlyPickWinner rand roundNum lineNum bracket =
    let
        appA =
            (appAt (roundNum - 1) (lineNum * 2)).getOption bracket

        appB =
            (appAt (roundNum - 1) (lineNum * 2 + 1)).getOption bracket
    in
        case ( appA, appB ) of
            ( Just a, Just b ) ->
                Optional.modify (appAt roundNum lineNum) (determiner rand roundNum a b) bracket

            _ ->
                bracket


andThen2 : (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
andThen2 callback a b =
    case ( a, b ) of
        ( Just aa, Just bb ) ->
            callback aa bb

        _ ->
            Nothing


determiner : Float -> Int -> Appearance -> Appearance -> Appearance -> Appearance
determiner rand roundNum a b =
    let
        ( highSeed, lowSeed ) =
            sortAppearances a b

        winner =
            andThen2 (strategy rand roundNum) (highSeed.winner) (lowSeed.winner)
    in
        setWinner winner


strategy : Float -> Int -> Team -> Team -> Maybe Team
strategy rand roundNum a b =
    case probabilityForHigherSeed roundNum a.seed b.seed of
        Just ( games, won ) ->
            if (toFloat won) / (toFloat games) > rand then
                Just a
            else
                Just b

        -- TODO: Start over, since we've never seen it
        Nothing ->
            Just a


pickWinner : Model -> Int -> Int -> Model
pickWinner ({ bracket } as model) roundNum lineNum =
    let
        winningTeam =
            teamAt roundNum lineNum bracket

        newBracket =
            assignWinnerInvalidatingPreviousWinner (roundNum + 1) (lineNum // 2) winningTeam bracket

        hoveredBracket =
            newBracket
                |> setHovers roundNum lineNum True
    in
        { model | bracket = hoveredBracket }


assignWinnerInvalidatingPreviousWinner : Int -> Int -> Maybe Team -> Bracket -> Bracket
assignWinnerInvalidatingPreviousWinner roundNum lineNum winningTeam =
    case winningTeam of
        Nothing ->
            identity

        Just team ->
            vacateFutureWins roundNum lineNum team
                >> Optional.modify (appAt roundNum lineNum) (setWinner winningTeam)


vacateFutureWins : Int -> Int -> Team -> Bracket -> Bracket
vacateFutureWins roundNum lineNum winner bracket =
    bracket
        |> (appAt roundNum lineNum).getOption
        |> Maybe.andThen .winner
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
            (\app ->
                if justsAreEqual app.winner (Just clearable) then
                    { app | winner = Nothing }
                else
                    app
            )
    in
        if roundNum >= Array.length bracket then
            bracket
        else
            bracket
                |> Optional.modify (appAt roundNum lineNum) clearAppearance
                |> clearThisTeam (roundNum + 1) (lineNum // 2) clearable


justsAreEqual : Maybe a -> Maybe a -> Bool
justsAreEqual aMaybe aMaybe2 =
    case ( aMaybe, aMaybe2 ) of
        ( Just x, Just y ) ->
            x == y

        _ ->
            False
