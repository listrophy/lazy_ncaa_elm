module Models exposing
    ( ModalType(..)
    , Model
    , Randomizing(..)
    , clearAllWinners
    , model
    )

import Array exposing (Array)
import Models.Appearance exposing (..)
import Models.Bracket exposing (..)
import Models.Team exposing (..)
import Rando exposing (Rando)


type alias Model =
    { randomizing : Randomizing
    , bracket : Bracket
    , hovered : Maybe ( Int, Int )
    , showModal : Maybe ModalType
    }


type Randomizing
    = Halted
    | Starting
    | Randomizing Rando


type ModalType
    = WhatsThis


model : Model
model =
    { randomizing = Halted
    , bracket = teamArray
    , hovered = Nothing
    , showModal = Nothing
    }


clearAllWinners : Bracket -> Bracket
clearAllWinners =
    let
        ignoreRound0 f index =
            if index == 0 then
                identity

            else
                f
    in
    Array.indexedMap <| ignoreRound0 (Array.map (setWinner Nothing))


teamArray : Array Round
teamArray =
    let
        firstRound =
            [ Team "Duke" 1 1
            , Team "NCCU/NDSU" 1 16
            , Team "VCU" 1 8
            , Team "UCF" 1 9
            , Team "Miss St" 1 5
            , Team "Liberty" 1 12
            , Team "Virginia Tech" 1 4
            , Team "St. Louis" 1 13
            , Team "Maryland" 1 6
            , Team "Belmont/Temple" 1 11
            , Team "LSU" 1 3
            , Team "Yale" 1 14
            , Team "Louisville" 1 7
            , Team "Minnesota" 1 10
            , Team "Michigan St" 1 2
            , Team "Bradley" 1 15
            , Team "Gonzaga" 4 1
            , Team "FDU/PV A&M" 4 16
            , Team "Syracuse" 4 8
            , Team "Baylor" 4 9
            , Team "Marquette" 4 5
            , Team "Murray St" 4 12
            , Team "Florida St" 4 4
            , Team "Vermont" 4 13
            , Team "Buffalo" 4 6
            , Team "ASU/St. John's" 4 11
            , Team "Texas Tech" 4 3
            , Team "N Kentucky" 4 14
            , Team "Nevada" 4 7
            , Team "Florida" 4 10
            , Team "Michigan" 4 2
            , Team "Montana" 4 15
            , Team "Virginia" 2 1
            , Team "Gardner-Webb" 2 16
            , Team "Ole Miss" 2 8
            , Team "Oklahoma" 2 9
            , Team "Wisconsin" 2 5
            , Team "Oregon" 2 12
            , Team "Kansas St" 2 4
            , Team "UC Irvine" 2 13
            , Team "Villanova" 2 6
            , Team "St. Mary's" 2 11
            , Team "Purdue" 2 3
            , Team "Old Dominion" 2 14
            , Team "Cincinnati" 2 7
            , Team "Iowa" 2 10
            , Team "Tennessee" 2 2
            , Team "Colgate" 2 15
            , Team "N Carolina" 3 1
            , Team "Iona" 3 16
            , Team "Utah St" 3 8
            , Team "Washington" 3 9
            , Team "Auburn" 3 5
            , Team "New Mex St" 3 12
            , Team "Kansas" 3 4
            , Team "Northeastern" 3 13
            , Team "Iowa St" 3 6
            , Team "Ohio St" 3 11
            , Team "Houston" 3 3
            , Team "Georgia St" 3 14
            , Team "Wofford" 3 7
            , Team "Seton Hall" 3 10
            , Team "Kentucky" 3 2
            , Team "Abilene Chr" 3 15
            ]
                |> List.map (\x -> Appearance (Just x) False False)

        builder l =
            case l of
                [] ->
                    []

                [] :: tl ->
                    tl

                x :: tl ->
                    let
                        appearances =
                            List.repeat (List.length x // 2) <| Appearance Nothing False False
                    in
                    builder <| appearances :: l
    in
    builder [ firstRound ]
        |> List.reverse
        |> List.map Array.fromList
        |> Array.fromList
