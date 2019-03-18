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
            [ Team "Virginia" 1 1
            , Team "UMBC" 1 16
            , Team "Creighton" 1 8
            , Team "Kansas St" 1 9
            , Team "Kentucky" 1 5
            , Team "Davidson" 1 12
            , Team "Arizona" 1 4
            , Team "Buffalo" 1 13
            , Team "Miami (FL)" 1 6
            , Team "Loyola-Chi" 1 11
            , Team "Tennessee" 1 3
            , Team "Wright St" 1 14
            , Team "Nevada" 1 7
            , Team "Texas" 1 10
            , Team "Cincinnati" 1 2
            , Team "Georgia St" 1 15
            , Team "Xavier" 4 1
            , Team "NCCU/TXSO" 4 16
            , Team "Missouri" 4 8
            , Team "Florida St" 4 9
            , Team "Ohio St" 4 5
            , Team "S Dakota St" 4 12
            , Team "Gonzaga" 4 4
            , Team "UNC-Green" 4 13
            , Team "Houston" 4 6
            , Team "San Diego St" 4 11
            , Team "Michigan" 4 3
            , Team "Montana" 4 14
            , Team "Texas A&M" 4 7
            , Team "Providence" 4 10
            , Team "N Carolina" 4 2
            , Team "Lipscomb" 4 15
            , Team "Villanova" 2 1
            , Team "LIU/RAD" 2 16
            , Team "Va Tech" 2 8
            , Team "Alabama" 2 9
            , Team "W Virginia" 2 5
            , Team "Murray St" 2 12
            , Team "Wichita St" 2 4
            , Team "Marshall" 2 13
            , Team "Florida" 2 6
            , Team "STBON/UCLA" 2 11
            , Team "Texas Tech" 2 3
            , Team "SF Austin" 2 14
            , Team "Arkansas" 2 7
            , Team "Butler" 2 10
            , Team "Purdue" 2 2
            , Team "CS Fullerton" 2 15
            , Team "Kansas" 3 1
            , Team "Penn" 3 16
            , Team "Seton Hall" 3 8
            , Team "NC State" 3 9
            , Team "Clemson" 3 5
            , Team "New Mex St" 3 12
            , Team "Auburn" 3 4
            , Team "Charleston" 3 13
            , Team "TCU" 3 6
            , Team "ASU/SYR" 3 11
            , Team "Michigan St" 3 3
            , Team "Bucknell" 3 14
            , Team "Rhode Isl" 3 7
            , Team "Oklahoma" 3 10
            , Team "Duke" 3 2
            , Team "Iona" 3 15
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
