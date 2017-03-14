module Models
    exposing
        ( Model
        , Randomizing(..)
        , ModalType(..)
        , model
        , clearAllWinners
        )

import Array exposing (Array)
import Models.Appearance exposing (..)
import Models.Team exposing (..)
import Models.Bracket exposing (..)
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
            [ Team "Villanova" 1 1
            , Team "MSM/New Orl" 1 16
            , Team "Wisconsin" 1 8
            , Team "Va. Tech" 1 9
            , Team "Virginia" 1 5
            , Team "UNC Wilm" 1 12
            , Team "Florida" 1 4
            , Team "E Tenn St" 1 13
            , Team "SMU" 1 6
            , Team "Prov/USC" 1 11
            , Team "Baylor" 1 3
            , Team "N. Mexico St." 1 14
            , Team "S. Carolina" 1 7
            , Team "Marquette" 1 10
            , Team "Duke" 1 2
            , Team "Troy" 1 15
            , Team "Gonzaga" 4 1
            , Team "S Dak St." 4 16
            , Team "Northwestern" 4 8
            , Team "Vanderbilt" 4 9
            , Team "Notre Dame" 4 5
            , Team "Princeton" 4 12
            , Team "W Virginia" 4 4
            , Team "Bucknell" 4 13
            , Team "Maryland" 4 6
            , Team "Xavier" 4 11
            , Team "Florida St" 4 3
            , Team "FGCU" 4 14
            , Team "St Mary's" 4 7
            , Team "VCU" 4 10
            , Team "Arizona" 4 2
            , Team "N Dakota" 4 15
            , Team "Kansas" 2 1
            , Team "NCC/UC Dav" 2 16
            , Team "Miami (FL)" 2 8
            , Team "Michigan St" 2 9
            , Team "Iowa St" 2 5
            , Team "Nevada" 2 12
            , Team "Purdue" 2 4
            , Team "Vermont" 2 13
            , Team "Creighton" 2 6
            , Team "Rhode Isl." 2 11
            , Team "Oregon" 2 3
            , Team "Iona" 2 14
            , Team "Michigan" 2 7
            , Team "Okla. St" 2 10
            , Team "Louisville" 2 2
            , Team "Jax'ville St" 2 15
            , Team "N Carolina" 3 1
            , Team "Texas Southern" 3 16
            , Team "Arkansas" 3 8
            , Team "Seton Hall" 3 9
            , Team "Minnesota" 3 5
            , Team "Middle Tenn." 3 12
            , Team "Butler" 3 4
            , Team "Winthrop" 3 13
            , Team "Cincinnati" 3 6
            , Team "Kan St/Wake F" 3 11
            , Team "UCLA" 3 3
            , Team "Kent St." 3 14
            , Team "Dayton" 3 7
            , Team "Wichita St" 3 10
            , Team "Kentucky" 3 2
            , Team "N Kentucky" 3 15
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
                            List.repeat ((List.length x) // 2) <| Appearance Nothing False False
                    in
                        builder <| appearances :: l
    in
        builder [ firstRound ]
            |> List.reverse
            |> List.map Array.fromList
            |> Array.fromList
