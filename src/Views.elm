module Views exposing (Column(..), Renderable, Side(..), appearanceText, clickWinner, columnize, gameSpacer, halve, isRight, isRightColumn, modal, modalView, myFooter, nbsp, randomizeButton, renderAppearance, renderBranding, renderChampion, renderColumn, renderColumn_, renderFinalist, renderGenericColumn, renderRound0Appearance, renderRoundNAppearance, renderablesToColumns, round0Elem, spacer, tourney, view)

import Array exposing (Array)
import Css.Global
import Html.Styled as Html exposing (Html, a, div, footer, h1, li, main_, p, span, styled, text, ul)
import Html.Styled.Attributes as A exposing (class, classList, id)
import Html.Styled.Events as E exposing (onClick)
import Html.Styled.Lazy
import List.Extra as List exposing (elemIndex)
import Messages exposing (Msg(..))
import Models exposing (ModalType(..), Model)
import Models.Appearance exposing (Appearance)
import Models.Bracket exposing (..)
import Style as S exposing (CssClasses(..))


type Side
    = Left
    | Right


type Column
    = RoundColumn Int Side (List Renderable)
    | FinalsColumn ( Renderable, Renderable, Renderable )


type alias Renderable =
    { round : Int
    , line : Int
    , appearance : Appearance
    }


view : Model -> Html Msg
view model =
    styled div
        []
        []
        ([ styled main_
            []
            [ id "Tournament" ]
            (tourney model)
         , myFooter model
         ]
            ++ [ Css.Global.global S.css ]
            ++ modalView model
        )


modalView : Model -> List (Html Msg)
modalView model =
    case model.showModal of
        Nothing ->
            []

        Just x ->
            modal x


myFooter : Model -> Html Msg
myFooter model =
    styled footer
        []
        []
        [ a
            [ onClick <| ShowModal WhatsThis, A.href "#" ]
            [ text "What's This?" ]
        , text " © 2019 "
        , a
            [ A.href "http://bendyworks.com" ]
            [ text "Bendyworks, Inc." ]
        , text " Written in "
        , a
            [ A.href "http://www.elm-lang.org"
            , A.target "_blank"
            ]
            [ text "Elm" ]
        , text ". Code available on "
        , a
            [ A.href "https://github.com/listrophy/lazy_ncaa_elm"
            , A.target "_blank"
            ]
            [ text "GitHub" ]
        ]


tourney : Model -> List (Html Msg)
tourney model =
    model.bracket
        |> columnize
        |> List.map renderColumn


columnize : Bracket -> List Column
columnize =
    Array.toIndexedList
        >> List.map (\( roundNum, round ) -> Array.indexedMap (Renderable roundNum) round |> Array.toList)
        >> renderablesToColumns 0


renderColumn : Column -> Html Msg
renderColumn =
    Html.Styled.Lazy.lazy <| renderColumn_


renderColumn_ : Column -> Html Msg
renderColumn_ column =
    case column of
        RoundColumn roundNum side renderables ->
            styled ul
                []
                [ classList
                    [ ( "Round", True )
                    , ( "RightHalf", isRightColumn column )
                    , ( "Round" ++ String.fromInt roundNum, True )
                    ]
                ]
                (renderGenericColumn roundNum side renderables)

        FinalsColumn ( leftFinalist, rightFinalist, champion ) ->
            styled ul
                []
                [ class "Round", class "Finals" ]
                [ renderBranding
                , renderFinalist Left leftFinalist
                , renderChampion champion
                , renderFinalist Right rightFinalist
                , randomizeButton
                ]


renderBranding : Html Msg
renderBranding =
    styled li
        []
        [ class "Branding" ]
        []


renderGenericColumn : Int -> Side -> List Renderable -> List (Html Msg)
renderGenericColumn roundNum side renderables =
    case renderables of
        top :: bottom :: tl ->
            [ spacer
            , renderAppearance roundNum side top
            , gameSpacer roundNum side <| List.length tl
            , renderAppearance roundNum side bottom
            ]
                ++ renderGenericColumn roundNum side tl

        [] ->
            [ spacer ]

        _ ->
            []


renderablesToColumns : Int -> List (List Renderable) -> List Column
renderablesToColumns roundNum list =
    case list of
        [ finalist1, finalist2 ] :: [ champion ] :: [] ->
            [ FinalsColumn ( finalist1, finalist2, champion ) ]

        hd :: tl ->
            let
                nextRound =
                    renderablesToColumns (roundNum + 1) tl

                ( left, right ) =
                    halve hd

                toColumn =
                    RoundColumn roundNum
            in
            List.concat
                [ [ toColumn Left left ]
                , nextRound
                , [ toColumn Right right ]
                ]

        _ ->
            []


renderChampion : Renderable -> Html Msg
renderChampion renderable =
    li
        [ classList
            [ ( "Champion", True )
            , ( "Appearance", True )
            , ( "CurrentHover", renderable.appearance.hovered )
            , ( "AncestorHover", renderable.appearance.ancestorHovered )
            ]
        , E.onMouseEnter <| MouseEntered renderable.round renderable.line
        , E.onMouseLeave <| MouseLeft renderable.round renderable.line
        ]
        [ appearanceText renderable.appearance ]


renderFinalist : Side -> Renderable -> Html Msg
renderFinalist side renderable =
    let
        winner =
            renderable.appearance.winner
    in
    li
        [ E.onClick <| clickWinner renderable
        , E.onMouseEnter <| MouseEntered renderable.round renderable.line
        , E.onMouseLeave <| MouseLeft renderable.round renderable.line
        , classList
            [ ( "Appearance", True )
            , ( "Finalist", True )
            , ( "FinalLeft", not <| isRight side )
            , ( "FinalRight", isRight side )
            , ( "CurrentHover", renderable.appearance.hovered )
            , ( "AncestorHover", renderable.appearance.ancestorHovered )
            , ( "NotYetChosen", Maybe.Nothing == winner )
            ]
        ]
        [ appearanceText renderable.appearance ]


renderAppearance : Int -> Side -> Renderable -> Html Msg
renderAppearance roundNum side renderable =
    if roundNum == 0 then
        renderRound0Appearance side renderable

    else
        renderRoundNAppearance side renderable


renderRound0Appearance : Side -> Renderable -> Html Msg
renderRound0Appearance side renderable =
    let
        maybeText f x =
            Maybe.withDefault "" <| Maybe.map f x.appearance.winner

        teamSpan =
            span [ class "Seed" ] [ text <| maybeText (String.fromInt << .seed) renderable ]

        seedSpan =
            span [ class "Team" ] [ text <| maybeText .name renderable ]
    in
    case side of
        Left ->
            round0Elem renderable
                [ teamSpan
                , seedSpan
                ]

        Right ->
            round0Elem renderable
                [ seedSpan
                , teamSpan
                ]


round0Elem : Renderable -> List (Html Msg) -> Html Msg
round0Elem renderable =
    li
        [ E.onClick <| clickWinner renderable
        , E.onMouseEnter <| MouseEntered renderable.round renderable.line
        , E.onMouseLeave <| MouseLeft renderable.round renderable.line
        , classList
            [ ( "Appearance", True )
            , ( "CurrentHover", renderable.appearance.hovered )
            , ( "AncestorHover", renderable.appearance.ancestorHovered )
            ]
        ]


renderRoundNAppearance : Side -> Renderable -> Html Msg
renderRoundNAppearance side renderable =
    li
        [ classList
            [ ( "Appearance", True )
            , ( "CurrentHover", renderable.appearance.hovered )
            , ( "AncestorHover", renderable.appearance.ancestorHovered )
            , ( "NotYetChosen", Maybe.Nothing == renderable.appearance.winner )
            ]
        , E.onClick <| clickWinner renderable
        , E.onMouseEnter <| MouseEntered renderable.round renderable.line
        , E.onMouseLeave <| MouseLeft renderable.round renderable.line
        ]
        [ appearanceText renderable.appearance ]


gameSpacer : Int -> Side -> Int -> Html Msg
gameSpacer roundNum side tlLength =
    if roundNum == 3 then
        let
            regionName =
                case ( side, tlLength ) of
                    ( Left, 2 ) ->
                        "East"

                    ( Left, 0 ) ->
                        "West"

                    ( Right, 2 ) ->
                        "South"

                    ( Right, 0 ) ->
                        "Midwest"

                    _ ->
                        nbsp
        in
        li [ class "GameSpacer", class "WithRegionName" ] [ span [] [ text regionName ] ]

    else
        li [ class "GameSpacer" ] [ text nbsp ]


spacer : Html Msg
spacer =
    li [ class "Spacer" ] [ text nbsp ]


randomizeButton : Html Msg
randomizeButton =
    li
        [ class "Randomizer" ]
        [ div [ E.onClick ClickRandomize ] [ text "Randomize" ]
        , span [] []
        , div [ E.onClick ClearBracket ] [ text "Clear Bracket" ]
        ]


nbsp : String
nbsp =
    " "


appearanceText : Appearance -> Html a
appearanceText appearance =
    text <| Maybe.withDefault "" <| Maybe.map .name appearance.winner


clickWinner : Renderable -> Msg
clickWinner { round, line } =
    PickWinner round line


halve : List a -> ( List a, List a )
halve list =
    List.splitAt (List.length list // 2) list


isRightColumn : Column -> Bool
isRightColumn column =
    case column of
        RoundColumn _ x _ ->
            isRight x

        _ ->
            False


isRight : Side -> Bool
isRight side =
    case side of
        Left ->
            False

        Right ->
            True


modal : ModalType -> List (Html Msg)
modal modalType =
    let
        h1text =
            "What's This?"

        p_text =
            "We generate a bracket based on historical matchups between seeds. That is, if seed A beats seed B 56% of the time, we'll choose seed A with 56% probability. We'll do that for all 63 matchups to create a customized bracket just for you! Don't like it? You can play around with the winners and even preselect winners prior to regenerating!"
    in
    [ div [ id "Modal" ]
        [ div []
            [ h1 [] [ text h1text ]
            , p [] [ text p_text ]
            , div [ class "CloseButton", E.onClick DismissModal ] [ text "Got it!" ]
            ]
        ]
    ]
