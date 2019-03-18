module Style exposing (CssClasses(..), CssIds(..), almostWhite, appearanceBackground, appearanceColor, appearances, borderNNNY, borderNNYN, borderNNYY, borderNYNN, borderNYYN, borderYNNN, borderYNNY, borderYYNN, borderYYYY, borders, bottomColorOfBackground, centerize, connectorColor, css, finalFour, fontSize_, footer_, gray0, grayA, grayB, grayC, hoverBackgroundAncestor, hoverBackgroundCurrent, hoverColorCurrent, hovering, lightGray, modal, myButton, overallPage, overallTournament, px0, radius, randomize, roundColumns, rounding, seeds, spacers)

import Css exposing (..)
import Css.Global exposing (Snippet, body, children, class, descendants, div, footer, h1, id, li, main_, p, span, withClass)


type CssClasses
    = Round
    | RoundN Int
    | Appearance
    | NotYetChosen
    | Spacer
    | Winner
    | GameSpacer
    | WithRegionName
    | RightHalf
    | Team
    | Seed
    | Finals
    | Champion
    | Finalist
    | FinalLeft
    | FinalRight
    | Branding
    | Randomizer
    | CurrentHover
    | AncestorHover
    | CloseButton


type CssIds
    = Tournament
    | Modal


fontSize_ : Px
fontSize_ =
    px 12


css : List Snippet
css =
    List.concat
        [ overallPage
        , overallTournament
        , roundColumns
        , appearances
        , rounding
        , borders
        , spacers
        , seeds
        , finalFour
        , hovering
        , randomize
        , footer_
        , modal
        ]


overallPage : List Snippet
overallPage =
    [ body
        [ margin zero
        , fontFamily sansSerif
        , minHeight <| vh 100
        , backgroundColor grayA
        , children
            [ div
                [ backgroundImage <| url "/hoop.jpg"
                , backgroundRepeat noRepeat
                , backgroundColor bottomColorOfBackground
                , property "background-position" "center -40px"
                , minHeight <| vh 100
                , width <| px 1280
                , margin2 px0 auto
                , borderLeftWidth <| px 3
                , borderRightWidth <| px 3
                , borderTopWidth px0
                , borderBottomWidth px0
                , borderColor grayB
                , borderStyle solid
                ]
            ]
        ]
    ]


overallTournament : List Snippet
overallTournament =
    [ main_
        [ displayFlex
        , flexDirection row
        , padding <| px 10
        ]
    , class "RightHalf"
        [ textAlign right
        ]
    ]


roundColumns : List Snippet
roundColumns =
    [ class "Round"
        [ displayFlex
        , flexDirection column
        , justifyContent center
        , flex3 (int 1) zero (px 110)
        , listStyle none
        , padding zero
        , marginTop zero
        , marginBottom zero
        , minWidth <| px 110
        ]
    ]


appearances : List Snippet
appearances =
    [ class "Round"
        [ fontSize fontSize_
        ]
    , class "Appearance"
        [ backgroundColor appearanceBackground
        , color appearanceColor
        , lineHeight <| num 1.2
        , minHeight <| px 14
        , padding2 (px 1) radius
        , overflowX hidden
        , cursor pointer
        , border3 (px 1) solid grayA
        , withClass "NotYetChosen" [ cursor default ]
        ]
    , class "Champion"
        [ fontSize <| px 18
        , minHeight <| px 22
        ]
    ]


rounding : List Snippet
rounding =
    let
        nthTop =
            nthOfType "4n+2"

        nthBottom =
            nthOfType "4n"
    in
    [ class "Appearance"
        [ nthTop [ borderNYNN ]
        , nthBottom [ borderNNYN ]
        ]
    , class "RightHalf"
        [ descendants
            [ class "Appearance"
                [ nthTop [ borderYNNN ]
                , nthBottom [ borderNNNY ]
                ]
            ]
        ]
    , class "Round0"
        [ descendants
            [ class "Appearance"
                [ nthTop [ borderYYNN ]
                , nthBottom [ borderNNYY ]
                ]
            ]
        ]
    ]


borders : List Snippet
borders =
    [ class "Round0"
        [ descendants
            [ class "Appearance"
                [ borderLeftWidth <| px 1
                ]
            ]
        , withClass "RightHalf"
            [ descendants
                [ class "Appearance"
                    [ borderRightWidth <| px 1
                    ]
                ]
            ]
        ]
    , class "Appearance"
        [ borderLeftWidth px0
        ]
    , class "RightHalf"
        [ descendants
            [ class "Appearance"
                [ borderLeftWidth <| px 1
                , borderRightWidth px0
                ]
            ]
        ]
    , class "Finals"
        [ children
            [ class "Champion"
                [ borderLeftWidth <| px 1 ]
            , class "FinalRight"
                [ borderLeftWidth <| px 1
                , borderRightWidth px0
                ]
            ]
        ]
    ]


radius : Px
radius =
    px 4


px0 : Px
px0 =
    px 0


spacers : List Snippet
spacers =
    [ class "Spacer"
        [ flexGrow <| int 1
        , minHeight <| px 6
        , firstOfType [ flexGrow <| num 0.5 ]
        , lastChild [ flexGrow <| num 0.5 ]
        ]
    , class "GameSpacer"
        [ flexGrow <| num 1
        , minHeight <| px 2
        , borderRight3 (px 1) solid connectorColor
        , withClass "WithRegionName"
            [ displayFlex
            , flexDirection column
            , justifyContent center
            , padding2 zero (px 4)
            ]
        ]
    , class "Round3"
        [ descendants
            [ class "GameSpacer"
                [ fontSize <| px 24
                , lineHeight <| px 14
                ]
            ]
        ]
    , class "RightHalf"
        [ descendants
            [ class "GameSpacer"
                [ borderRightWidth <| px 0
                , borderLeft3 (px 1) solid connectorColor
                ]
            ]
        ]
    ]


seeds : List Snippet
seeds =
    [ class "Seed"
        [ fontSize <| px 9
        , width <| em 1.5
        , lineHeight fontSize_
        , display inlineBlock
        ]
    ]


hovering : List Snippet
hovering =
    [ class "CurrentHover"
        [ important <| hoverBackgroundCurrent
        , important <| hoverColorCurrent
        ]
    , class "AncestorHover"
        [ important <| hoverBackgroundAncestor
        ]
    ]


finalFour : List Snippet
finalFour =
    [ class "Finals"
        [ flexBasis <| px 130
        , justifyContent spaceAround
        , children
            [ li
                [ maxWidth <| pct 80
                , minWidth <| pct 80
                ]
            , class "FinalLeft" [ alignSelf flexStart, borderNYYN ]
            , class "FinalRight" [ alignSelf flexEnd, textAlign right, borderYNNY ]
            , class "Champion" [ centerize, borderYYYY, cursor default ]
            , class "Finalist" []
            , class "Branding" [ centerize, flexBasis <| px 150 ]
            ]
        ]
    , class "Randomizer"
        [ centerize
        , flexBasis <| px 100
        , displayFlex
        , flexDirection column
        , justifyContent center
        , descendants
            [ span
                [ paddingTop <| px 16
                , display block
                ]
            ]
        ]
    ]


footer_ : List Snippet
footer_ =
    [ footer
        [ textAlign center
        , fontSize <| pct 80
        , padding2 (px 18) zero
        ]
    ]


modal : List Snippet
modal =
    [ id "Modal"
        [ position absolute
        , top zero
        , left zero
        , right zero
        , bottom zero
        , displayFlex
        , justifyContent center
        , alignItems center
        , backgroundColor <| rgba 255 255 255 0.7
        , children
            [ div
                [ width <| px 300

                -- , height <| px 200
                , backgroundColor <| hex "fff"
                , borderRadius radius
                , padding <| px 8
                , boxShadow5 zero zero (px 10) (px 5) (hex "bbb")
                , descendants
                    [ class "CloseButton"
                        [ myButton
                        ]
                    , h1
                        [ textAlign center ]
                    , p
                        [ fontSize <| pct 90
                        , marginBottom <| em 2
                        ]
                    ]
                ]
            ]
        ]
    ]


centerize : Style
centerize =
    batch
        [ alignSelf center
        , textAlign center
        ]



-- border radius


borderYYYY : Style
borderYYYY =
    borderRadius radius


borderNYYN : Style
borderNYYN =
    borderRadius4 px0 radius radius px0


borderYNNY : Style
borderYNNY =
    borderRadius4 radius px0 px0 radius


borderNYNN : Style
borderNYNN =
    borderRadius4 px0 radius px0 px0


borderNNYN : Style
borderNNYN =
    borderRadius4 px0 px0 radius px0


borderYYNN : Style
borderYYNN =
    borderRadius4 radius radius px0 px0


borderNNYY : Style
borderNNYY =
    borderRadius4 px0 px0 radius radius


borderYNNN : Style
borderYNNN =
    borderRadius4 radius px0 px0 px0


borderNNNY : Style
borderNNNY =
    borderRadius4 px0 px0 px0 radius



-- Colors


connectorColor : Color
connectorColor =
    grayA


appearanceBackground : Color
appearanceBackground =
    gray0


appearanceColor : Color
appearanceColor =
    grayB


lightGray : Color
lightGray =
    hex "eee"


hoverBackgroundCurrent : Style
hoverBackgroundCurrent =
    backgroundColor grayB


hoverColorCurrent : Style
hoverColorCurrent =
    color almostWhite


hoverBackgroundAncestor : Style
hoverBackgroundAncestor =
    backgroundColor grayA


gray0 : Color
gray0 =
    hex "c9c9c9"


grayA : Color
grayA =
    hex "a3a3a3"


grayB : Color
grayB =
    hex "555555"


grayC : Color
grayC =
    hex "1a1a1a"


bottomColorOfBackground : Color
bottomColorOfBackground =
    almostWhite


almostWhite : Color
almostWhite =
    hex "fbfbfb"



-- button


myButton : Style
myButton =
    batch
        [ padding2 (px 12) (px 12)
        , cursor pointer
        , property "user-select" "none"
        , property "transition" "all 60ms ease-in-out"
        , textAlign center
        , whiteSpace noWrap
        , important <| textDecoration none
        , color <| almostWhite
        , backgroundColor <| grayB
        , border2 zero none
        , borderRadius <| px 4
        , fontWeight <| int 700
        , lineHeight <| num 1.3
        , property "appearance" "none"
        , hover
            [ property "transition" "all 60ms ease"
            , opacity <| num 0.85
            ]
        , active
            [ property "transition" "all 60ms ease"
            , opacity <| num 0.75
            ]
        , focus
            [ outline3 (px 1) dotted (hex "959595")
            , outlineOffset <| px -4
            ]
        ]


randomize : List Snippet
randomize =
    [ class "Randomizer"
        [ children
            [ div
                [ myButton
                ]
            ]
        ]
    ]
