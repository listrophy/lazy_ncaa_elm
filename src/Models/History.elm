module Models.History exposing (probabilityForHigherSeed)

import Dict exposing (Dict)


probabilityForHigherSeed : Int -> Int -> Int -> Maybe ( Int, Int )
probabilityForHigherSeed round highSeed lowSeed =
    if highSeed == lowSeed then
        Just ( 2, 1 )
    else
        Dict.get ( round, highSeed, lowSeed ) dict


dict : Dict ( Int, Int, Int ) ( Int, Int )
dict =
    Dict.fromList
        [ ( ( 1, 1, 16 ), ( 136, 135 ) )
        , ( ( 1, 2, 15 ), ( 136, 128 ) )
        , ( ( 1, 3, 14 ), ( 136, 115 ) )
        , ( ( 1, 4, 13 ), ( 136, 108 ) )
        , ( ( 1, 5, 12 ), ( 136, 89 ) )
        , ( ( 1, 6, 11 ), ( 136, 85 ) )
        , ( ( 1, 7, 10 ), ( 136, 84 ) )
        , ( ( 1, 8, 9 ), ( 136, 64 ) )
        , ( ( 2, 1, 8 ), ( 68, 55 ) )
        , ( ( 2, 1, 9 ), ( 67, 61 ) )
        , ( ( 2, 2, 7 ), ( 81, 55 ) )
        , ( ( 2, 2, 10 ), ( 47, 29 ) )
        , ( ( 2, 3, 6 ), ( 69, 41 ) )
        , ( ( 2, 3, 11 ), ( 46, 29 ) )
        , ( ( 2, 4, 5 ), ( 72, 40 ) )
        , ( ( 2, 4, 12 ), ( 36, 24 ) )
        , ( ( 2, 5, 13 ), ( 17, 14 ) )
        , ( ( 2, 6, 14 ), ( 16, 14 ) )
        , ( ( 2, 7, 15 ), ( 3, 2 ) )
        , ( ( 2, 9, 16 ), ( 1, 1 ) )
        , ( ( 2, 10, 15 ), ( 5, 5 ) )
        , ( ( 2, 11, 14 ), ( 5, 5 ) )
        , ( ( 2, 12, 13 ), ( 11, 8 ) )
        , ( ( 3, 1, 4 ), ( 52, 38 ) )
        , ( ( 3, 1, 5 ), ( 42, 36 ) )
        , ( ( 3, 1, 12 ), ( 19, 19 ) )
        , ( ( 3, 1, 13 ), ( 3, 3 ) )
        , ( ( 3, 2, 3 ), ( 42, 26 ) )
        , ( ( 3, 2, 6 ), ( 29, 23 ) )
        , ( ( 3, 2, 11 ), ( 15, 13 ) )
        , ( ( 3, 3, 7 ), ( 15, 9 ) )
        , ( ( 3, 3, 10 ), ( 13, 9 ) )
        , ( ( 3, 3, 15 ), ( 1, 1 ) )
        , ( ( 3, 4, 8 ), ( 9, 4 ) )
        , ( ( 3, 4, 9 ), ( 3, 2 ) )
        , ( ( 3, 5, 8 ), ( 2, 0 ) )
        , ( ( 3, 5, 9 ), ( 3, 1 ) )
        , ( ( 3, 6, 7 ), ( 7, 4 ) )
        , ( ( 3, 6, 10 ), ( 6, 4 ) )
        , ( ( 3, 7, 11 ), ( 4, 0 ) )
        , ( ( 3, 7, 14 ), ( 1, 1 ) )
        , ( ( 3, 8, 12 ), ( 1, 0 ) )
        , ( ( 3, 8, 13 ), ( 1, 1 ) )
        , ( ( 3, 9, 13 ), ( 1, 1 ) )
        , ( ( 3, 10, 11 ), ( 3, 2 ) )
        , ( ( 3, 10, 14 ), ( 1, 1 ) )
        , ( ( 4, 1, 2 ), ( 46, 23 ) )
        , ( ( 4, 1, 3 ), ( 23, 14 ) )
        , ( ( 4, 1, 6 ), ( 9, 7 ) )
        , ( ( 4, 1, 7 ), ( 4, 4 ) )
        , ( ( 4, 1, 10 ), ( 5, 4 ) )
        , ( ( 4, 1, 11 ), ( 7, 4 ) )
        , ( ( 4, 2, 4 ), ( 6, 2 ) )
        , ( ( 4, 2, 5 ), ( 3, 0 ) )
        , ( ( 4, 2, 8 ), ( 5, 2 ) )
        , ( ( 4, 2, 9 ), ( 1, 0 ) )
        , ( ( 4, 2, 12 ), ( 1, 1 ) )
        , ( ( 4, 3, 4 ), ( 5, 2 ) )
        , ( ( 4, 3, 5 ), ( 3, 2 ) )
        , ( ( 4, 3, 8 ), ( 1, 1 ) )
        , ( ( 4, 3, 9 ), ( 2, 2 ) )
        , ( ( 4, 4, 6 ), ( 3, 2 ) )
        , ( ( 4, 4, 7 ), ( 5, 2 ) )
        , ( ( 4, 4, 10 ), ( 2, 2 ) )
        , ( ( 4, 5, 6 ), ( 1, 1 ) )
        , ( ( 4, 5, 10 ), ( 1, 1 ) )
        , ( ( 4, 6, 8 ), ( 1, 0 ) )
        , ( ( 4, 7, 8 ), ( 1, 0 ) )
        , ( ( 4, 9, 11 ), ( 1, 0 ) )
        , ( ( 5, 1, 2 ), ( 18, 12 ) )
        , ( ( 5, 1, 3 ), ( 9, 5 ) )
        , ( ( 5, 1, 4 ), ( 9, 7 ) )
        , ( ( 5, 1, 5 ), ( 4, 4 ) )
        , ( ( 5, 1, 6 ), ( 2, 1 ) )
        , ( ( 5, 1, 7 ), ( 3, 2 ) )
        , ( ( 5, 1, 8 ), ( 2, 1 ) )
        , ( ( 5, 1, 9 ), ( 1, 1 ) )
        , ( ( 5, 1, 10 ), ( 1, 1 ) )
        , ( ( 5, 2, 3 ), ( 10, 6 ) )
        , ( ( 5, 2, 4 ), ( 1, 1 ) )
        , ( ( 5, 2, 5 ), ( 1, 0 ) )
        , ( ( 5, 2, 6 ), ( 2, 1 ) )
        , ( ( 5, 2, 8 ), ( 2, 0 ) )
        , ( ( 5, 2, 11 ), ( 1, 1 ) )
        , ( ( 5, 3, 4 ), ( 2, 2 ) )
        , ( ( 5, 3, 8 ), ( 1, 1 ) )
        , ( ( 5, 3, 11 ), ( 2, 2 ) )
        , ( ( 5, 4, 5 ), ( 1, 1 ) )
        , ( ( 5, 4, 6 ), ( 1, 0 ) )
        , ( ( 5, 5, 8 ), ( 1, 1 ) )
        , ( ( 5, 7, 8 ), ( 1, 1 ) )
        , ( ( 5, 8, 11 ), ( 1, 1 ) )
        ]
