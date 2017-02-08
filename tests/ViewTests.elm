module ViewTests exposing (..)

import Expect
-- import Fuzz exposing (int, list, string, tuple)
import Views exposing (..)
import Test exposing (..)

all : Test
all =
    describe "View"
        [ describe "leftRightBreak"
            [ test "simple test 1" <|
                \() ->
                  let
                      input =
                        [ [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
                        , [11, 22, 33, 44, 55, 66, 77, 88]
                        , [111, 222, 333, 444]
                        , [1111, 2222]
                        , [11111]
                        ]
                      output =
                        ( [ [ 1, 2, 3, 4, 5, 6, 7, 8 ]
                          , [ 11, 22, 33, 44 ]
                          , [ 111, 222 ]
                          ]
                        , [ 11111, 1111, 2222 ]
                        , [ [ 333, 444 ]
                          , [ 55, 66, 77, 88 ]
                          , [ 9, 10, 11, 12, 13, 14, 15, 16 ]
                          ]
                        )
                  in
                    Expect.equal
                      output
                      (leftRightBreak <| List.reverse input)

            ]

        ]
