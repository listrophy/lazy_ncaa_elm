module ModelsTests exposing (..)

import Expect
-- import Fuzz exposing (int, list, string, tuple)
import Models exposing (..)
import Test exposing (..)

all : Test
all =
  describe "nothing" []
--     describe "Models Suite"
--         [ describe "seedForRegionalGameLocation"
--             [ test "seed 1" <|
--                 \() ->
--                     Expect.equal 1 (Models.seedForRegionalGameLocation [Up, Up, Up, Up])
--             , test "seed 16" <|
--                 \() ->
--                     Expect.equal 16 (Models.seedForRegionalGameLocation [Up, Up, Up, Dn])
--             , test "seed 9" <|
--                 \() ->
--                     Expect.equal 9 (Models.seedForRegionalGameLocation [Up, Up, Dn, Dn])
--             , test "seed 2" <|
--                 \() ->
--                     Expect.equal 2 (Models.seedForRegionalGameLocation [Dn, Dn, Dn, Up])
--             , test "seed 3" <|
--                 \() ->
--                     Expect.equal 3 (Models.seedForRegionalGameLocation [Dn, Up, Dn, Up])
--             ]
-- 
--         ]
