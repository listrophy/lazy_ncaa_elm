port module Main exposing (..)

import ModelsTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit ModelsTests.all


port emit : ( String, Value ) -> Cmd msg
