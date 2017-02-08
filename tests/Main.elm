port module Main exposing (..)

import ModelsTests
import ViewTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit ViewTests.all


port emit : ( String, Value ) -> Cmd msg
