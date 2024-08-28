module ListTests exposing (..)

import Expect
import Functions.Coordinate exposing (removeCoordinateFromList)
import Models.Cell exposing (Coordinate)
import Test exposing (Test, describe, test)


nextCoordinateTests : Test
nextCoordinateTests =
    let
        coordinateOne =
            Coordinate 1 1

        coordinateTwo =
            Coordinate 3 3

        testList =
            [ coordinateOne, coordinateTwo ]
    in
    describe "Removing a coordinate from a list by filtering"
        [ test "Removing (1,1)" <|
            \_ ->
                removeCoordinateFromList coordinateOne testList
                    |> Expect.equal [ coordinateTwo ]
        ]
