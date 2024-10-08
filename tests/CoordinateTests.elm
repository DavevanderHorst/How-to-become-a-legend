module CoordinateTests exposing (..)

import Expect
import Functions.Coordinate exposing (getNextCoordinateForDirection)
import Models.Cell exposing (Coordinate)
import Test exposing (..)
import Types exposing (Direction(..))


nextCoordinateTests : Test
nextCoordinateTests =
    let
        startCoordinate =
            Coordinate 3 7
    in
    describe "After pressing an arrow, we need to find next Coordinate"
        [ test "Pressed up" <|
            \_ ->
                getNextCoordinateForDirection Up startCoordinate
                    |> Expect.equal { startCoordinate | rowNumber = startCoordinate.rowNumber - 1 }
        , test "Pressed down" <|
            \_ ->
                getNextCoordinateForDirection Down startCoordinate
                    |> Expect.equal { startCoordinate | rowNumber = startCoordinate.rowNumber + 1 }
        , test "Pressed left" <|
            \_ ->
                getNextCoordinateForDirection Left startCoordinate
                    |> Expect.equal { startCoordinate | columnNumber = startCoordinate.columnNumber - 1 }
        , test "Pressed right" <|
            \_ ->
                getNextCoordinateForDirection Right startCoordinate
                    |> Expect.equal { startCoordinate | columnNumber = startCoordinate.columnNumber + 1 }
        ]
