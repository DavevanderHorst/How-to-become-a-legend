module CoordinateTests exposing (..)

import Expect
import Functions.Coordinate exposing (getNextCoordinateForDirection)
import Functions.PathFinding exposing (calculateRoundsNeededForPathFinding)
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


calculateRoundsNeededForPathFindingTest : Test
calculateRoundsNeededForPathFindingTest =
    describe "Calculate how many rounds are needed to fill map with steps"
        [ test "should be max in width" <|
            \_ ->
                calculateRoundsNeededForPathFinding (Coordinate 2 1) 10 2
                    |> Expect.equal 8
        , test "should be max in height" <|
            \_ ->
                calculateRoundsNeededForPathFinding (Coordinate 1 1) 2 9
                    |> Expect.equal 19
        ]
