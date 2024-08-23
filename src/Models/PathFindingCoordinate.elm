module Models.PathFindingCoordinate exposing (..)

import Models.Cell exposing (Coordinate)
import Types exposing (Direction)


type alias PathFindingCoordinate =
    { coordinate : Coordinate
    , lowestStepsAround : Maybe Int
    , directionToCheck : Direction
    }
