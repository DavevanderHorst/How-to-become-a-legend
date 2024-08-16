module Functions.Coordinate exposing (..)

import Models.Cell exposing (Coordinate)
import Types exposing (Direction(..))


getNextCoordinateForDirection : Direction -> Coordinate -> Coordinate
getNextCoordinateForDirection direction coordinate =
    case direction of
        Up ->
            -- means go up on screen, is down in our playField in rows
            { coordinate | rowNumber = coordinate.rowNumber - 1 }

        Down ->
            { coordinate | rowNumber = coordinate.rowNumber + 1 }

        Right ->
            { coordinate | columnNumber = coordinate.columnNumber + 1 }

        Left ->
            { coordinate | columnNumber = coordinate.columnNumber - 1 }
