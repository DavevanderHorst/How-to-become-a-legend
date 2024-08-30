module Functions.Coordinate exposing (..)

import Functions.Base exposing (differenceBetweenNumbers)
import Models.Cell exposing (Coordinate)
import Types exposing (Direction(..))


getNextCoordinateForDirection : Direction -> Coordinate -> Coordinate
getNextCoordinateForDirection direction coordinate =
    case direction of
        Up ->
            -- means go up on screen, is down in our playField in rows
            { coordinate | rowNumber = coordinate.rowNumber - 1 }

        UpRight ->
            { coordinate | rowNumber = coordinate.rowNumber - 1, columnNumber = coordinate.columnNumber + 1 }

        Right ->
            { coordinate | columnNumber = coordinate.columnNumber + 1 }

        DownRight ->
            { coordinate | rowNumber = coordinate.rowNumber + 1, columnNumber = coordinate.columnNumber + 1 }

        Down ->
            { coordinate | rowNumber = coordinate.rowNumber + 1 }

        DownLeft ->
            { coordinate | rowNumber = coordinate.rowNumber + 1, columnNumber = coordinate.columnNumber - 1 }

        Left ->
            { coordinate | columnNumber = coordinate.columnNumber - 1 }

        UpLeft ->
            { coordinate | rowNumber = coordinate.rowNumber - 1, columnNumber = coordinate.columnNumber - 1 }


removeCoordinateFromList : Coordinate -> List Coordinate -> List Coordinate
removeCoordinateFromList coordinate coordinateList =
    List.filter (isNotSameCoordinate coordinate) coordinateList


isNotSameCoordinate : Coordinate -> Coordinate -> Bool
isNotSameCoordinate a b =
    if a.rowNumber == b.rowNumber && a.columnNumber == b.columnNumber then
        False

    else
        True


areCoordinatesNextToEachOther : Coordinate -> Coordinate -> Bool
areCoordinatesNextToEachOther coordinateOne coordinateTwo =
    if coordinateOne.columnNumber == coordinateTwo.columnNumber then
        if differenceBetweenNumbers coordinateOne.rowNumber coordinateTwo.rowNumber == 1 then
            True

        else
            False

    else if coordinateOne.rowNumber == coordinateTwo.rowNumber then
        if differenceBetweenNumbers coordinateOne.columnNumber coordinateTwo.columnNumber == 1 then
            True

        else
            False

    else
        False
