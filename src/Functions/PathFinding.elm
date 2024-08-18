module Functions.PathFinding exposing (setPathFindingInPlayField)

import Dict exposing (Dict)
import Functions.Coordinate exposing (getNextCoordinateForDirection)
import Functions.Direction exposing (findNextDirectionForGoingAroundUnSafe)
import Functions.PlayField.Get exposing (getMaybeStepsForCoordinateInPlayField, tryGetCellFromPlayFieldByCoordinate, tryGetCellFromPlayFieldByKey)
import Functions.PlayField.Set exposing (setStepsForCoordinateInPlayFieldIfEmpty)
import Models.Cell exposing (Cell, Coordinate)
import Types exposing (CellContent(..), Direction(..))


setPathFindingInPlayField : Coordinate -> Dict String Cell -> Dict String Cell
setPathFindingInPlayField heroSpot playField =
    let
        playFieldWithFirstRoundDone =
            setStepsAroundHero heroSpot playField

        startCoordinate =
            -- we start 1 right from the top left
            { heroSpot | rowNumber = heroSpot.rowNumber - 2, columnNumber = heroSpot.columnNumber - 1 }
    in
    fillStepsForRestOfPlayField startCoordinate playFieldWithFirstRoundDone


fillStepsForRestOfPlayField : Coordinate -> Dict String Cell -> Dict String Cell
fillStepsForRestOfPlayField startSpot playField =
    -- first we try to fill steps from given coordinate
    --
    goRightAroundAndFindLowestSteps goRightAroundAndFindLowestStepsStartDirection 0 3 startSpot playField


goRightAroundAndFindLowestSteps : Direction -> Int -> Int -> Coordinate -> Dict String Cell -> Dict String Cell
goRightAroundAndFindLowestSteps currentDirection doneSteps maxSteps currentCoordinate playField =
    let
        currentCellResult =
            tryGetCellFromPlayFieldByCoordinate currentCoordinate playField

        updatedPlayField =
            case currentCellResult of
                Err _ ->
                    playField

                Ok cell ->
                    if cell.content /= Empty then
                        playField

                    else
                        let
                            lowestStepsAround =
                                findLowestStepsAroundCoordinate currentCoordinate findLowestStepsAroundCoordinateStartDirection Nothing playField
                        in
                        case lowestStepsAround of
                            Nothing ->
                                playField

                            Just steps ->
                                setStepsForCoordinateInPlayFieldIfEmpty (steps + 1) currentCoordinate playField
    in
    if doneSteps == maxSteps then
        if currentDirection == goRightAroundAndFindLowestStepsEndDirection then
            updatedPlayField

        else
            let
                nextDirection =
                    findNextDirectionForGoingAroundUnSafe currentDirection

                nextCoordinate =
                    getNextCoordinateForDirection nextDirection currentCoordinate
            in
            goRightAroundAndFindLowestSteps nextDirection 0 maxSteps nextCoordinate updatedPlayField

    else
        let
            nextCoordinate =
                getNextCoordinateForDirection currentDirection currentCoordinate
        in
        goRightAroundAndFindLowestSteps currentDirection (doneSteps + 1) maxSteps nextCoordinate updatedPlayField


findLowestStepsAroundCoordinate : Coordinate -> Direction -> Maybe Int -> Dict String Cell -> Maybe Int
findLowestStepsAroundCoordinate coordinate currentDirection maybeSteps playField =
    let
        coordinateToCheck =
            getNextCoordinateForDirection currentDirection coordinate

        checkedMaybeSteps =
            getMaybeStepsForCoordinateInPlayField coordinateToCheck playField

        updatedMaybeSteps =
            case checkedMaybeSteps of
                Nothing ->
                    maybeSteps

                Just checkedSteps ->
                    case maybeSteps of
                        Nothing ->
                            Just checkedSteps

                        Just steps ->
                            Just (min checkedSteps steps)
    in
    if currentDirection == findLowestStepsAroundCoordinateEndDirection then
        updatedMaybeSteps

    else
        let
            nextDirection =
                findNextDirectionForGoingAroundUnSafe currentDirection
        in
        findLowestStepsAroundCoordinate coordinate nextDirection updatedMaybeSteps playField



-- Start and end directions for functions


findLowestStepsAroundCoordinateStartDirection : Direction
findLowestStepsAroundCoordinateStartDirection =
    Up


findLowestStepsAroundCoordinateEndDirection : Direction
findLowestStepsAroundCoordinateEndDirection =
    Left


goRightAroundAndFindLowestStepsStartDirection : Direction
goRightAroundAndFindLowestStepsStartDirection =
    Right


goRightAroundAndFindLowestStepsEndDirection : Direction
goRightAroundAndFindLowestStepsEndDirection =
    Up



-- First circle around hero for steps


setStepsAroundHero : Coordinate -> Dict String Cell -> Dict String Cell
setStepsAroundHero heroSpot playField =
    let
        straightSteps =
            1

        diagonalSteps =
            2

        upDone =
            setStepsForCoordinateInPlayFieldIfEmpty straightSteps (getNextCoordinateForDirection Up heroSpot) playField

        upRightDone =
            setStepsForCoordinateInPlayFieldIfEmpty diagonalSteps (getNextCoordinateForDirection UpRight heroSpot) upDone

        rightDone =
            setStepsForCoordinateInPlayFieldIfEmpty straightSteps (getNextCoordinateForDirection Right heroSpot) upRightDone

        downRightDone =
            setStepsForCoordinateInPlayFieldIfEmpty diagonalSteps (getNextCoordinateForDirection DownRight heroSpot) rightDone

        downDone =
            setStepsForCoordinateInPlayFieldIfEmpty straightSteps (getNextCoordinateForDirection Down heroSpot) downRightDone

        downLeftDone =
            setStepsForCoordinateInPlayFieldIfEmpty diagonalSteps (getNextCoordinateForDirection DownLeft heroSpot) downDone

        leftDone =
            setStepsForCoordinateInPlayFieldIfEmpty straightSteps (getNextCoordinateForDirection Left heroSpot) downLeftDone
    in
    setStepsForCoordinateInPlayFieldIfEmpty diagonalSteps (getNextCoordinateForDirection UpLeft heroSpot) leftDone
