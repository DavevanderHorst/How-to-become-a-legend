module Functions.PathFinding exposing (setPathFindingInPlayField)

import Dict exposing (Dict)
import Functions.Coordinate exposing (getNextCoordinateForDirection)
import Functions.Direction exposing (findNextDirectionForGoingAroundUnSafe)
import Functions.PlayField.Get exposing (getMaybeStepsForCoordinateInPlayField, tryGetCellFromPlayFieldByCoordinate)
import Functions.PlayField.Set exposing (setStepsForCoordinateInPlayFieldIfEmpty)
import Models.Cell exposing (Cell, Coordinate)
import Models.Level exposing (PlayField)
import Types exposing (CellContent(..), Direction(..))


setPathFindingInPlayField : Coordinate -> PlayField -> PlayField
setPathFindingInPlayField heroSpot playField =
    let
        -- first we set the first circle, so we have a start
        -- we calculate how many rounds we need to fully circle our field
        -- then we start circling
        playFieldWithFirstRoundDone =
            setStepsAroundHero heroSpot playField.field

        startRoundCoordinate =
            -- we start 1 right from the top left
            { heroSpot | rowNumber = heroSpot.rowNumber - 2, columnNumber = heroSpot.columnNumber - 1 }

        calculatedRoundsNeeded =
            -- we did 1 round, see setStepsAroundHero , so need to subtract 1.
            calculateRoundsNeededForPathFinding heroSpot playField.maxColumns playField.maxRows

        roundsNeeded =
            calculatedRoundsNeeded - 1

        doneField =
            --
            fillStepsForRestOfPlayField 0 3 1 roundsNeeded startRoundCoordinate [] playFieldWithFirstRoundDone
    in
    { playField | field = doneField }


fillStepsForRestOfPlayField : Int -> Int -> Int -> Int -> Coordinate -> List Coordinate -> Dict String Cell -> Dict String Cell
fillStepsForRestOfPlayField currentStep maxSteps currentRoundNumber maxRounds startSpot coordinatesToDo playField =
    -- first we go right around, and save all found cells in a list
    -- then we start with the last one we put in the list, and check all coordinates again.
    -- if a coordinate still is empty, we save it in a list to check again after we are done circling
    let
        ( rightAroundFoundCoordinateWithSteps, updatedPlayField ) =
            goRightAroundAndFindLowestSteps goRightAroundAndFindLowestStepsStartDirection currentStep maxSteps startSpot playField []

        ( roundDoneInPlayField, updatedCoordinatesToDo ) =
            checkFoundCoordinatesAgainAndSetStepsInPlayField rightAroundFoundCoordinateWithSteps coordinatesToDo updatedPlayField
    in
    if currentRoundNumber == maxRounds then
        -- we did all our rounds, now we start by checking our left over coordinates
        -- these are coordinates that couldn't be set by rounds, but might have a value around them now.
        checkLeftOverCoordinatesFirstTime updatedCoordinatesToDo roundDoneInPlayField
        --roundDoneInPlayField

    else
        let
            newStartSpot =
                { startSpot | rowNumber = startSpot.rowNumber - 1, columnNumber = startSpot.columnNumber - 1 }
        in
        fillStepsForRestOfPlayField 0 (maxSteps + 2) (currentRoundNumber + 1) maxRounds newStartSpot updatedCoordinatesToDo roundDoneInPlayField


checkLeftOverCoordinatesFirstTime : List Coordinate -> Dict String Cell -> Dict String Cell
checkLeftOverCoordinatesFirstTime coordinatesToCheck playField =
    checkLeftOverCoordinates True coordinatesToCheck playField


checkLeftOverCoordinates : Bool -> List Coordinate -> Dict String Cell -> Dict String Cell
checkLeftOverCoordinates isFirstRound coordinatesToCheck playField =
    -- leftover coordinates are set when rounds are over.
    -- we dont know where to start
    -- so we need to do this for all coordinates until notting is changed anymore
    let
        ( updatedPlayField, isChanged ) =
            List.foldl (checkLeftOverCoordinate isFirstRound) ( playField, False ) coordinatesToCheck
    in
    if isChanged then
        checkLeftOverCoordinates False coordinatesToCheck updatedPlayField

    else
        updatedPlayField


checkLeftOverCoordinate : Bool -> Coordinate -> ( Dict String Cell, Bool ) -> ( Dict String Cell, Bool )
checkLeftOverCoordinate isFirstRound coordinate ( playField, isChanged ) =
    let
        maybeLowestStepsAround =
            findLowestStepsAroundCoordinate coordinate playField
    in
    case maybeLowestStepsAround of
        Nothing ->
            ( playField, isChanged )

        Just lowestStepsAround ->
            if isFirstRound then
                setStepsForCheckLeftOverCoordinate lowestStepsAround coordinate playField

            else
                let
                    currentCoordinateSteps =
                        getMaybeStepsForCoordinateInPlayField coordinate playField
                in
                case currentCoordinateSteps of
                    Nothing ->
                        setStepsForCheckLeftOverCoordinate lowestStepsAround coordinate playField

                    Just currentSteps ->
                        -- current coordinate has given one extra when added to our playField.
                        if lowestStepsAround < (currentSteps - 1) then
                            setStepsForCheckLeftOverCoordinate lowestStepsAround coordinate playField

                        else
                            ( playField, isChanged )


setStepsForCheckLeftOverCoordinate : Int -> Coordinate -> Dict String Cell -> ( Dict String Cell, Bool )
setStepsForCheckLeftOverCoordinate steps coordinate playField =
    let
        updatedPlayField =
            setFoundStepsInPlayFieldByCoordinate steps coordinate playField
    in
    ( updatedPlayField, True )


checkFoundCoordinatesAgainAndSetStepsInPlayField : List ( Coordinate, Maybe Int ) -> List Coordinate -> Dict String Cell -> ( Dict String Cell, List Coordinate )
checkFoundCoordinatesAgainAndSetStepsInPlayField coordinateWithStepsList coordinatesToDo playField =
    -- last checked cell is first in list, so we check foldl
    List.foldl checkFoundCellAgainAndSetStepsInPlayField ( playField, coordinatesToDo ) coordinateWithStepsList


checkFoundCellAgainAndSetStepsInPlayField : ( Coordinate, Maybe Int ) -> ( Dict String Cell, List Coordinate ) -> ( Dict String Cell, List Coordinate )
checkFoundCellAgainAndSetStepsInPlayField ( coordinate, maybeSteps ) ( playField, coordinatesToDoList ) =
    -- we already checked everything, so we only look for steps around
    -- we need to save all found values.
    -- we check the found values if they are 1 lower or 1 higher then our current spot.
    -- if not, we stop, and we need to go redo set values
    let
        maybeLowestStepsAround =
            findLowestStepsAroundCoordinate coordinate playField
    in
    case maybeLowestStepsAround of
        Nothing ->
            ( playField, coordinate :: coordinatesToDoList )

        Just lowestStepsAround ->
            case maybeSteps of
                Nothing ->
                    let
                        updatedPlayField =
                            setFoundStepsInPlayFieldByCoordinate lowestStepsAround coordinate playField
                    in
                    ( updatedPlayField, coordinatesToDoList )

                Just oldSteps ->
                    let
                        leastSteps =
                            min lowestStepsAround oldSteps

                        updatedPlayField =
                            setFoundStepsInPlayFieldByCoordinate leastSteps coordinate playField
                    in
                    ( updatedPlayField, coordinatesToDoList )


goRightAroundAndFindLowestSteps : Direction -> Int -> Int -> Coordinate -> Dict String Cell -> List ( Coordinate, Maybe Int ) -> ( List ( Coordinate, Maybe Int ), Dict String Cell )
goRightAroundAndFindLowestSteps currentDirection doneSteps maxSteps currentCoordinate playField foundCoordinateWithStepsList =
    let
        currentCellResult =
            tryGetCellFromPlayFieldByCoordinate currentCoordinate playField

        ( updatedFoundCoordinatesWithStepsList, updatedPlayField ) =
            case currentCellResult of
                Err _ ->
                    ( foundCoordinateWithStepsList, playField )

                Ok cell ->
                    if cell.content /= Empty then
                        ( foundCoordinateWithStepsList, playField )

                    else
                        let
                            maybeLowestStepsAround =
                                findLowestStepsAroundCoordinate currentCoordinate playField

                            updatedList =
                                ( currentCoordinate, maybeLowestStepsAround ) :: foundCoordinateWithStepsList
                        in
                        case maybeLowestStepsAround of
                            Nothing ->
                                ( updatedList, playField )

                            Just steps ->
                                let
                                    newPlayField =
                                        setFoundStepsInPlayFieldByCoordinate steps currentCoordinate playField
                                in
                                ( updatedList, newPlayField )
    in
    if doneSteps == maxSteps then
        if currentDirection == goRightAroundAndFindLowestStepsEndDirection then
            ( updatedFoundCoordinatesWithStepsList, updatedPlayField )

        else
            let
                nextDirection =
                    findNextDirectionForGoingAroundUnSafe currentDirection

                nextCoordinate =
                    getNextCoordinateForDirection nextDirection currentCoordinate
            in
            goRightAroundAndFindLowestSteps nextDirection 0 maxSteps nextCoordinate updatedPlayField updatedFoundCoordinatesWithStepsList

    else
        let
            nextCoordinate =
                getNextCoordinateForDirection currentDirection currentCoordinate
        in
        goRightAroundAndFindLowestSteps currentDirection (doneSteps + 1) maxSteps nextCoordinate updatedPlayField updatedFoundCoordinatesWithStepsList


setFoundStepsInPlayFieldByCoordinate : Int -> Coordinate -> Dict String Cell -> Dict String Cell
setFoundStepsInPlayFieldByCoordinate steps coordinate playField =
    -- found steps is from a coordinate 1 away, so need to add one.
    -- we checked that cell is empty, so we know it is.
    setStepsForCoordinateInPlayFieldIfEmpty (steps + 1) coordinate playField


findLowestStepsAroundCoordinate : Coordinate -> Dict String Cell -> Maybe Int
findLowestStepsAroundCoordinate coordinate playField =
    findLowestStepsAroundCoordinateFunction coordinate findLowestStepsAroundCoordinateStartDirection Nothing playField


findLowestStepsAroundCoordinateFunction : Coordinate -> Direction -> Maybe Int -> Dict String Cell -> Maybe Int
findLowestStepsAroundCoordinateFunction coordinate currentDirection maybeSteps playField =
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
        findLowestStepsAroundCoordinateFunction coordinate nextDirection updatedMaybeSteps playField



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


calculateRoundsNeededForPathFinding : Coordinate -> Int -> Int -> Int
calculateRoundsNeededForPathFinding coordinate columns rows =
    let
        xRoundsNeeded =
            max (columns - coordinate.columnNumber) (coordinate.columnNumber - 1)

        yRoundsNeeded =
            max (rows - coordinate.rowNumber) (coordinate.rowNumber - 1)

        maxRoundsNeeded =
            max xRoundsNeeded yRoundsNeeded
    in
    maxRoundsNeeded
