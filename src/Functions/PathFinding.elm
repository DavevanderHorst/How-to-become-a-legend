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
            -- we did 1 round, see setStepsAroundHero , so we start at round 2
            calculateRoundsNeededForPathFinding heroSpot playField.maxColumns playField.maxRows

        doneField =
            -- start at round 2
            -- 0 steps, since we start, 3 steps for our second round, before we switch direction.
            fillStepsForRestOfPlayField 0 3 2 calculatedRoundsNeeded startRoundCoordinate [] [] playFieldWithFirstRoundDone
    in
    { playField | field = doneField }


fillStepsForRestOfPlayField : Int -> Int -> Int -> Int -> Coordinate -> List Coordinate -> List (List Coordinate) -> Dict String Cell -> Dict String Cell
fillStepsForRestOfPlayField currentStep maxSteps currentRoundNumber maxRounds startSpot todoCoordinates coordinatesByRoundList playField =
    -- first we go right around, update playField and save all found PathFindingCoordinates in a list
    -- We save all these by round, and when we foldl, we can go back rounds.
    -- We go left around, doing this by starting with the last one we put in the list, and check all coordinates again.
    -- Then we go 1 round back, and see if there are any changes. If we found changes we keep going rounds back.
    -- Until we are with round 1, or no more changes are detected.
    -- We keep doing this for every round
    -- Coordinates that are not set in a round will be saved, and done last.
    let
        ( allRightAroundCoordinates, updatedPlayField ) =
            goRightAroundAndFindLowestSteps
                goRightAroundAndFindLowestStepsStartDirection
                currentStep
                maxSteps
                startSpot
                playField
                []

        ( roundDoneInPlayField, updatedTodoCoordinates ) =
            checkFoundCoordinatesAgain
                allRightAroundCoordinates
                todoCoordinates
                updatedPlayField

        finishedPlayField =
            checkOldRoundsForChanges coordinatesByRoundList roundDoneInPlayField

        finishedCoordinatesByRoundList =
            allRightAroundCoordinates :: coordinatesByRoundList
    in
    if currentRoundNumber == maxRounds then
        -- need to do left over coordinates here.
        checkLeftOverCoordinatesFirstTime updatedTodoCoordinates finishedPlayField

    else
        let
            newStartSpot =
                { startSpot | rowNumber = startSpot.rowNumber - 1, columnNumber = startSpot.columnNumber - 1 }
        in
        fillStepsForRestOfPlayField 0
            (maxSteps + 2)
            (currentRoundNumber + 1)
            maxRounds
            newStartSpot
            updatedTodoCoordinates
            finishedCoordinatesByRoundList
            finishedPlayField


checkOldRoundsForChanges : List (List Coordinate) -> Dict String Cell -> Dict String Cell
checkOldRoundsForChanges coordinatesByRoundList playField =
    let
        maybeRoundToCheck =
            List.head coordinatesByRoundList
    in
    case maybeRoundToCheck of
        Nothing ->
            playField

        Just roundToCheck ->
            let
                ( updatedPlayField, hasChanged ) =
                    setRoundAndCheckIfChanged roundToCheck playField
            in
            if hasChanged then
                checkOldRoundsForChanges (List.drop 1 coordinatesByRoundList) updatedPlayField

            else
                playField


setRoundAndCheckIfChanged : List Coordinate -> Dict String Cell -> ( Dict String Cell, Bool )
setRoundAndCheckIfChanged roundToCheck playField =
    let
        ( updatedPlayField, hasChanged ) =
            List.foldl checkCoordinateForStepChanges ( playField, False ) roundToCheck
    in
    if hasChanged then
        List.foldr checkCoordinateForStepChanges ( updatedPlayField, hasChanged ) roundToCheck

    else
        ( playField, False )


checkCoordinateForStepChanges : Coordinate -> ( Dict String Cell, Bool ) -> ( Dict String Cell, Bool )
checkCoordinateForStepChanges coordinate ( playField, hasChanged ) =
    let
        maybeNewLowestStepAround =
            findLowestStepsAroundCoordinate coordinate playField

        maybeCurrentSteps =
            getMaybeStepsForCoordinateInPlayField coordinate playField
    in
    case maybeNewLowestStepAround of
        -- notting found, only possible if it always was like this.
        Nothing ->
            ( playField, hasChanged )

        Just newLowestStepsAround ->
            case maybeCurrentSteps of
                Nothing ->
                    -- we had nothing before, so this is changed, and this coordinate is in the to do list.
                    -- we set it now, but keep it in the to do list
                    let
                        updatedPlayField =
                            setFoundStepsInPlayFieldByCoordinate newLowestStepsAround coordinate playField
                    in
                    ( updatedPlayField, True )

                Just currentSteps ->
                    -- we have oldSteps and newSteps, newSteps is from current coordinate, which is always 1 higher.
                    if currentSteps - 1 > newLowestStepsAround then
                        -- Found a change, so we set playField
                        let
                            updatedPlayField =
                                setFoundStepsInPlayFieldByCoordinate newLowestStepsAround coordinate playField
                        in
                        ( updatedPlayField, True )

                    else
                        -- notting changed
                        ( playField, hasChanged )


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


checkFoundCoordinatesAgain :
    List Coordinate
    -> List Coordinate
    -> Dict String Cell
    -> ( Dict String Cell, List Coordinate )
checkFoundCoordinatesAgain foundCoordinates toDoCoordinates playField =
    -- last checked cell is first in list, so we check foldl, meaning we go left around this time
    -- we need to make our round list here
    -- and we need to set to do coordinates
    List.foldl checkFoundCellAgainAndSetStepsInPlayField ( playField, toDoCoordinates ) foundCoordinates


checkFoundCellAgainAndSetStepsInPlayField :
    Coordinate
    -> ( Dict String Cell, List Coordinate )
    -> ( Dict String Cell, List Coordinate )
checkFoundCellAgainAndSetStepsInPlayField foundCoordinate ( playField, toDoCoordinates ) =
    -- we already checked everything, so we only look for steps around
    let
        maybeLowestStepsAround =
            findLowestStepsAroundCoordinate foundCoordinate playField
    in
    case maybeLowestStepsAround of
        Nothing ->
            -- if we found nothing this time, we found nothing last time as well.
            -- meaning we might have to check this coordinate later
            ( playField, foundCoordinate :: toDoCoordinates )

        Just lowestStepsAround ->
            -- Now found steps are always lowest, so we just set it again.
            let
                updatedPlayField =
                    setFoundStepsInPlayFieldByCoordinate lowestStepsAround foundCoordinate playField
            in
            ( updatedPlayField, toDoCoordinates )


addCoordinatesToCoordinatesToDoList : Int -> Direction -> Coordinate -> List Coordinate -> Dict String Cell -> List Coordinate
addCoordinatesToCoordinatesToDoList diff direction currentCoordinate coordinatesToDoList playField =
    -- we keep adding, until the difference is 1 or we meet an obstacle
    if diff == 1 then
        coordinatesToDoList

    else
        let
            getCellResult =
                tryGetCellFromPlayFieldByCoordinate currentCoordinate playField
        in
        case getCellResult of
            Err _ ->
                coordinatesToDoList

            Ok cell ->
                if isBlocked cell.content then
                    coordinatesToDoList

                else
                    let
                        updatedCoordinatesToDoList =
                            if List.member currentCoordinate coordinatesToDoList then
                                coordinatesToDoList

                            else
                                currentCoordinate :: coordinatesToDoList

                        nextCoordinate =
                            getNextCoordinateForDirection direction currentCoordinate
                    in
                    addCoordinatesToCoordinatesToDoList (diff - 1) direction nextCoordinate updatedCoordinatesToDoList playField


isBlocked : CellContent -> Bool
isBlocked content =
    case content of
        Empty ->
            False

        Hero ->
            True

        Monster _ ->
            False

        Obstacle _ ->
            True


goRightAroundAndFindLowestSteps :
    Direction
    -> Int
    -> Int
    -> Coordinate
    -> Dict String Cell
    -> List Coordinate
    -> ( List Coordinate, Dict String Cell )
goRightAroundAndFindLowestSteps currentDirection doneSteps maxSteps currentCoordinate playField foundCoordinatesList =
    let
        currentCellResult =
            tryGetCellFromPlayFieldByCoordinate currentCoordinate playField

        ( updatedFoundCoordinatesList, updatedPlayField ) =
            case currentCellResult of
                Err _ ->
                    ( foundCoordinatesList, playField )

                Ok cell ->
                    -- TODO also monster should be legal
                    if cell.content /= Empty then
                        ( foundCoordinatesList, playField )

                    else
                        let
                            maybeLowestStepsAround =
                                findLowestStepsAroundCoordinate currentCoordinate playField

                            updatedList =
                                currentCoordinate :: foundCoordinatesList
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
            ( updatedFoundCoordinatesList, updatedPlayField )

        else
            let
                nextDirection =
                    findNextDirectionForGoingAroundUnSafe currentDirection

                nextCoordinate =
                    getNextCoordinateForDirection nextDirection currentCoordinate
            in
            goRightAroundAndFindLowestSteps nextDirection 0 maxSteps nextCoordinate updatedPlayField updatedFoundCoordinatesList

    else
        let
            nextCoordinate =
                getNextCoordinateForDirection currentDirection currentCoordinate
        in
        goRightAroundAndFindLowestSteps currentDirection (doneSteps + 1) maxSteps nextCoordinate updatedPlayField updatedFoundCoordinatesList


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
