module Functions.PathFinding exposing (..)

import Dict exposing (Dict)
import Functions.Coordinate exposing (emptyCoordinate, getNextCoordinateForDirection, isSameCoordinate)
import Functions.Direction exposing (findNextDirectionForGoingAroundUnSafe)
import Functions.Monsters.MonsterDict exposing (tryGetMonsterFromMonsterDictByCoordinate, tryGetMonsterFromMonsterDictByKey)
import Functions.PlayField.Get exposing (getMaybeStepsForCoordinateInField, tryGetCellFromFieldByCoordinate, tryGetCellFromFieldByKey)
import Functions.PlayField.Helpers exposing (makeDictKeyFromCoordinate)
import Functions.PlayField.Set exposing (setStepsForCoordinateInFieldIfEmptyOrMovingMonster, setStepsForCoordinateInFieldUnsafe, trySetStepsForCoordinateInFieldIfEmptyOrMovingMonster)
import Models.Cell exposing (Cell, Coordinate, CoordinateWithStep)
import Models.Level exposing (PlayField)
import Models.Monster exposing (MonsterModel)
import Types exposing (Action(..), CellContent(..), Direction(..))


setPathFindingInPlayField : Coordinate -> PlayField -> Dict String MonsterModel -> PlayField
setPathFindingInPlayField heroSpot playField monsterDict =
    let
        -- first we set the first circle, so we have a start
        -- we calculate how many rounds we need to fully circle our field
        -- then we start circling
        ( fieldWithFirstRoundDone, leftOverCoordinates ) =
            setStepsAroundHero heroSpot playField.field monsterDict

        startRoundCoordinate =
            -- we start 1 right from the top left
            { heroSpot | rowNumber = heroSpot.rowNumber - 2, columnNumber = heroSpot.columnNumber - 1 }

        calculatedRoundsNeeded =
            -- we did 1 round, see setStepsAroundHero , so we start at round 2
            calculateRoundsNeededForPathFinding heroSpot playField.maxColumns playField.maxRows

        doneField =
            -- start at round 2
            -- 0 steps, since we start, 3 steps for our second round, before we switch direction.
            fillStepsForRestOfField 0 3 2 calculatedRoundsNeeded startRoundCoordinate leftOverCoordinates [] fieldWithFirstRoundDone monsterDict
    in
    { playField | field = doneField }


fillStepsForRestOfField : Int -> Int -> Int -> Int -> Coordinate -> List Coordinate -> List (List Coordinate) -> Dict String Cell -> Dict String MonsterModel -> Dict String Cell
fillStepsForRestOfField currentStep maxSteps currentRoundNumber maxRounds startSpot todoCoordinates coordinatesByRoundList playField monsterDict =
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
                monsterDict

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
        fillStepsForRestOfField 0
            (maxSteps + 2)
            (currentRoundNumber + 1)
            maxRounds
            newStartSpot
            updatedTodoCoordinates
            finishedCoordinatesByRoundList
            finishedPlayField
            monsterDict


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
            getMaybeStepsForCoordinateInField coordinate playField
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
                            setOneHigherStepForCoordinateInPlayFieldUnsafe newLowestStepsAround coordinate playField
                    in
                    ( updatedPlayField, True )

                Just currentSteps ->
                    -- we have oldSteps and newSteps, newSteps is from current coordinate, which is always 1 higher.
                    if currentSteps - 1 > newLowestStepsAround then
                        -- Found a change, so we set playField
                        let
                            updatedPlayField =
                                setOneHigherStepForCoordinateInPlayFieldUnsafe newLowestStepsAround coordinate playField
                        in
                        ( updatedPlayField, True )

                    else
                        -- notting changed
                        ( playField, hasChanged )


setOneHigherStepForCoordinateInPlayFieldUnsafe : Int -> Coordinate -> Dict String Cell -> Dict String Cell
setOneHigherStepForCoordinateInPlayFieldUnsafe steps coordinate field =
    setStepsForCoordinateInFieldUnsafe coordinate (steps + 1) field


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
                        getMaybeStepsForCoordinateInField coordinate playField
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
            setOneHigherStepForCoordinateInPlayFieldUnsafe steps coordinate playField
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
                    setStepsForCoordinateInFieldUnsafe foundCoordinate (lowestStepsAround + 1) playField
            in
            ( updatedPlayField, toDoCoordinates )


addCoordinatesToCoordinatesToDoList : Int -> Direction -> Coordinate -> List Coordinate -> Dict String Cell -> Dict String MonsterModel -> List Coordinate
addCoordinatesToCoordinatesToDoList diff direction currentCoordinate coordinatesToDoList field monsterDict =
    -- we keep adding, until the difference is 1 or we meet an obstacle
    if diff == 1 then
        coordinatesToDoList

    else
        let
            getCellResult =
                tryGetCellFromFieldByCoordinate currentCoordinate field
        in
        case getCellResult of
            Err _ ->
                coordinatesToDoList

            Ok cell ->
                if isBlocked cell.content currentCoordinate monsterDict then
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
                    addCoordinatesToCoordinatesToDoList (diff - 1) direction nextCoordinate updatedCoordinatesToDoList field monsterDict


isBlocked : CellContent -> Coordinate -> Dict String MonsterModel -> Bool
isBlocked content coordinate monsterDict =
    case content of
        Empty ->
            False

        Hero ->
            True

        Monster ->
            let
                getMonsterResult =
                    tryGetMonsterFromMonsterDictByCoordinate coordinate monsterDict
            in
            case getMonsterResult of
                Err _ ->
                    -- Should not be possible, we check if monsters are on right position in monsters turn.
                    False

                Ok monster ->
                    case monster.action of
                        Moving ->
                            False

                        Attacking ->
                            True

        Obstacle _ ->
            True


goRightAroundAndFindLowestSteps :
    Direction
    -> Int
    -> Int
    -> Coordinate
    -> Dict String Cell
    -> List Coordinate
    -> Dict String MonsterModel
    -> ( List Coordinate, Dict String Cell )
goRightAroundAndFindLowestSteps currentDirection doneSteps maxSteps currentCoordinate playField foundCoordinatesList monsterDict =
    let
        dictKey =
            makeDictKeyFromCoordinate currentCoordinate

        currentCellResult =
            tryGetCellFromFieldByKey dictKey playField

        ( updatedFoundCoordinatesList, updatedPlayField ) =
            case currentCellResult of
                Err _ ->
                    ( foundCoordinatesList, playField )

                Ok cell ->
                    case cell.content of
                        Empty ->
                            findLowestSteps currentCoordinate foundCoordinatesList playField

                        Hero ->
                            ( foundCoordinatesList, playField )

                        Monster ->
                            let
                                getMonsterResult =
                                    tryGetMonsterFromMonsterDictByKey dictKey monsterDict
                            in
                            case getMonsterResult of
                                Err _ ->
                                    ( foundCoordinatesList, playField )

                                Ok monster ->
                                    case monster.action of
                                        Moving ->
                                            findLowestSteps currentCoordinate foundCoordinatesList playField

                                        Attacking ->
                                            -- an attacking monster is blocking his cell. So no steps set.
                                            ( foundCoordinatesList, playField )

                        Obstacle _ ->
                            ( foundCoordinatesList, playField )
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
            goRightAroundAndFindLowestSteps nextDirection 0 maxSteps nextCoordinate updatedPlayField updatedFoundCoordinatesList monsterDict

    else
        let
            nextCoordinate =
                getNextCoordinateForDirection currentDirection currentCoordinate
        in
        goRightAroundAndFindLowestSteps currentDirection (doneSteps + 1) maxSteps nextCoordinate updatedPlayField updatedFoundCoordinatesList monsterDict


findLowestSteps : Coordinate -> List Coordinate -> Dict String Cell -> ( List Coordinate, Dict String Cell )
findLowestSteps currentCoordinate foundCoordinatesList playField =
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
                    setStepsForCoordinateInFieldUnsafe currentCoordinate (steps + 1) playField
            in
            ( updatedList, newPlayField )


findLowestStepsAroundCoordinate : Coordinate -> Dict String Cell -> Maybe Int
findLowestStepsAroundCoordinate coordinate playField =
    let
        maybeLowestStepsWithCoordinate =
            findLowestStepsWithCoordinateAroundCoordinateFunction coordinate Nothing findLowestStepsAroundCoordinateStartDirection Nothing playField
    in
    case maybeLowestStepsWithCoordinate of
        Nothing ->
            Nothing

        Just ( lowestSteps, _ ) ->
            Just lowestSteps


makeLowestStepWithCoordinateListFromAroundCoordinate : Coordinate -> Dict String Cell -> List CoordinateWithStep
makeLowestStepWithCoordinateListFromAroundCoordinate coordinate field =
    -- TODO we can make a list with directions, take the first, remove it from list and put in back again., and make that the one to check first.
    -- this can change the way monsters move.
    let
        startDirection =
            Up

        coordinateWithStepList =
            setCoordinatesWithStepInListAroundCoordinate coordinate startDirection startDirection True field []
    in
    List.sortBy .steps coordinateWithStepList


setCoordinatesWithStepInListAroundCoordinate : Coordinate -> Direction -> Direction -> Bool -> Dict String Cell -> List CoordinateWithStep -> List CoordinateWithStep
setCoordinatesWithStepInListAroundCoordinate coordinate currentDirection endDirection isStart field coordinateWithStepList =
    if currentDirection == endDirection && not isStart then
        coordinateWithStepList

    else
        let
            coordinateToCheck =
                getNextCoordinateForDirection currentDirection coordinate

            checkedMaybeSteps =
                getMaybeStepsForCoordinateInField coordinateToCheck field

            updatedCoordinatesWithStepList =
                case checkedMaybeSteps of
                    Nothing ->
                        coordinateWithStepList

                    Just foundSteps ->
                        CoordinateWithStep coordinateToCheck foundSteps :: coordinateWithStepList

            nextDirection =
                findNextDirectionForGoingAroundUnSafe currentDirection
        in
        setCoordinatesWithStepInListAroundCoordinate coordinate nextDirection endDirection False field updatedCoordinatesWithStepList


tryFindCoordinateWithStepsAroundCoordinate : Coordinate -> Int -> Dict String Cell -> Direction -> Direction -> Bool -> ( Bool, Coordinate )
tryFindCoordinateWithStepsAroundCoordinate coordinate steps field currentDirection endDirection isStart =
    if currentDirection == endDirection && not isStart then
        ( False, emptyCoordinate )

    else
        let
            coordinateToCheck =
                getNextCoordinateForDirection currentDirection coordinate
        in
        let
            checkedMaybeSteps =
                getMaybeStepsForCoordinateInField coordinateToCheck field
        in
        case checkedMaybeSteps of
            Nothing ->
                let
                    nextDirection =
                        findNextDirectionForGoingAroundUnSafe currentDirection
                in
                tryFindCoordinateWithStepsAroundCoordinate coordinate steps field nextDirection endDirection False

            Just foundSteps ->
                if foundSteps == steps then
                    ( True, coordinateToCheck )

                else
                    let
                        nextDirection =
                            findNextDirectionForGoingAroundUnSafe currentDirection
                    in
                    tryFindCoordinateWithStepsAroundCoordinate coordinate steps field nextDirection endDirection False


findLowestStepsWithCoordinateAroundCoordinateFunction : Coordinate -> Maybe Coordinate -> Direction -> Maybe ( Int, Coordinate ) -> Dict String Cell -> Maybe ( Int, Coordinate )
findLowestStepsWithCoordinateAroundCoordinateFunction coordinate maybeWrongCoordinate currentDirection maybeStepsWithCoordinate playField =
    let
        coordinateToCheck =
            getNextCoordinateForDirection currentDirection coordinate
    in
    if maybeCoordinateIsSameAsCoordinate coordinateToCheck maybeWrongCoordinate then
        if currentDirection == findLowestStepsAroundCoordinateEndDirection then
            maybeStepsWithCoordinate

        else
            let
                nextDirection =
                    findNextDirectionForGoingAroundUnSafe currentDirection
            in
            findLowestStepsWithCoordinateAroundCoordinateFunction coordinate maybeWrongCoordinate nextDirection maybeStepsWithCoordinate playField

    else
        let
            checkedMaybeSteps =
                getMaybeStepsForCoordinateInField coordinateToCheck playField

            updatedMaybeStepsWithCoordinate =
                case checkedMaybeSteps of
                    Nothing ->
                        maybeStepsWithCoordinate

                    Just newSteps ->
                        case maybeStepsWithCoordinate of
                            Nothing ->
                                Just ( newSteps, coordinateToCheck )

                            Just ( oldSteps, _ ) ->
                                if oldSteps > newSteps then
                                    Just ( newSteps, coordinateToCheck )

                                else
                                    maybeStepsWithCoordinate
        in
        if currentDirection == findLowestStepsAroundCoordinateEndDirection then
            updatedMaybeStepsWithCoordinate

        else
            let
                nextDirection =
                    findNextDirectionForGoingAroundUnSafe currentDirection
            in
            findLowestStepsWithCoordinateAroundCoordinateFunction coordinate maybeWrongCoordinate nextDirection updatedMaybeStepsWithCoordinate playField


maybeCoordinateIsSameAsCoordinate : Coordinate -> Maybe Coordinate -> Bool
maybeCoordinateIsSameAsCoordinate coordinate maybeCoordinate =
    case maybeCoordinate of
        Nothing ->
            False

        Just otherCoordinate ->
            isSameCoordinate coordinate otherCoordinate



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


setStepsAroundHero : Coordinate -> Dict String Cell -> Dict String MonsterModel -> ( Dict String Cell, List Coordinate )
setStepsAroundHero heroSpot field monsterDict =
    let
        straightSteps =
            1

        diagonalSteps =
            2

        leftOverCoordinatesStartList =
            []

        ( isUpOke, upDoneField ) =
            trySetStepsForCoordinateInFieldIfEmptyOrMovingMonster straightSteps (getNextCoordinateForDirection Up heroSpot) field monsterDict

        ( isRightOke, rightDoneField ) =
            trySetStepsForCoordinateInFieldIfEmptyOrMovingMonster straightSteps (getNextCoordinateForDirection Right heroSpot) upDoneField monsterDict

        ( isDownOke, downDoneField ) =
            trySetStepsForCoordinateInFieldIfEmptyOrMovingMonster straightSteps (getNextCoordinateForDirection Down heroSpot) rightDoneField monsterDict

        ( isLeftOke, leftDoneField ) =
            trySetStepsForCoordinateInFieldIfEmptyOrMovingMonster straightSteps (getNextCoordinateForDirection Left heroSpot) downDoneField monsterDict

        ( upRightDonePlayField, upRightDoneLeftOverCoordinates ) =
            let
                nextCoordinate =
                    getNextCoordinateForDirection UpRight heroSpot
            in
            if isUpOke || isRightOke then
                ( setStepsForCoordinateInFieldIfEmptyOrMovingMonster diagonalSteps nextCoordinate leftDoneField monsterDict, leftOverCoordinatesStartList )

            else
                ( leftDoneField, nextCoordinate :: leftOverCoordinatesStartList )

        ( downRightDonePlayField, downRightDoneLeftOverCoordinates ) =
            let
                nextCoordinate =
                    getNextCoordinateForDirection DownRight heroSpot
            in
            if isDownOke || isRightOke then
                ( setStepsForCoordinateInFieldIfEmptyOrMovingMonster diagonalSteps nextCoordinate upRightDonePlayField monsterDict, upRightDoneLeftOverCoordinates )

            else
                ( upRightDonePlayField, nextCoordinate :: upRightDoneLeftOverCoordinates )

        ( downLeftDonePlayField, downLeftDoneLeftOverCoordinate ) =
            let
                nextCoordinate =
                    getNextCoordinateForDirection DownLeft heroSpot
            in
            if isDownOke || isLeftOke then
                ( setStepsForCoordinateInFieldIfEmptyOrMovingMonster diagonalSteps nextCoordinate downRightDonePlayField monsterDict, downRightDoneLeftOverCoordinates )

            else
                ( downRightDonePlayField, nextCoordinate :: downRightDoneLeftOverCoordinates )
    in
    let
        nextCoordinate =
            getNextCoordinateForDirection UpLeft heroSpot
    in
    if isUpOke || isLeftOke then
        ( setStepsForCoordinateInFieldIfEmptyOrMovingMonster diagonalSteps nextCoordinate downLeftDonePlayField monsterDict, downLeftDoneLeftOverCoordinate )

    else
        ( downLeftDonePlayField, nextCoordinate :: downLeftDoneLeftOverCoordinate )


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
