module Functions.Monsters.Moving exposing (handleMonsterMovingTurn)

import Functions.Animations.Idle exposing (makeMonsterIdleAnimationUnsafe)
import Functions.Animations.Move exposing (makeMonsterMoveAnimationUnsafe)
import Functions.Base exposing (addToTheBackOfTheList, tryTakeSecondFromList)
import Functions.Coordinate exposing (isSameCoordinate)
import Functions.Monsters.FinishMonsterTurn exposing (finishMonsterTurn)
import Functions.PathFinding exposing (makeLowestStepWithCoordinateListFromAroundCoordinate)
import Functions.PlayField.Get exposing (tryGetCellFromFieldByCoordinate)
import Messages exposing (Msg(..))
import Models.Cell exposing (Cell, Coordinate)
import Models.MainModel exposing (MainModel)
import Models.Monster exposing (MonsterModel)
import Task
import Types exposing (CellContent(..))


handleMonsterMovingTurn : MonsterModel -> Cell -> MainModel -> List MonsterModel -> ( MainModel, Cmd Msg )
handleMonsterMovingTurn monster monsterCell model restOfMonsters =
    -- monster and monster cell are checked, we check everything else here
    case monsterCell.stepsToHero of
        Nothing ->
            -- cell has no steps, so monster wont move! TODO
            let
                updatedError =
                    { method = "handleMonsterMovingTurn"
                    , error = "TODO no steps, cant move animation"
                    }
            in
            ( { model | error = Just updatedError }, Cmd.none )

        Just steps ->
            let
                field =
                    model.level.playField.field

                lowestStepsList =
                    makeLowestStepWithCoordinateListFromAroundCoordinate monster.coordinate field
            in
            case List.head lowestStepsList of
                Nothing ->
                    let
                        updatedError =
                            { method = "handleMonsterMovingTurn"
                            , error = "Our monster cell has steps, but we didnt find another cell around with steps."
                            }
                    in
                    ( { model | error = Just updatedError }, Cmd.none )

                Just lowestCoordinateWithStep ->
                    if lowestCoordinateWithStep.steps + 1 /= steps then
                        let
                            updatedError =
                                { method = "handleMonsterMovingTurn"
                                , error = "Found wrong steps, should be 1 lower."
                                }
                        in
                        ( { model | error = Just updatedError }, Cmd.none )

                    else if isCoordinateInMonsterList lowestCoordinateWithStep.coordinate restOfMonsters then
                        -- there is still a monster on this spot, so that monster has to move first.
                        -- we add this monster to the back of the list
                        let
                            changedMonsterList =
                                addToTheBackOfTheList monster restOfMonsters
                        in
                        ( model, Task.perform (\_ -> DoNextMonster changedMonsterList) (Task.succeed True) )

                    else
                        let
                            foundCellResult =
                                tryGetCellFromFieldByCoordinate lowestCoordinateWithStep.coordinate field
                        in
                        case foundCellResult of
                            Err foundCellError ->
                                let
                                    updatedError =
                                        { method = "handleMonsterMovingTurn - " ++ foundCellError.method
                                        , error = foundCellError.error
                                        }
                                in
                                ( { model | error = Just updatedError }, Cmd.none )

                            Ok foundCell ->
                                -- we need to check if this cell is empty
                                case foundCell.content of
                                    Empty ->
                                        -- everything is oke!
                                        finishMonsterMovingTurn monster monsterCell foundCell model restOfMonsters

                                    Hero ->
                                        -- not possible, else something is wrong with pathfinding.
                                        let
                                            updatedError =
                                                { method = "handleMonsterMovingTurn"
                                                , error = "The hero is in the cell where monster is moving towards."
                                                }
                                        in
                                        ( { model | error = Just updatedError }, Cmd.none )

                                    Monster ->
                                        -- we found a monster on the spot our monster wants to move towards
                                        -- that means that another monster already moved here this round.
                                        -- so we go for the second best spot if its the same steps.
                                        -- of course we have to check this coordinate again.
                                        let
                                            maybeSecondBestCoordinate =
                                                tryTakeSecondFromList lowestStepsList
                                        in
                                        case maybeSecondBestCoordinate of
                                            Nothing ->
                                                finishMonsterIdleTurn monster monsterCell model restOfMonsters

                                            Just secondBestCoordinate ->
                                                if secondBestCoordinate.steps + 1 /= steps then
                                                    finishMonsterIdleTurn monster monsterCell model restOfMonsters

                                                else if isCoordinateInMonsterList secondBestCoordinate.coordinate restOfMonsters then
                                                    -- there is still a monster on this spot, so that monster has to move first.
                                                    -- we add this monster to the back of the list
                                                    finishChangedRestOfMonstersList monster model restOfMonsters

                                                else
                                                    -- now we have a coordinate. We need to check this one as well
                                                    let
                                                        getSecondBestCellResult =
                                                            tryGetCellFromFieldByCoordinate secondBestCoordinate.coordinate field
                                                    in
                                                    case getSecondBestCellResult of
                                                        Err error ->
                                                            let
                                                                updatedError =
                                                                    { method = "handleMonsterMovingTurn - " ++ error.method
                                                                    , error = error.error
                                                                    }
                                                            in
                                                            ( { model | error = Just updatedError }, Cmd.none )

                                                        Ok secondBestCell ->
                                                            if secondBestCell.content == Empty then
                                                                finishMonsterMovingTurn monster monsterCell secondBestCell model restOfMonsters

                                                            else
                                                                finishMonsterIdleTurn monster monsterCell model restOfMonsters

                                    --finishMonsterMovingTurn monster monsterCell foundCell model restOfMonsters
                                    Obstacle _ ->
                                        -- not possible, else something is wrong with pathfinding.
                                        let
                                            updatedError =
                                                { method = "handleMonsterMovingTurn"
                                                , error = "There is an obstacle in the cell where monster is moving towards."
                                                }
                                        in
                                        ( { model | error = Just updatedError }, Cmd.none )


finishMonsterMovingTurn : MonsterModel -> Cell -> Cell -> MainModel -> List MonsterModel -> ( MainModel, Cmd Msg )
finishMonsterMovingTurn monster monsterCell nextMonsterCell model restOfMonsters =
    let
        animation =
            makeMonsterMoveAnimationUnsafe monster.specie monsterCell nextMonsterCell

        updatedMonster =
            { monster | coordinate = nextMonsterCell.coordinate }
    in
    finishMonsterTurn monster (Just updatedMonster) [ animation ] model restOfMonsters


finishMonsterIdleTurn : MonsterModel -> Cell -> MainModel -> List MonsterModel -> ( MainModel, Cmd Msg )
finishMonsterIdleTurn monster monsterCell model restOfMonsters =
    let
        animation =
            makeMonsterIdleAnimationUnsafe monster.specie monsterCell
    in
    finishMonsterTurn monster Nothing [ animation ] model restOfMonsters


finishChangedRestOfMonstersList : MonsterModel -> MainModel -> List MonsterModel -> ( MainModel, Cmd Msg )
finishChangedRestOfMonstersList monster model restOfMonsters =
    let
        changedMonsterList =
            addToTheBackOfTheList monster restOfMonsters
    in
    ( model, Task.perform (\_ -> DoNextMonster changedMonsterList) (Task.succeed True) )


isCoordinateInMonsterList : Coordinate -> List MonsterModel -> Bool
isCoordinateInMonsterList coordinate monsters =
    let
        filteredList =
            List.filter (monsterHasSameCoordinate coordinate) monsters
    in
    if List.length filteredList > 0 then
        True

    else
        False


monsterHasSameCoordinate : Coordinate -> MonsterModel -> Bool
monsterHasSameCoordinate coordinate monster =
    isSameCoordinate coordinate monster.coordinate
