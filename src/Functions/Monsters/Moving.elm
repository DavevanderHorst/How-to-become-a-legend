module Functions.Monsters.Moving exposing (handleMonsterMovingTurn)

import Constants.Times exposing (monsterAnimationDuration)
import Functions.Animations.Move exposing (makeMonsterMoveAnimationUnsafe)
import Functions.Base exposing (addToTheBackOfTheList)
import Functions.Coordinate exposing (isSameCoordinate)
import Functions.PathFinding exposing (tryFindCoordinateWithOneLowerStepAroundCoordinate)
import Functions.PlayField.Get exposing (tryGetCellFromFieldByCoordinate)
import Functions.PlayField.Insert exposing (insertMonsterInToMonsterDict)
import Functions.PlayField.Set exposing (removeMonsterFromPlayFieldUnsafe)
import Messages exposing (Msg(..))
import Models.Cell exposing (Coordinate)
import Models.MainModel exposing (MainModel)
import Models.Monster exposing (MonsterModel)
import Process
import Task
import Types exposing (CellContent(..))


handleMonsterMovingTurn : MonsterModel -> MainModel -> List MonsterModel -> ( MainModel, Cmd Msg )
handleMonsterMovingTurn monster model restOfMonsters =
    -- we check everything here
    -- cell monster moves towards, cell hes in
    let
        field =
            model.level.playField.field

        monsterCellResult =
            tryGetCellFromFieldByCoordinate monster.coordinate field
    in
    case monsterCellResult of
        Err error ->
            let
                updatedError =
                    { method = "handleMonsterMovingTurn - " ++ error.method
                    , error = error.error
                    }
            in
            ( { model | error = Just updatedError }, Cmd.none )

        Ok monsterCell ->
            let
                ( monsterIsInCell, error ) =
                    checkIfMonsterIsInCell monster monsterCell.content
            in
            if not monsterIsInCell then
                let
                    updatedError =
                        { method = "handleMonsterMovingTurn"
                        , error = error
                        }
                in
                ( { model | error = Just updatedError }, Cmd.none )

            else
                -- monster is oke, we check if his cell has any steps.
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
                            ( didWeFindCoordinate, foundCoordinate ) =
                                tryFindCoordinateWithOneLowerStepAroundCoordinate monster.coordinate steps field
                        in
                        if didWeFindCoordinate then
                            if isCoordinateInMonsterList foundCoordinate restOfMonsters then
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
                                        tryGetCellFromFieldByCoordinate foundCoordinate field
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
                                        let
                                            animation =
                                                makeMonsterMoveAnimationUnsafe monster.specie monsterCell foundCell

                                            fieldWithRemovedMonster =
                                                removeMonsterFromPlayFieldUnsafe monster field

                                            updatedMonster =
                                                { monster | coordinate = foundCoordinate }

                                            oldLevel =
                                                model.level

                                            oldPlayField =
                                                oldLevel.playField

                                            updatedPlayField =
                                                { oldPlayField | field = fieldWithRemovedMonster }

                                            updatedMonsters =
                                                insertMonsterInToMonsterDict updatedMonster oldLevel.monsterModels

                                            updatedLevel =
                                                { oldLevel | animations = [ animation ], playField = updatedPlayField, monsterModels = updatedMonsters }

                                            nextCommand =
                                                Process.sleep (toFloat <| monsterAnimationDuration)
                                                    |> Task.perform (always (MonsterAnimationIsDone updatedMonster restOfMonsters))
                                        in
                                        ( { model | level = updatedLevel }, nextCommand )

                        else
                            let
                                updatedError =
                                    { method = "handleMonsterMovingTurn"
                                    , error = "found steps in coordinate, but did not find one lower step around it."
                                    }
                            in
                            ( { model | error = Just updatedError }, Cmd.none )


checkIfMonsterIsInCell : MonsterModel -> CellContent -> ( Bool, String )
checkIfMonsterIsInCell monster content =
    case content of
        Empty ->
            ( False, "Cell content is empty" )

        Hero ->
            ( False, "Cell content contains a hero" )

        Monster specie action ->
            if monster.specie /= specie then
                ( False, "Monster in cell is different specie" )

            else if monster.action /= action then
                ( False, "Monster in cell has different action" )

            else
                ( True, "" )

        Obstacle _ ->
            ( False, "Cell content contains an obstacle" )


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
