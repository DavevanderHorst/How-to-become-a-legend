module Functions.Monsters.Base exposing (handleMonstersTurn)

import Constants.Times exposing (monsterAnimationDuration)
import Dict exposing (Dict)
import Functions.Animations.Move exposing (tryMakeMonsterAnimationsAndAdjustModels)
import Functions.Coordinate exposing (areCoordinatesNextToEachOther)
import Functions.PlayField.Set exposing (removeMonstersFromPlayFieldUnSafe)
import Messages exposing (Msg(..))
import Models.Cell exposing (Coordinate)
import Models.Level exposing (Level)
import Models.MainModel exposing (Error, MainModel)
import Models.Monster exposing (MonsterModel)
import Process
import Task
import Types exposing (Action(..))


handleMonstersTurn : MainModel -> ( MainModel, Cmd Msg )
handleMonstersTurn model =
    -- every monster does do something, move or attack, what he does says his action
    -- we check if a monster is next to a hero, and set them in action/attack modes
    -- move or attack
    -- so all get an animation, which means we have the images temporarily
    let
        oldLevel =
            model.level

        oldMonsters =
            oldLevel.monsterModels

        oldPlayField =
            oldLevel.playField

        updatedMonsters =
            checkActionTodoForMonsters model.level.heroModel.coordinate oldMonsters

        makeAnimationsResult =
            tryMakeMonsterAnimationsAndAdjustModels oldPlayField.field updatedMonsters
    in
    case makeAnimationsResult of
        -- animations are made correctly, so we can remove them from play field.
        Ok ( animations, newMonsterModels ) ->
            let
                fieldWithoutMonsters =
                    removeMonstersFromPlayFieldUnSafe oldLevel.monsterModels oldLevel.playField.field

                playFieldWithoutMonsters =
                    { oldPlayField | field = fieldWithoutMonsters }

                updatedLevel =
                    { oldLevel | currentAnimations = animations, playField = playFieldWithoutMonsters, monsterModels = newMonsterModels }

                nextCommand =
                    Process.sleep (toFloat <| monsterAnimationDuration) |> Task.perform (always MonsterAnimationsAreDone)
            in
            ( { model | level = updatedLevel }, nextCommand )

        Err err ->
            ( { model | error = Just err }, Cmd.none )


checkActionTodoForMonsters : Coordinate -> Dict String MonsterModel -> Dict String MonsterModel
checkActionTodoForMonsters heroCoordinate monsterDict =
    Dict.map (setMonsterAction heroCoordinate) monsterDict


setMonsterAction : Coordinate -> String -> MonsterModel -> MonsterModel
setMonsterAction heroCoordinate _ monster =
    if areCoordinatesNextToEachOther monster.coordinate heroCoordinate then
        { monster | action = Attacking }

    else
        monster
