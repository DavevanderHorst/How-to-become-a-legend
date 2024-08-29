module Functions.Monsters.Base exposing (handleMonstersTurn)

import Constants.Times exposing (monsterAnimationDuration)
import Functions.Animations.Move exposing (tryMakeMonsterAnimationsAndAdjustModels)
import Functions.PlayField.Set exposing (removeMonstersFromPlayFieldUnSafe)
import Messages exposing (Msg(..))
import Models.Level exposing (Level)
import Models.MainModel exposing (Error, MainModel)
import Process
import Task


handleMonstersTurn : MainModel -> ( MainModel, Cmd Msg )
handleMonstersTurn model =
    -- every monster does do something, move or attack, what he does says his action
    -- move or attack
    -- so all get an animation, which means we have to remove them temporarily
    let
        oldLevel =
            model.level

        oldPlayField =
            oldLevel.playField

        makeAnimationsResult =
            tryMakeMonsterAnimationsAndAdjustModels oldPlayField.field oldLevel.monsterModels
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
