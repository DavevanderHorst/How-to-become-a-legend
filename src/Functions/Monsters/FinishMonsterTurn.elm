module Functions.Monsters.FinishMonsterTurn exposing (..)

import Constants.Times exposing (monsterAnimationDuration)
import Functions.PlayField.Insert exposing (insertMonsterInToMonsterDict)
import Functions.PlayField.Set exposing (removeMonsterFromPlayFieldUnsafe)
import Messages exposing (Msg(..))
import Models.MainModel exposing (MainModel)
import Models.Monster exposing (MonsterModel)
import Process
import Svg exposing (Svg)
import Task


finishMonsterTurn : MonsterModel -> Maybe MonsterModel -> List (Svg Msg) -> MainModel -> List MonsterModel -> ( MainModel, Cmd Msg )
finishMonsterTurn oldMonster maybeUpdatedMonster animations model restOfMonsters =
    let
        updatedMonster =
            case maybeUpdatedMonster of
                Nothing ->
                    oldMonster

                Just monster ->
                    monster

        fieldWithRemovedMonster =
            removeMonsterFromPlayFieldUnsafe oldMonster model.level.playField.field

        oldLevel =
            model.level

        oldPlayField =
            oldLevel.playField

        updatedPlayField =
            { oldPlayField | field = fieldWithRemovedMonster }

        updatedMonsters =
            insertMonsterInToMonsterDict updatedMonster oldLevel.monsterModels

        updatedLevel =
            { oldLevel | animations = animations, playField = updatedPlayField, monsterModels = updatedMonsters }

        nextCommand =
            Process.sleep (toFloat <| monsterAnimationDuration)
                |> Task.perform (always (MonsterAnimationIsDone updatedMonster restOfMonsters))
    in
    ( { model | level = updatedLevel }, nextCommand )
