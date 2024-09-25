module Functions.Monsters.FinishMonsterTurn exposing (..)

import Constants.Times exposing (monsterAnimationDuration)
import Functions.Monsters.MonsterDict exposing (removeMonsterFromDictByCoordinateUnsafe)
import Functions.PlayField.Insert exposing (insertMonsterInToMonsterDict)
import Functions.PlayField.Set exposing (removeMonsterFromFieldUnsafe)
import Messages exposing (Msg(..))
import Models.MainModel exposing (MainModel)
import Models.Monster exposing (MonsterModel)
import Process
import Svg exposing (Svg)
import Task


finishMonsterTurn : MonsterModel -> Maybe MonsterModel -> List (Svg Msg) -> MainModel -> List MonsterModel -> ( MainModel, Cmd Msg )
finishMonsterTurn oldMonster maybeUpdatedMonster animations model restOfMonsters =
    let
        fieldWithRemovedMonster =
            removeMonsterFromFieldUnsafe oldMonster model.level.playField.field

        oldLevel =
            model.level

        oldPlayField =
            oldLevel.playField

        updatedPlayField =
            { oldPlayField | field = fieldWithRemovedMonster }

        ( newMonsters, newMonster ) =
            case maybeUpdatedMonster of
                Nothing ->
                    ( oldLevel.monsterDict, oldMonster )

                Just updatedMonster ->
                    let
                        monsterDictWithoutOldMonster =
                            removeMonsterFromDictByCoordinateUnsafe oldMonster.coordinate oldLevel.monsterDict
                    in
                    ( insertMonsterInToMonsterDict updatedMonster monsterDictWithoutOldMonster, updatedMonster )

        updatedLevel =
            { oldLevel | animations = animations, playField = updatedPlayField, monsterDict = newMonsters }

        nextCommand =
            Process.sleep (toFloat <| monsterAnimationDuration)
                |> Task.perform (always (MonsterAnimationIsDone newMonster restOfMonsters))
    in
    ( { model | level = updatedLevel }, nextCommand )
