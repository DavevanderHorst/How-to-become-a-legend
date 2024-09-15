module Functions.Monsters.Base exposing (..)

import Constants.Times exposing (monsterAnimationDuration)
import Dict exposing (Dict)
import Functions.Animations.Base exposing (tryMakeMonsterAnimationAndAdjustModel)
import Functions.Animations.Move exposing (tryMakeMonsterMoveAnimationForOtherSmallestCoordinate)
import Functions.Base exposing (addToTheBackOfTheList)
import Functions.Coordinate exposing (areCoordinatesNextToEachOther, isSameCoordinate)
import Functions.Monsters.MonsterDict exposing (addMonsterToMonsterDictUnsafe)
import Functions.PlayField.Get exposing (tryGetCellFromPlayFieldByCoordinate)
import Functions.PlayField.Insert exposing (insertMonsterInToMonsterDict, updateMonsterInPlayFieldUnsafe)
import Functions.PlayField.Set exposing (removeMonsterFromPlayFieldUnsafe)
import Messages exposing (Msg(..))
import Models.Cell exposing (Cell, Coordinate)
import Models.Level exposing (Level)
import Models.MainModel exposing (Error, MainModel)
import Models.Monster exposing (MonsterModel)
import Process
import Task
import Types exposing (Action(..))


putMonsterInList : String -> MonsterModel -> List MonsterModel -> List MonsterModel
putMonsterInList _ monster monsterList =
    monster :: monsterList


handleMonstersTurn : MainModel -> ( MainModel, Cmd Msg )
handleMonstersTurn model =
    -- we make a list of our monster, and that we use to make animations for them 1 by 1
    -- we empty our monster dict, and we put them back after we made the animation,
    let
        oldLevel =
            model.level

        monstersList =
            Dict.foldl putMonsterInList [] oldLevel.monsterModels

        levelWithEmptyMonsterDict =
            { oldLevel | monsterModels = Dict.empty }
    in
    handleMonsterTurn monstersList { model | level = levelWithEmptyMonsterDict }


handleMonsterTurn : List MonsterModel -> MainModel -> ( MainModel, Cmd Msg )
handleMonsterTurn monstersToDoList model =
    -- every monster does do something, move or attack, what he does says his action
    -- we doing monsters one by one, animation after animation
    let
        oldLevel =
            model.level

        oldPlayField =
            oldLevel.playField

        firstMaybeMonster =
            List.head monstersToDoList

        tailOfMonsterList =
            List.drop 1 monstersToDoList
    in
    case firstMaybeMonster of
        Nothing ->
            ( { model | error = Just { method = "handleMonstersTurn", error = "No more monsters in our list." } }, Cmd.none )

        Just firstMonster ->
            let
                makeAnimationsResult =
                    tryMakeMonsterAnimationAndAdjustModel firstMonster oldPlayField.field oldLevel.heroModel.coordinate
            in
            case makeAnimationsResult of
                -- we make from our monster dict a list
                -- then we take the first monster
                -- we make an
                Err err ->
                    ( { model | error = Just err }, Cmd.none )

                Ok ( animations, newMonsterModel ) ->
                    if isCoordinateInMonsterList newMonsterModel.coordinate tailOfMonsterList then
                        -- there is still a monster on this spot, so that monster has to move first.
                        -- we add this monster to the back of the list
                        let
                            changedMonsterList =
                                addToTheBackOfTheList firstMonster tailOfMonsterList
                        in
                        ( model, Task.perform (\_ -> DoNextMonster changedMonsterList) (Task.succeed True) )

                    else if coordinateContainsAMonster newMonsterModel.coordinate model.level.playField.field && newMonsterModel.action == Moving then
                        let
                            otherCoordinateAnimationResult =
                                tryMakeMonsterMoveAnimationForOtherSmallestCoordinate firstMonster newMonsterModel.coordinate model.level.playField.field
                        in
                        case otherCoordinateAnimationResult of
                            -- we make from our monster dict a list
                            -- then we take the first monster
                            -- we make an
                            Err err ->
                                ( { model | error = Just err }, Cmd.none )

                            Ok ( animation, newMonsterModelTwo ) ->
                                let
                                    fieldWithRemovedMonster =
                                        removeMonsterFromPlayFieldUnsafe firstMonster oldPlayField.field

                                    updatedPlayField =
                                        { oldPlayField | field = fieldWithRemovedMonster }

                                    updatedMonsters =
                                        insertMonsterInToMonsterDict newMonsterModelTwo oldLevel.monsterModels

                                    updatedLevel =
                                        { oldLevel | animations = [ animation ], playField = updatedPlayField, monsterModels = updatedMonsters }

                                    nextCommand =
                                        Process.sleep (toFloat <| monsterAnimationDuration)
                                            |> Task.perform (always (MonsterAnimationIsDone newMonsterModelTwo tailOfMonsterList))
                                in
                                ( { model | level = updatedLevel }, nextCommand )

                    else
                        let
                            fieldWithRemovedMonster =
                                removeMonsterFromPlayFieldUnsafe firstMonster oldPlayField.field

                            updatedPlayField =
                                { oldPlayField | field = fieldWithRemovedMonster }

                            updatedMonsters =
                                insertMonsterInToMonsterDict newMonsterModel oldLevel.monsterModels

                            updatedLevel =
                                { oldLevel | animations = animations, playField = updatedPlayField, monsterModels = updatedMonsters }

                            nextCommand =
                                Process.sleep (toFloat <| monsterAnimationDuration)
                                    |> Task.perform (always (MonsterAnimationIsDone newMonsterModel tailOfMonsterList))
                        in
                        ( { model | level = updatedLevel }, nextCommand )


coordinateContainsAMonster : Coordinate -> Dict String Cell -> Bool
coordinateContainsAMonster coordinate field =
    let
        getCellResult =
            tryGetCellFromPlayFieldByCoordinate coordinate field
    in
    case getCellResult of
        Err _ ->
            False

        Ok cell ->
            case cell.content of
                Types.Empty ->
                    False

                Types.Hero ->
                    False

                Types.Monster _ _ ->
                    True

                Types.Obstacle _ ->
                    False


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


setMonsterActions : Level -> Level
setMonsterActions level =
    let
        oldPlayField =
            level.playField

        ( updatedMonsters, updatedField ) =
            checkActionTodoForMonsters level.heroModel.coordinate level.monsterModels oldPlayField.field

        newPlayField =
            { oldPlayField | field = updatedField }
    in
    { level | monsterModels = updatedMonsters, playField = newPlayField }


checkActionTodoForMonsters : Coordinate -> Dict String MonsterModel -> Dict String Cell -> ( Dict String MonsterModel, Dict String Cell )
checkActionTodoForMonsters heroCoordinate monsterDict playField =
    Dict.foldl (setMonsterAction heroCoordinate) ( Dict.empty, playField ) monsterDict


setMonsterAction : Coordinate -> String -> MonsterModel -> ( Dict String MonsterModel, Dict String Cell ) -> ( Dict String MonsterModel, Dict String Cell )
setMonsterAction heroCoordinate _ monster ( monsterDict, playField ) =
    let
        updatedMonster =
            if areCoordinatesNextToEachOther monster.coordinate heroCoordinate then
                { monster | action = Attacking }

            else
                { monster | action = Moving }

        updatedMonsterDict =
            addMonsterToMonsterDictUnsafe updatedMonster monsterDict

        updatedPlayField =
            if monster.action == updatedMonster.action then
                playField

            else
                updateMonsterInPlayFieldUnsafe updatedMonster playField
    in
    ( updatedMonsterDict, updatedPlayField )
