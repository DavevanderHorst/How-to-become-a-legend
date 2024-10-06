module Functions.Monsters.Base exposing (..)

import Dict exposing (Dict)
import Functions.Coordinate exposing (areCoordinatesNextToEachOther)
import Functions.Monsters.Attacking exposing (handleMonsterAttackingTurn)
import Functions.Monsters.MonsterDict exposing (addMonsterToMonsterDictUnsafe)
import Functions.Monsters.Moving exposing (handleMonsterMovingTurn)
import Functions.PlayField.Get exposing (tryGetCellFromFieldByCoordinate)
import Functions.PlayField.Insert exposing (updateMonsterInPlayFieldUnsafe)
import Functions.ToString exposing (cellContentToString, coordinateToString)
import Messages exposing (Msg(..))
import Models.Cell exposing (Cell, Coordinate)
import Models.Level exposing (Level)
import Models.MainModel exposing (Error, MainModel)
import Models.Monster exposing (MonsterModel)
import Types exposing (Action(..), CellContent(..))


putMonsterInList : String -> MonsterModel -> List MonsterModel -> List MonsterModel
putMonsterInList _ monster monsterList =
    monster :: monsterList


handleMonstersTurn : MainModel -> ( MainModel, Cmd Msg )
handleMonstersTurn model =
    -- we make a list of our monster, and that we use to make animations for them 1 by 1
    -- we empty our monster dict, and we put them back after we made the animation,
    let
        monstersList =
            Dict.foldl putMonsterInList [] model.level.monsterDict
    in
    handleMonsterTurn monstersList model


handleMonsterTurn : List MonsterModel -> MainModel -> ( MainModel, Cmd Msg )
handleMonsterTurn monstersToDoList model =
    -- every monster does do something, move or attack, what he does says his action
    -- we doing monsters one by one, animation after animation
    -- we take the first monster, find his spot to move, and check in the list if there is still a monster there that has to move.
    let
        firstMaybeMonster =
            List.head monstersToDoList

        tailOfMonsterList =
            List.drop 1 monstersToDoList
    in
    case firstMaybeMonster of
        Nothing ->
            ( { model
                | error =
                    Just
                        { method = "handleMonstersTurn"
                        , error = "No more monsters in our list."
                        }
              }
            , Cmd.none
            )

        Just firstMonster ->
            -- now we check monster and cell if everything is oke
            let
                field =
                    model.level.playField.field

                monsterCellResult =
                    tryGetCellFromFieldByCoordinate firstMonster.coordinate field
            in
            case monsterCellResult of
                Err error ->
                    let
                        updatedError =
                            { method = "handleMonsterTurn - " ++ error.method
                            , error = error.error
                            }
                    in
                    ( { model | error = Just updatedError }, Cmd.none )

                Ok monsterCell ->
                    if monsterCell.content /= Monster then
                        let
                            updatedError =
                                { method = "handleMonsterTurn"
                                , error =
                                    "There is no monster in given coordinate : "
                                        ++ coordinateToString firstMonster.coordinate
                                        ++ ". Content = "
                                        ++ cellContentToString monsterCell.content
                                }
                        in
                        ( { model | error = Just updatedError }, Cmd.none )

                    else
                        case firstMonster.action of
                            Moving ->
                                handleMonsterMovingTurn firstMonster monsterCell model tailOfMonsterList

                            Attacking ->
                                handleMonsterAttackingTurn firstMonster monsterCell model tailOfMonsterList


setMonsterActions : Level -> Level
setMonsterActions level =
    let
        oldPlayField =
            level.playField

        ( updatedMonsters, updatedField ) =
            checkActionTodoForMonsters level.heroModel.coordinate level.monsterDict oldPlayField.field

        newPlayField =
            { oldPlayField | field = updatedField }
    in
    { level | monsterDict = updatedMonsters, playField = newPlayField }


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


doDamageToMonster : MonsterModel -> Int -> MonsterModel
doDamageToMonster monster damage =
    let
        stats =
            monster.stats

        updatedStats =
            { stats | currentHitPoints = stats.currentHitPoints - damage }
    in
    { monster | stats = updatedStats }
