module Functions.PlayField.Set exposing (..)

import Dict exposing (Dict)
import Functions.Monsters.MonsterDict exposing (tryGetMonsterFromMonsterDictByCoordinate)
import Functions.PlayField.Get exposing (tryGetCellFromFieldByCoordinate)
import Functions.PlayField.Helpers exposing (setCellContentToHero, setCellContentToMonster, setContentToEmpty, setSteps, setStepsToNothing, updateGridCellDictByCoordinateUnsafe)
import Functions.ToString exposing (cellContentToString)
import Models.Cell exposing (Cell, Coordinate)
import Models.Level exposing (PlayField)
import Models.MainModel exposing (Error)
import Models.Monster exposing (MonsterModel)
import Types exposing (Action(..), CellContent(..), Specie)


setHeroInPlayUnsafe : Coordinate -> Dict String Cell -> Dict String Cell
setHeroInPlayUnsafe coordinate playField =
    -- unsafe, cell will be set to Hero, even if there is something in that cell
    updateGridCellDictByCoordinateUnsafe coordinate setCellContentToHero playField


trySetMonsterInField : MonsterModel -> Dict String Cell -> Result Error (Dict String Cell)
trySetMonsterInField monster playField =
    let
        monsterCellResult =
            tryGetCellFromFieldByCoordinate monster.coordinate playField
    in
    case monsterCellResult of
        Err err ->
            Err { method = "trySetMonsterInPlayField - " ++ err.method, error = err.error }

        Ok monsterCell ->
            if monsterCell.content == Empty then
                Ok (setMonsterInFieldUnsafe monster playField)

            else
                Err
                    { method = "trySetMonsterInPlayField"
                    , error = "Cell content is not empty for placing monster : " ++ cellContentToString monsterCell.content
                    }


setMonsterInFieldUnsafe : MonsterModel -> Dict String Cell -> Dict String Cell
setMonsterInFieldUnsafe monster field =
    updateGridCellDictByCoordinateUnsafe monster.coordinate setCellContentToMonster field


setStepsForCoordinateInFieldUnsafe : Coordinate -> Int -> Dict String Cell -> Dict String Cell
setStepsForCoordinateInFieldUnsafe coordinate steps field =
    updateGridCellDictByCoordinateUnsafe coordinate (setSteps steps) field


removeHeroFromFieldUnsafe : Coordinate -> Dict String Cell -> Dict String Cell
removeHeroFromFieldUnsafe coordinate playField =
    -- unsafe, cell will be set to empty, even if hero is not there, or if cell does not exist
    updateGridCellDictByCoordinateUnsafe coordinate setContentToEmpty playField


removeMonsterFromFieldUnsafe : MonsterModel -> Dict String Cell -> Dict String Cell
removeMonsterFromFieldUnsafe monster playField =
    updateGridCellDictByCoordinateUnsafe monster.coordinate setContentToEmpty playField


removeStepsFromPlayField : PlayField -> PlayField
removeStepsFromPlayField playField =
    let
        newField =
            Dict.map setStepsToNothing playField.field
    in
    { playField | field = newField }


setStepsForCoordinateInFieldIfEmptyOrMovingMonster : Int -> Coordinate -> Dict String Cell -> Dict String MonsterModel -> Dict String Cell
setStepsForCoordinateInFieldIfEmptyOrMovingMonster steps coordinate field monsterDict =
    let
        ( _, updatedField ) =
            trySetStepsForCoordinateInFieldIfEmptyOrMovingMonster steps coordinate field monsterDict
    in
    updatedField


trySetStepsForCoordinateInFieldIfEmptyOrMovingMonster : Int -> Coordinate -> Dict String Cell -> Dict String MonsterModel -> ( Bool, Dict String Cell )
trySetStepsForCoordinateInFieldIfEmptyOrMovingMonster steps coordinate field monsterDict =
    let
        getCellResult =
            tryGetCellFromFieldByCoordinate coordinate field
    in
    case getCellResult of
        Err _ ->
            ( False, field )

        Ok cell ->
            case cell.content of
                Empty ->
                    ( True, setStepsForCoordinateInFieldUnsafe coordinate steps field )

                Hero ->
                    ( False, field )

                Monster ->
                    let
                        getMonsterResult =
                            tryGetMonsterFromMonsterDictByCoordinate coordinate monsterDict
                    in
                    case getMonsterResult of
                        Err _ ->
                            ( False, field )

                        Ok monster ->
                            case monster.action of
                                Moving ->
                                    ( True, setStepsForCoordinateInFieldUnsafe coordinate steps field )

                                Attacking ->
                                    ( False, field )

                Obstacle _ ->
                    ( False, field )
