module Functions.PlayField.Set exposing (..)

import Dict exposing (Dict)
import Functions.PlayField.Get exposing (tryGetCellFromPlayFieldByCoordinate)
import Functions.PlayField.KeyHelpers exposing (makeDictKeyFromCoordinate)
import Functions.ToString exposing (cellContentToString)
import Models.Cell exposing (Cell, Coordinate)
import Models.Level exposing (PlayField)
import Models.MainModel exposing (Error)
import Models.Monster exposing (MonsterModel)
import Types exposing (Action(..), CellContent(..), Specie)


setHeroInPlayFieldUnsafe : Coordinate -> Dict String Cell -> Dict String Cell
setHeroInPlayFieldUnsafe coordinate playField =
    -- unsafe, cell will be set to Hero, even if there is something in that cell
    updateGridCellDict coordinate setContentToHero playField


setContentToHero : Maybe Cell -> Maybe Cell
setContentToHero =
    Maybe.map
        (\old -> { old | content = Hero })


trySetMonsterInPlayField : MonsterModel -> Dict String Cell -> Result Error (Dict String Cell)
trySetMonsterInPlayField monster playField =
    let
        monsterCellResult =
            tryGetCellFromPlayFieldByCoordinate monster.coordinate playField
    in
    case monsterCellResult of
        Err err ->
            Err { method = "trySetMonsterInPlayField - " ++ err.method, error = err.error }

        Ok monsterCell ->
            if monsterCell.content == Empty then
                Ok (setMonsterInPlayFieldUnsafe monster playField)

            else
                Err
                    { method = "trySetMonsterInPlayField"
                    , error = "Cell content is not empty for placing monster : " ++ cellContentToString monsterCell.content
                    }


setMonsterInPlayFieldUnsafe : MonsterModel -> Dict String Cell -> Dict String Cell
setMonsterInPlayFieldUnsafe monster playField =
    updateGridCellDict monster.coordinate (setContentToMonster monster.specie monster.action) playField


setContentToMonster : Specie -> Action -> Maybe Cell -> Maybe Cell
setContentToMonster specie action =
    Maybe.map
        (\old -> { old | content = Monster specie action })


removeHeroFromPlayFieldUnsafe : Coordinate -> Dict String Cell -> Dict String Cell
removeHeroFromPlayFieldUnsafe coordinate playField =
    -- unsafe, cell will be set to empty, even if hero is not there, or if cell does not exist
    updateGridCellDict coordinate setContentToEmpty playField


setContentToEmpty : Maybe Cell -> Maybe Cell
setContentToEmpty =
    Maybe.map
        (\old -> { old | content = Empty })


removeMonsterFromPlayFieldUnsafe : MonsterModel -> Dict String Cell -> Dict String Cell
removeMonsterFromPlayFieldUnsafe monster playField =
    updateGridCellDict monster.coordinate setContentToEmpty playField


removeStepsFromPlayField : PlayField -> PlayField
removeStepsFromPlayField playField =
    let
        newField =
            Dict.map setStepsToNothing playField.field
    in
    { playField | field = newField }


setStepsForCoordinateInPlayFieldIfEmptyOrMovingMonster : Int -> Coordinate -> Dict String Cell -> Dict String Cell
setStepsForCoordinateInPlayFieldIfEmptyOrMovingMonster steps coordinate playField =
    updateGridCellDict coordinate (setStepsIfEmptyOrMovingMonster steps) playField


setStepsToNothing : String -> Cell -> Cell
setStepsToNothing _ =
    \old -> { old | stepsToHero = Nothing }


setStepsIfEmptyOrMovingMonster : Int -> Maybe Cell -> Maybe Cell
setStepsIfEmptyOrMovingMonster steps =
    Maybe.map
        (\old ->
            case old.content of
                Empty ->
                    { old | stepsToHero = Just steps }

                Hero ->
                    old

                Monster _ action ->
                    case action of
                        Moving ->
                            { old | stepsToHero = Just steps }

                        Attacking ->
                            old

                Obstacle _ ->
                    old
        )


updateGridCellDict : Coordinate -> (Maybe Cell -> Maybe Cell) -> Dict String Cell -> Dict String Cell
updateGridCellDict roomCoordinate function gridCellDict =
    Dict.update (makeDictKeyFromCoordinate roomCoordinate) function gridCellDict
