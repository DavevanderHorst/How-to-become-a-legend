module Functions.PlayField.Set exposing (removeHeroFromPlayFieldUnsafe, removeMonstersFromPlayFieldUnSafe, removeStepsFromPlayField, setHeroInPlayFieldUnsafe, setMonstersInPlayFieldUnsafe, setStepsForCoordinateInPlayFieldIfEmptyOrMonster)

import Dict exposing (Dict)
import Functions.PlayField.KeyHelpers exposing (makePlayFieldDictKeyFromCoordinate)
import Models.Cell exposing (Cell, Coordinate)
import Models.Level exposing (PlayField)
import Models.Monster exposing (MonsterModel)
import Types exposing (CellContent(..), Specie)


setHeroInPlayFieldUnsafe : Coordinate -> Dict String Cell -> Dict String Cell
setHeroInPlayFieldUnsafe coordinate playField =
    -- unsafe, cell will be set to Hero, even if there is something in that cell
    updateGridCellDict coordinate setContentToHero playField


setContentToHero : Maybe Cell -> Maybe Cell
setContentToHero =
    Maybe.map
        (\old -> { old | content = Hero })


setMonstersInPlayFieldUnsafe : Dict String MonsterModel -> Dict String Cell -> Dict String Cell
setMonstersInPlayFieldUnsafe monsterDict playField =
    Dict.foldl setMonsterInPlayFieldUnsafe playField monsterDict


setMonsterInPlayFieldUnsafe : String -> MonsterModel -> Dict String Cell -> Dict String Cell
setMonsterInPlayFieldUnsafe _ monster playField =
    updateGridCellDict monster.coordinate (setContentToMonster monster.specie) playField


setContentToMonster : Specie -> Maybe Cell -> Maybe Cell
setContentToMonster specie =
    Maybe.map
        (\old -> { old | content = Monster specie })


removeHeroFromPlayFieldUnsafe : Coordinate -> Dict String Cell -> Dict String Cell
removeHeroFromPlayFieldUnsafe coordinate playField =
    -- unsafe, cell will be set to empty, even if hero is not there, or if cell does not exist
    updateGridCellDict coordinate setContentToEmpty playField


setContentToEmpty : Maybe Cell -> Maybe Cell
setContentToEmpty =
    Maybe.map
        (\old -> { old | content = Empty })


removeMonstersFromPlayFieldUnSafe : Dict String MonsterModel -> Dict String Cell -> Dict String Cell
removeMonstersFromPlayFieldUnSafe monsterDict playField =
    Dict.foldl removeMonsterInPlayFieldUnsafe playField monsterDict


removeMonsterInPlayFieldUnsafe : String -> MonsterModel -> Dict String Cell -> Dict String Cell
removeMonsterInPlayFieldUnsafe _ monster playField =
    updateGridCellDict monster.coordinate setContentToEmpty playField


removeStepsFromPlayField : PlayField -> PlayField
removeStepsFromPlayField playField =
    let
        newField =
            Dict.map setStepsToNothing playField.field
    in
    { playField | field = newField }


setStepsForCoordinateInPlayFieldIfEmptyOrMonster : Int -> Coordinate -> Dict String Cell -> Dict String Cell
setStepsForCoordinateInPlayFieldIfEmptyOrMonster steps coordinate playField =
    updateGridCellDict coordinate (setStepsIfEmptyOrMonster steps) playField


setStepsToNothing : String -> Cell -> Cell
setStepsToNothing _ =
    \old -> { old | stepsToHero = Nothing }


setStepsIfEmptyOrMonster : Int -> Maybe Cell -> Maybe Cell
setStepsIfEmptyOrMonster steps =
    Maybe.map
        (\old ->
            case old.content of
                Empty ->
                    { old | stepsToHero = Just steps }

                Hero ->
                    old

                Monster _ ->
                    { old | stepsToHero = Just steps }

                Obstacle _ ->
                    old
        )


updateGridCellDict : Coordinate -> (Maybe Cell -> Maybe Cell) -> Dict String Cell -> Dict String Cell
updateGridCellDict roomCoordinate function gridCellDict =
    Dict.update (makePlayFieldDictKeyFromCoordinate roomCoordinate) function gridCellDict
