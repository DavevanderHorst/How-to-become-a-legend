module Functions.PlayField.Set exposing (removeHeroFromPlayFieldUnsafe, setHeroInPlayFieldUnsafe)

import Dict exposing (Dict)
import Functions.PlayField.KeyHelpers exposing (makePlayFieldDictKeyFromCoordinate)
import Models exposing (Cell, Coordinate)
import Types exposing (CellContent(..))


setHeroInPlayFieldUnsafe : Coordinate -> Dict String Cell -> Dict String Cell
setHeroInPlayFieldUnsafe coordinate playField =
    -- unsafe, cell will be set to Hero, even if there is something in that cell
    updateGridCellDict coordinate setContentToHero playField


setContentToHero : Maybe Cell -> Maybe Cell
setContentToHero =
    Maybe.map
        (\old -> { old | content = Hero })


removeHeroFromPlayFieldUnsafe : Coordinate -> Dict String Cell -> Dict String Cell
removeHeroFromPlayFieldUnsafe coordinate playField =
    -- unsafe, cell will be set to empty, even if hero is not there, or if cell does not exist
    updateGridCellDict coordinate setContentToEmpty playField


setContentToEmpty : Maybe Cell -> Maybe Cell
setContentToEmpty =
    Maybe.map
        (\old -> { old | content = Empty })


updateGridCellDict : Coordinate -> (Maybe Cell -> Maybe Cell) -> Dict String Cell -> Dict String Cell
updateGridCellDict roomCoordinate function gridCellDict =
    Dict.update (makePlayFieldDictKeyFromCoordinate roomCoordinate) function gridCellDict
