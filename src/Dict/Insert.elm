module Dict.Insert exposing (..)

import Dict exposing (Dict)
import Dict.Get exposing (tryGetCellFromPlayField)
import Dict.KeyHelpers exposing (makeDictKeyFromCoordinate)
import Functions.ToString exposing (cellContentToString)
import Models exposing (Cell, CellContent(..), Coordinate, Error)


trySetHeroInPlayField : Coordinate -> Dict String Cell -> Result Error (Dict String Cell)
trySetHeroInPlayField coordinate playField =
    let
        dictKey =
            makeDictKeyFromCoordinate coordinate

        getCellResult =
            tryGetCellFromPlayField dictKey playField
    in
    case getCellResult of
        Err err ->
            Err err

        Ok cell ->
            if cell.content == Empty then
                Ok (updateGridCellDict dictKey setCellStateToHero playField)

            else
                Err
                    { method = "Functions.Dict.Insert.trySetHeroInPlayField"
                    , error = "Cant set hero in play field, cell is not empty : " ++ cellContentToString cell.content
                    }


setCellStateToHero : Maybe Cell -> Maybe Cell
setCellStateToHero =
    Maybe.map
        (\old -> { old | content = Hero })


updateGridCellDict : String -> (Maybe Cell -> Maybe Cell) -> Dict String Cell -> Dict String Cell
updateGridCellDict key function gridCellDict =
    -- Unsafe, only use when your 100% sure cellState is Empty
    Dict.update key function gridCellDict
