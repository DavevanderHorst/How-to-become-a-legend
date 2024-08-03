module Dict.Insert exposing (..)

import Dict exposing (Dict)
import Dict.Get exposing (tryGetCellFromCellDict)
import Dict.KeyHelpers exposing (makeDictKeyFromCoordinate)
import Functions.ToString exposing (cellContentToString)
import Models exposing (Cell, CellContent(..), Coordinate, Error)


trySetHeroInMapCellDict : Coordinate -> Dict String Cell -> Result Error (Dict String Cell)
trySetHeroInMapCellDict coordinate mapCellDict =
    let
        dictKey =
            makeDictKeyFromCoordinate coordinate

        getCellResult =
            tryGetCellFromCellDict dictKey mapCellDict
    in
    case getCellResult of
        Err err ->
            Err err

        Ok mapCell ->
            if mapCell.content == Empty then
                Ok (updateGridCellDict dictKey setCellStateToHero mapCellDict)

            else
                Err
                    { method = "Functions.Dict.Insert.trySetHeroInMapCellDict"
                    , error = "Cant set hero, cell is not empty : " ++ cellContentToString mapCell.content
                    }


setCellStateToHero : Maybe Cell -> Maybe Cell
setCellStateToHero =
    Maybe.map
        (\old -> { old | content = Hero })


updateGridCellDict : String -> (Maybe Cell -> Maybe Cell) -> Dict String Cell -> Dict String Cell
updateGridCellDict key function gridCellDict =
    -- Unsafe, only use when your 100% sure cellState is Empty
    Dict.update key function gridCellDict
