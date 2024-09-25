module Functions.PlayField.Helpers exposing (..)

import Dict exposing (Dict)
import Models.Cell exposing (Cell, Coordinate)
import Types exposing (CellContent(..), ObstacleType)


makeDictKeyFromCoordinate : Coordinate -> String
makeDictKeyFromCoordinate coordinate =
    String.fromInt coordinate.columnNumber ++ "," ++ String.fromInt coordinate.rowNumber


updateGridCellDictByCoordinateUnsafe : Coordinate -> (Maybe Cell -> Maybe Cell) -> Dict String Cell -> Dict String Cell
updateGridCellDictByCoordinateUnsafe coordinate function gridCellDict =
    Dict.update (makeDictKeyFromCoordinate coordinate) function gridCellDict


updateGridCellDictByKeyUnsafe : String -> (Maybe Cell -> Maybe Cell) -> Dict String Cell -> Dict String Cell
updateGridCellDictByKeyUnsafe key function gridCellDict =
    Dict.update key function gridCellDict


setStepsToNothing : String -> Cell -> Cell
setStepsToNothing _ =
    \old -> { old | stepsToHero = Nothing }


setSteps : Int -> Maybe Cell -> Maybe Cell
setSteps steps =
    Maybe.map
        (\old -> { old | stepsToHero = Just steps })


setCellContentToMonster : Maybe Cell -> Maybe Cell
setCellContentToMonster =
    setCellContent Monster


setContentToEmpty : Maybe Cell -> Maybe Cell
setContentToEmpty =
    setCellContent Empty


setCellContentToHero : Maybe Cell -> Maybe Cell
setCellContentToHero =
    setCellContent Hero


setCellContentToObstacle : ObstacleType -> Maybe Cell -> Maybe Cell
setCellContentToObstacle obstacleType =
    setCellContent (Obstacle obstacleType)


setCellContent : CellContent -> Maybe Cell -> Maybe Cell
setCellContent newContent =
    Maybe.map
        (\old -> { old | content = newContent })
