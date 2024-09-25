module Functions.PlayField.Get exposing (..)

import Dict exposing (Dict)
import Functions.PlayField.Helpers exposing (makeDictKeyFromCoordinate)
import Models.Cell exposing (Cell, Coordinate)
import Models.MainModel exposing (Error)


tryGetCellFromFieldByCoordinate : Coordinate -> Dict String Cell -> Result Error Cell
tryGetCellFromFieldByCoordinate coordinate playField =
    let
        key =
            makeDictKeyFromCoordinate coordinate
    in
    tryGetCellFromFieldByKey key playField


tryGetCellFromFieldByKey : String -> Dict String Cell -> Result Error Cell
tryGetCellFromFieldByKey key playField =
    let
        maybeCell =
            Dict.get key playField
    in
    case maybeCell of
        Nothing ->
            Err
                { method = "Functions.Dict.Get.tryGetCellFromPlayField"
                , error = "Key is not in our play field : " ++ key
                }

        Just cell ->
            Ok cell


getMaybeStepsForCoordinateInField : Coordinate -> Dict String Cell -> Maybe Int
getMaybeStepsForCoordinateInField coordinate playField =
    let
        key =
            makeDictKeyFromCoordinate coordinate

        getCellResult =
            tryGetCellFromFieldByKey key playField
    in
    case getCellResult of
        Err _ ->
            Nothing

        Ok cell ->
            cell.stepsToHero
