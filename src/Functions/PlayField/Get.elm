module Functions.PlayField.Get exposing (..)

import Dict exposing (Dict)
import Functions.PlayField.KeyHelpers exposing (makePlayFieldDictKeyFromCoordinate)
import Models.Cell exposing (Cell, Coordinate)
import Models.MainModel exposing (Error)


tryGetCellFromPlayFieldByKey : String -> Dict String Cell -> Result Error Cell
tryGetCellFromPlayFieldByKey key playField =
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


tryGetCellFromPlayFieldByCoordinate : Coordinate -> Dict String Cell -> Result Error Cell
tryGetCellFromPlayFieldByCoordinate coordinate playField =
    let
        key =
            makePlayFieldDictKeyFromCoordinate coordinate
    in
    tryGetCellFromPlayFieldByKey key playField


getMaybeStepsForCoordinateInPlayField : Coordinate -> Dict String Cell -> Maybe Int
getMaybeStepsForCoordinateInPlayField coordinate playField =
    let
        key =
            makePlayFieldDictKeyFromCoordinate coordinate

        getCellResult =
            tryGetCellFromPlayFieldByKey key playField
    in
    case getCellResult of
        Err _ ->
            Nothing

        Ok cell ->
            cell.stepsToHero
