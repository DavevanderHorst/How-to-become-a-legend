module Dict.Get exposing (..)

import Dict exposing (Dict)
import Models exposing (Cell, Coordinate, Error)


tryGetCellFromCellDict : String -> Dict String Cell -> Result Error Cell
tryGetCellFromCellDict key cellDict =
    let
        maybeCell =
            Dict.get key cellDict
    in
    case maybeCell of
        Nothing ->
            Err
                { method = "Functions.Dict.Get.tryGetCellFromCellDict"
                , error = "Key is not in our cell dict : " ++ key
                }

        Just mapCell ->
            Ok mapCell
