module Dict.Get exposing (..)

import Dict exposing (Dict)
import Models exposing (Cell, Coordinate, Error)


tryGetCellFromPlayField : String -> Dict String Cell -> Result Error Cell
tryGetCellFromPlayField key playField =
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
