module Functions.PlayField.KeyHelpers exposing (..)

import Models.Cell exposing (Coordinate)


makeDictKeyFromCoordinate : Coordinate -> String
makeDictKeyFromCoordinate coordinate =
    String.fromInt coordinate.columnNumber ++ "," ++ String.fromInt coordinate.rowNumber
