module Functions.PlayField.KeyHelpers exposing (..)

import Models.Cell exposing (Coordinate)


makePlayFieldDictKeyFromCoordinate : Coordinate -> String
makePlayFieldDictKeyFromCoordinate coordinate =
    String.fromInt coordinate.columnNumber ++ "," ++ String.fromInt coordinate.rowNumber
