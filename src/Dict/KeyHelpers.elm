module Dict.KeyHelpers exposing (..)

import Models exposing (Coordinate)


makeDictKeyFromCoordinate : Coordinate -> String
makeDictKeyFromCoordinate coordinate =
    String.fromInt coordinate.columnNumber ++ "," ++ String.fromInt coordinate.rowNumber
