module Functions.ToString exposing (..)

import Models exposing (CellContent(..), Coordinate)


coordinateToString : Coordinate -> String
coordinateToString coordinate =
    "(" ++ String.fromInt coordinate.columnNumber ++ "," ++ String.fromInt coordinate.rowNumber ++ ")"


cellContentToString : CellContent -> String
cellContentToString content =
    case content of
        Empty ->
            "Empty"

        Hero ->
            "Hero"
