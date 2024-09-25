module Functions.ToString exposing (..)

import Models.Cell exposing (Coordinate)
import Types exposing (CellContent(..), ObstacleType(..), Specie(..))


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

        Monster ->
            "Monster"

        Obstacle obstacleType ->
            "Obstacle - " ++ obstacleTypeToString obstacleType


specieToString : Specie -> String
specieToString specie =
    case specie of
        Dummy ->
            "Dummy"


obstacleTypeToString : ObstacleType -> String
obstacleTypeToString obstacleType =
    case obstacleType of
        Rock ->
            "rock"
