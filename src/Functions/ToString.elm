module Functions.ToString exposing (..)

import Dict exposing (Dict)
import Models.Cell exposing (Coordinate)
import Models.Monster exposing (MonsterModel)
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

        Monster specie ->
            "Monster - " ++ specieToString specie

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


monsterCoordinatesToString : Dict String MonsterModel -> String
monsterCoordinatesToString monsterDict =
    Dict.foldl monsterCoordinateToString "" monsterDict


monsterCoordinateToString : String -> MonsterModel -> String -> String
monsterCoordinateToString key monster stringSoFar =
    stringSoFar ++ "Key : (" ++ key ++ ") " ++ "Coordinate : (" ++ coordinateToString monster.coordinate ++ ") "
