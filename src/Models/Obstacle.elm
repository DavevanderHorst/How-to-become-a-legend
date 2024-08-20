module Models.Obstacle exposing (..)

import Models.Cell exposing (Coordinate)
import Types exposing (ObstacleType)


type alias ObstacleModel =
    { coordinate : Coordinate
    , obstacleType : ObstacleType
    }
