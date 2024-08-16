module Models.Monster exposing (..)

import Models.Cell exposing (Coordinate)
import Types exposing (Specie)


type alias MonsterModel =
    { coordinate : Coordinate, specie : Specie }
