module Models.Monster exposing (..)

import Models.Cell exposing (Coordinate)
import Types exposing (Action, Specie)


type alias MonsterModel =
    { coordinate : Coordinate
    , specie : Specie
    , action : Action
    }
