module Models.Monster exposing (..)

import Models.Cell exposing (Coordinate)
import Models.Stats exposing (Stats)
import Types exposing (Action, Specie)


type alias MonsterModel =
    { coordinate : Coordinate
    , specie : Specie
    , action : Action
    , number : Int
    , stats : Stats
    }
