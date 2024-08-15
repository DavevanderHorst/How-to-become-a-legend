module Models exposing (..)

import Dict exposing (Dict)
import Messages exposing (Msg)
import Svg exposing (Svg)
import Types exposing (CellContent, PlayerInput, Specie)


type alias MainModel =
    { windowSize : Size
    , error : Maybe Error
    , playerInput : PlayerInput
    , level : Level
    }


type alias Error =
    { -- The method in which the error occurred
      method : String

    -- the error explained in details
    , error : String
    }


type alias Level =
    { playFieldWidth : Int
    , playFieldHeight : Int
    , playField : Dict String Cell
    , heroCoordinate : Coordinate
    , currentAnimations : List (Svg Msg)
    }


emptyLevel : Level
emptyLevel =
    Level 1 1 Dict.empty (Coordinate 1 1) []


type alias Cell =
    { coordinate : Coordinate
    , gridX : Int
    , gridY : Int
    , content : CellContent
    }


type alias Coordinate =
    { columnNumber : Int
    , rowNumber : Int
    }


type alias Size =
    { width : Float
    , height : Float
    }


startSize : Size
startSize =
    Size 0 0


type alias MonsterModel =
    { coordinate : Coordinate, specie : Specie }
