module Models exposing (..)

import Dict exposing (Dict)


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
    { playField : Dict String Cell
    , heroCoordinate : Coordinate
    , playFieldWidth : Int
    , playFieldHeight : Int
    }


emptyLevel : Level
emptyLevel =
    Level Dict.empty (Coordinate 1 1) 1 1


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


type alias MonsterModel =
    { coordinate : Coordinate, specie : Specie }


type CellContent
    = Empty
    | Hero
    | Monster Specie


type Specie
    = Dummy


type PlayerInput
    = Stopped
    | Possible


type PressedKey
    = Arrow Direction


type Direction
    = Up
    | Down
    | Right
    | Left


startSize : Size
startSize =
    Size 0 0
