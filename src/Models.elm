module Models exposing (..)

import Dict exposing (Dict)


type alias MainModel =
    { windowSize : Size
    , error : Maybe String
    , playerInput : PlayerInput
    , level : Level
    }


type alias Level =
    { playFieldDict : Dict String Cell
    , playFieldWidth : Int
    , playFieldHeight : Int
    }


type alias Cell =
    { coordinate : Coordinate
    , gridX : Int
    , gridY : Int
    }


type alias Coordinate =
    { rowNumber : Int
    , columnNumber : Int
    }


type alias Size =
    { width : Float
    , height : Float
    }


type PlayerInput
    = Stopped
    | Possible


startSize : Size
startSize =
    Size 0 0
