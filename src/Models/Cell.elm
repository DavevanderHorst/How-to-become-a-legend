module Models.Cell exposing (..)

import Types exposing (CellContent)


type alias Cell =
    { coordinate : Coordinate
    , gridX : Int
    , gridY : Int
    , content : CellContent
    , stepsToHero : Maybe Int
    }


type alias Coordinate =
    { columnNumber : Int
    , rowNumber : Int
    }


type alias CoordinateWithStep =
    -- For pathfinding
    { coordinate : Coordinate
    , steps : Int
    }
