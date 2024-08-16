module Models.Cell exposing (..)

import Types exposing (CellContent)


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
