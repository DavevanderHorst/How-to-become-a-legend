module Models.MainModel exposing (..)

import Models.Cell exposing (Coordinate)
import Models.Level exposing (Level)
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


type alias Size =
    { width : Float
    , height : Float
    }


startSize : Size
startSize =
    Size 0 0
