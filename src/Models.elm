module Models exposing (..)


type alias MainModel =
    { windowSize : Size
    , error : Maybe String
    , playerInput : PlayerInput
    }


type PlayerInput
    = Stopped
    | Possible


type alias Size =
    { width : Float
    , height : Float
    }


startSize : Size
startSize =
    Size 0 0
