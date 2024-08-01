module Messages exposing (..)

import Browser.Dom exposing (Viewport)


type Msg
    = GotViewport Viewport
    | GotNewSize Int Int
    | KeyPressed String
