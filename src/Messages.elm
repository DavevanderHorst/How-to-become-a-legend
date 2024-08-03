module Messages exposing (..)

import Browser.Dom exposing (Viewport)
import Models exposing (PressedKey)


type Msg
    = GotViewport Viewport
    | GotNewSize Int Int
    | KeyPressed String
    | HandleKeyPressed PressedKey
