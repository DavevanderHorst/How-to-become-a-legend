module Messages exposing (..)

import Browser.Dom exposing (Viewport)
import Types exposing (PressedKey)


type Msg
    = GotViewport Viewport
    | GotNewSize Int Int
    | KeyPressed String
    | HandleKeyPressed PressedKey
    | HeroAnimationIsDone
