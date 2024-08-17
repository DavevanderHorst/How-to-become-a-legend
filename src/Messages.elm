module Messages exposing (..)

import Browser.Dom exposing (Viewport)
import Models.Cell exposing (Cell)
import Types exposing (PressedKey)


type Msg
    = GotViewport Viewport
    | GotNewSize Int Int
    | KeyPressed String
    | HandleKeyPressed PressedKey
    | HeroAnimationIsDone
    | MonsterAnimationsAreDone
    | HeroAttacks Cell Int
    | MonstersTurn
