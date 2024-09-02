module Messages exposing (..)

import Browser.Dom exposing (Viewport)
import Models.Cell exposing (Cell)
import Models.Monster exposing (MonsterModel)
import Types exposing (PressedKey)


type Msg
    = GotViewport Viewport
    | GotNewSize Int Int
    | KeyPressed String
    | HandleKeyPressed PressedKey
    | HeroAnimationIsDone
    | MonsterAnimationIsDone MonsterModel (List MonsterModel)
    | HeroAttacks Cell Int
    | MonstersTurn
