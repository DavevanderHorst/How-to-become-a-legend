module Types exposing (..)


type CellContent
    = Empty
    | Hero
    | Monster Specie
    | Obstacle ObstacleType


type Action
    = Moving
    | Attacking


type ObstacleType
    = Rock


type Specie
    = Dummy


type PlayerInput
    = Stopped
    | Possible


type PressedKey
    = Arrow Direction


type Direction
    = Up
    | UpRight
    | Right
    | DownRight
    | Down
    | DownLeft
    | Left
    | UpLeft
