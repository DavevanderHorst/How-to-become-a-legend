module Types exposing (..)


type CellContent
    = Empty
    | Hero
    | Monster Specie


type Specie
    = Dummy Int


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
