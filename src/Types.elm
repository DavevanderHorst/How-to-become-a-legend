module Types exposing (..)


type CellContent
    = Empty
    | Hero
    | Monster Specie


type Specie
    = Dummy


type PlayerInput
    = Stopped
    | Possible


type PressedKey
    = Arrow Direction


type Direction
    = Up
    | Down
    | Right
    | Left
