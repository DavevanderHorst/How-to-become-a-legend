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
    | Down
    | Right
    | Left
