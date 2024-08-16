module Models.Hero exposing (..)

import Models.Cell exposing (Coordinate)


type alias HeroModel =
    { coordinate : Coordinate }


emptyHeroModel : HeroModel
emptyHeroModel =
    HeroModel (Coordinate 1 1)
