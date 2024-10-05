module Models.Hero exposing (..)

import Models.Cell exposing (Coordinate)
import Models.Stats exposing (Stats)


type alias HeroModel =
    { coordinate : Coordinate
    , stats : Stats
    }


emptyHeroModel : HeroModel
emptyHeroModel =
    { coordinate = Coordinate 1 1
    , stats = Stats 20 20
    }
