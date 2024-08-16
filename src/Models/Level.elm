module Models.Level exposing (..)

import Dict exposing (Dict)
import Messages exposing (Msg)
import Models.Cell exposing (Cell, Coordinate)
import Models.Hero exposing (HeroModel, emptyHeroModel)
import Svg exposing (Svg)


type alias Level =
    { playFieldWidth : Int
    , playFieldHeight : Int
    , playField : Dict String Cell
    , heroModel : HeroModel
    , currentAnimations : List (Svg Msg)
    }


emptyLevel : Level
emptyLevel =
    Level 1 1 Dict.empty emptyHeroModel []
