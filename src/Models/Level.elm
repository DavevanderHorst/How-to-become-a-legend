module Models.Level exposing (..)

import Dict exposing (Dict)
import Messages exposing (Msg)
import Models.Cell exposing (Cell, Coordinate)
import Models.Hero exposing (HeroModel, emptyHeroModel)
import Models.Monster exposing (MonsterModel)
import Svg exposing (Svg)


type alias Level =
    { playField : PlayField
    , heroModel : HeroModel
    , monsterModels : Dict String MonsterModel
    , currentAnimations : List (Svg Msg)
    }


type alias PlayField =
    { width : Int
    , height : Int
    , field : Dict String Cell
    }


emptyPlayField : PlayField
emptyPlayField =
    PlayField 1 1 Dict.empty


emptyLevel : Level
emptyLevel =
    Level emptyPlayField emptyHeroModel Dict.empty []
