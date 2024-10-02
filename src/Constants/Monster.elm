module Constants.Monster exposing (..)

import Constants.FieldSizes exposing (squareSize)
import Types exposing (Specie(..))


getDamageForSpecie : Specie -> Int
getDamageForSpecie specie =
    case specie of
        Dummy ->
            1


getImageLinkForSpecie : Specie -> String
getImageLinkForSpecie specie =
    case specie of
        Dummy ->
            "assets/images/dummyNoBg.png"


monsterNumberOffsetX : Int
monsterNumberOffsetX =
    squareSize - 7


monsterNumberOffsetY : Int
monsterNumberOffsetY =
    squareSize
