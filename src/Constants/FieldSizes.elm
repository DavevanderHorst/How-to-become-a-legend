module Constants.FieldSizes exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Views.ViewHelpers exposing (makePxStringFromInt)


squareSize : Int
squareSize =
    50


halfSquareSize : Int
halfSquareSize =
    squareSize // 2


betweenSquaresSize : Int
betweenSquaresSize =
    3


totalSquareSize : Int
totalSquareSize =
    squareSize + betweenSquaresSize


backGroundMargin : Int
backGroundMargin =
    20


totalBackGroundMargin : Int
totalBackGroundMargin =
    2 * backGroundMargin



-- stats next to field from hero and monsters


imageStatsSize : Int
imageStatsSize =
    2 * squareSize


healthBarHeight : Int
healthBarHeight =
    squareSize


healthBarWidth : Int
healthBarWidth =
    4 * squareSize
