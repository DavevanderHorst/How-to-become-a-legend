module Constants.FieldSizes exposing (..)


squareSize : Int
squareSize =
    50


imageStatsSize : Int
imageStatsSize =
    2 * squareSize


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
