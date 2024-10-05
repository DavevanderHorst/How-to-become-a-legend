module Views.Attributes exposing (..)

import Constants.FieldSizes exposing (imageStatsSize, squareSize)
import Constants.Hero exposing (heroWarriorImageLink)
import Constants.Monster exposing (getImageLinkForSpecie, monsterNumberOffsetX, monsterNumberOffsetY)
import Html.Attributes exposing (height, src, style, width)
import Models.Cell exposing (Cell)
import Svg exposing (Attribute)
import Svg.Attributes as SvgAttr
import Types exposing (Specie)
import Views.ViewHelpers exposing (makePxStringFromInt)


makeBaseGridCellAttributes : Cell -> List (Attribute msg)
makeBaseGridCellAttributes cell =
    baseGridCellAttributes cell "white" 0 0


baseGridCellAttributes : Cell -> String -> Int -> Int -> List (Attribute msg)
baseGridCellAttributes cell color xOff yOff =
    [ SvgAttr.x (makePxStringFromInt (cell.gridX + xOff))
    , SvgAttr.y (makePxStringFromInt (cell.gridY + yOff))
    , SvgAttr.fill color
    ]
        ++ baseCellAttributes


monsterNumberImageAttributes : Cell -> List (Attribute msg)
monsterNumberImageAttributes cell =
    baseGridCellAttributes cell "black" monsterNumberOffsetX monsterNumberOffsetY


monsterNumberAnimationAttributes : List (Attribute msg)
monsterNumberAnimationAttributes =
    let
        squareSizeInPixelString =
            makePxStringFromInt squareSize
    in
    [ SvgAttr.width squareSizeInPixelString
    , SvgAttr.height squareSizeInPixelString
    , SvgAttr.x (makePxStringFromInt monsterNumberOffsetX)
    , SvgAttr.y (makePxStringFromInt monsterNumberOffsetY)
    ]


textAttributes : String -> List (Attribute msg)
textAttributes color =
    [ SvgAttr.fill color
    , SvgAttr.fontWeight "950"
    , SvgAttr.fontFamily "Helvetica"
    , SvgAttr.stroke "white"
    ]


monsterNumberStatsAttributes : List (Attribute msg)
monsterNumberStatsAttributes =
    [ style "font-weight" "950"
    , style "font-family" "Helvetica"
    , style "font-size" "20px"
    , style "position" "absolute"
    , style "bottom" "0"
    , style "right" "0"
    ]


heroImageStatsAttributes : List (Attribute msg)
heroImageStatsAttributes =
    imageStatsAttributes heroWarriorImageLink


monsterImageStatsAttributes : Specie -> List (Attribute msg)
monsterImageStatsAttributes specie =
    let
        imageLink =
            getImageLinkForSpecie specie
    in
    imageStatsAttributes imageLink


imageStatsAttributes : String -> List (Attribute msg)
imageStatsAttributes imageLink =
    [ width imageStatsSize
    , height imageStatsSize
    , src imageLink
    ]


baseCellAttributes : List (Attribute msg)
baseCellAttributes =
    let
        squareSizeInPixelString =
            makePxStringFromInt squareSize
    in
    [ SvgAttr.width squareSizeInPixelString
    , SvgAttr.height squareSizeInPixelString
    ]


widthStyle : Int -> Attribute msg
widthStyle width =
    style "width" (makePxStringFromInt width)


heightStyle : Int -> Attribute msg
heightStyle height =
    style "height" (makePxStringFromInt height)


backGroundColorStyle : String -> Attribute msg
backGroundColorStyle color =
    style "background" color


displayFlexStyle : Attribute msg
displayFlexStyle =
    style "display" "flex"


positionAbsoluteStyle : Attribute msg
positionAbsoluteStyle =
    style "position" "absolute"


positionRelativeStyle : Attribute msg
positionRelativeStyle =
    style "position" "relative"
