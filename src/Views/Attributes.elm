module Views.Attributes exposing (..)

import Constants.FieldSizes exposing (squareSize)
import Models.Cell exposing (Cell)
import Svg exposing (Attribute)
import Svg.Attributes as SvgAttr
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
    baseGridCellAttributes cell "black" 5 20


monsterNumberAnimationAttributes : List (Attribute msg)
monsterNumberAnimationAttributes =
    let
        squareSizeInPixelString =
            makePxStringFromInt squareSize
    in
    [ SvgAttr.width squareSizeInPixelString
    , SvgAttr.height squareSizeInPixelString
    , SvgAttr.x (makePxStringFromInt 5)
    , SvgAttr.y (makePxStringFromInt 20)
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


textAttributes : String -> List (Attribute msg)
textAttributes color =
    [ SvgAttr.fill color
    , SvgAttr.fontWeight "950"
    , SvgAttr.fontFamily "Helvetica"
    , SvgAttr.stroke "white"
    ]
