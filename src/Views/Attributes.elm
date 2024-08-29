module Views.Attributes exposing (..)

import Constants.FieldSizes exposing (squareSize)
import Models.Cell exposing (Cell)
import Svg exposing (Attribute)
import Svg.Attributes as SvgAttr
import Views.ViewHelpers exposing (makePxStringFromInt)


makeBaseGridCellAttributes : Cell -> String -> Int -> Int -> List (Attribute msg)
makeBaseGridCellAttributes cell color xOff yOff =
    [ SvgAttr.x (makePxStringFromInt (cell.gridX + xOff))
    , SvgAttr.y (makePxStringFromInt (cell.gridY + yOff))
    , SvgAttr.fill color
    ]
        ++ baseCellAttributes


baseCellAttributes : List (Attribute msg)
baseCellAttributes =
    let
        squareSizeInPixelString =
            makePxStringFromInt squareSize
    in
    [ SvgAttr.width squareSizeInPixelString
    , SvgAttr.height squareSizeInPixelString
    ]
