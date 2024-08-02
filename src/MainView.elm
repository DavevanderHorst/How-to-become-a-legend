module MainView exposing (..)

import Constants.FieldSizes exposing (squareSize)
import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Messages exposing (Msg)
import Models exposing (Cell, MainModel)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttr
import Views.ViewHelpers exposing (makePxStringFromFloat, makePxStringFromInt)


mainView : MainModel -> Html Msg
mainView model =
    --let
    --minWindowWidth =
    --    playFieldWidth + rightColumnWidth + marginBetweenBlocks
    --
    --minWindowHeight =
    --    playFieldHeight
    --in
    --if width < toFloat minWindowWidth || height < toFloat minWindowHeight then
    --    errorScreen
    --
    --else
    div
        [ style "width" (makePxStringFromFloat model.windowSize.width)
        , style "height" (makePxStringFromFloat model.windowSize.height)
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Svg.svg
            [ style "width" (makePxStringFromInt model.level.playFieldWidth)
            , style "height" (makePxStringFromInt model.level.playFieldHeight)
            , style "background-color" "black"
            ]
            [ Svg.g [] (drawCells model.level.playFieldDict) ]
        ]


drawCells : Dict String Cell -> List (Svg Msg)
drawCells cellDict =
    Dict.foldl drawCell [] cellDict


drawCell : String -> Cell -> List (Svg Msg) -> List (Svg Msg)
drawCell _ cell svgList =
    -- first parameter is the key of the dict
    let
        baseGridCellAttributes =
            makeBaseGridCellAttributes cell
    in
    Svg.rect baseGridCellAttributes [] :: svgList


makeBaseGridCellAttributes : Cell -> List (Attribute msg)
makeBaseGridCellAttributes cell =
    let
        squareSizeInPixelString =
            makePxStringFromInt squareSize
    in
    [ SvgAttr.width squareSizeInPixelString
    , SvgAttr.height squareSizeInPixelString
    , SvgAttr.x (makePxStringFromInt cell.gridX)
    , SvgAttr.y (makePxStringFromInt cell.gridY)
    , SvgAttr.fill "white"
    ]
