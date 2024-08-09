module MainView exposing (..)

import Constants.FieldSizes exposing (squareSize)
import Dict exposing (Dict)
import Html exposing (Html, audio, div, text)
import Html.Attributes exposing (id, style)
import Messages exposing (Msg(..))
import Models exposing (Cell, CellContent(..), MainModel)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttr exposing (visibility)
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
    case model.error of
        Nothing ->
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
                    [ Svg.g [] (drawCells model.level.playField) ]
                , audio [ id "audio-player", visibility "hidden" ] []
                ]

        Just error ->
            div [] [ div [] [ text error.method ], div [] [ text error.error ] ]


drawCells : Dict String Cell -> List (Svg Msg)
drawCells cellDict =
    Dict.foldl drawCell [] cellDict


drawCell : String -> Cell -> List (Svg Msg) -> List (Svg Msg)
drawCell _ cell svgList =
    -- first parameter is the key of the dict
    let
        baseGridCellAttributes =
            makeBaseGridCellAttributes cell

        baseRect =
            Svg.rect baseGridCellAttributes []
    in
    case cell.content of
        Empty ->
            baseRect :: svgList

        Hero ->
            let
                imageAttributes =
                    SvgAttr.xlinkHref "assets/images/swordsmanNoBg.png" :: baseGridCellAttributes
            in
            baseRect :: Svg.image imageAttributes [] :: svgList

        Monster specie ->
            let
                imageLink =
                    case specie of
                        Models.Dummy ->
                            "assets/images/dummyNoBg.png"

                imageAttributes =
                    SvgAttr.xlinkHref imageLink :: baseGridCellAttributes
            in
            baseRect :: Svg.image imageAttributes [] :: svgList


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
