module Views.MainView exposing (..)

import Dict exposing (Dict)
import Functions.ToString exposing (monsterCoordinatesToString)
import Html exposing (Html, audio, div, text)
import Html.Attributes exposing (id, style)
import Messages exposing (Msg(..))
import Models.Cell exposing (Cell)
import Models.Level exposing (Level)
import Models.MainModel exposing (MainModel)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttr exposing (visibility)
import Types exposing (CellContent(..), ObstacleType(..), Specie(..))
import Views.Attributes exposing (makeBaseGridCellAttributes)
import Views.ViewHelpers exposing (makePxStringFromFloat)


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
                [ style "width" (makePxStringFromFloat (model.windowSize.width - 100))
                , style "height" (makePxStringFromFloat (model.windowSize.height - 50))
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                ]
                [ Svg.svg
                    [ style "width" (makePxStringFromFloat model.level.playField.screenWidth)
                    , style "height" (makePxStringFromFloat model.level.playField.screenHeight)
                    , style "background-color" "black"
                    ]
                    (drawLevel model.level)
                , audio [ id "audio-player", visibility "hidden" ] []

                --, div [] [ text (monsterCoordinatesToString model.level.monsterModels) ]
                ]

        Just error ->
            div [] [ div [] [ text error.method ], div [] [ text error.error ] ]


drawLevel : Level -> List (Svg Msg)
drawLevel level =
    -- animation must be first in the list, so that it will be rendered last.
    let
        startSvgList =
            level.currentAnimations
    in
    Dict.foldl drawCell startSvgList level.playField.field


drawCell : String -> Cell -> List (Svg Msg) -> List (Svg Msg)
drawCell _ cell svgList =
    -- first parameter is the key of the dict
    let
        baseGridCellAttributes =
            makeBaseGridCellAttributes cell "white" 0 0

        baseRect =
            case cell.stepsToHero of
                Nothing ->
                    Svg.g []
                        [ Svg.rect baseGridCellAttributes []
                        ]

                Just steps ->
                    Svg.g []
                        [ Svg.rect baseGridCellAttributes []
                        , Svg.text_ (makeBaseGridCellAttributes cell "black" 5 20) [ Svg.text (String.fromInt steps) ]
                        ]
    in
    case cell.content of
        Empty ->
            baseRect :: svgList

        Hero ->
            baseRect :: renderHeroCell baseGridCellAttributes :: svgList

        Monster specie ->
            baseRect :: renderMonsterCell specie baseGridCellAttributes :: svgList

        Obstacle obstacleType ->
            baseRect :: renderObstacleCell obstacleType baseGridCellAttributes :: svgList


renderMonsterCell : Specie -> List (Attribute msg) -> Svg msg
renderMonsterCell specie attr =
    let
        imageLink =
            case specie of
                Dummy ->
                    "assets/images/dummyNoBg.png"

        imageAttributes =
            SvgAttr.xlinkHref imageLink :: attr
    in
    Svg.image imageAttributes []


renderObstacleCell : ObstacleType -> List (Attribute msg) -> Svg msg
renderObstacleCell obstacle attr =
    let
        imageLink =
            case obstacle of
                Rock ->
                    "assets/images/rock.png"

        imageAttributes =
            SvgAttr.xlinkHref imageLink :: attr
    in
    Svg.image imageAttributes []


renderHeroCell : List (Attribute msg) -> Svg msg
renderHeroCell attr =
    let
        attributes =
            SvgAttr.xlinkHref "assets/images/swordsmanNoBg.png" :: attr
    in
    Svg.image attributes []