module Views.MainView exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, audio, div, text)
import Html.Attributes exposing (id, style)
import Messages exposing (Msg(..))
import Models.Cell exposing (Cell)
import Models.Level exposing (Level)
import Models.MainModel exposing (MainModel)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttr exposing (visibility)
import Types exposing (CellContent(..), Display(..), ObstacleType(..), Specie(..))
import Views.Attributes exposing (baseCellAttributes, makeBaseGridCellAttributes, monsterNumberAnimationAttributes, monsterNumberImageAttributes)
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
                [ style "width" (makePxStringFromFloat (model.windowSize.width - 30))
                , style "height" (makePxStringFromFloat (model.windowSize.height - 30))
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                ]
                [ Svg.svg
                    [ style "width" (makePxStringFromFloat model.level.playField.screenWidth)
                    , style "height" (makePxStringFromFloat model.level.playField.screenHeight)
                    , style "background-color" "WhiteSmoke"
                    , style "border" "10px solid"
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
            level.animations
    in
    Dict.foldl drawCell startSvgList level.playField.field


drawCell : String -> Cell -> List (Svg Msg) -> List (Svg Msg)
drawCell _ cell svgList =
    -- first parameter is the key of the dict
    let
        baseRect =
            Svg.g []
                [ Svg.rect (makeBaseGridCellAttributes cell) []
                ]

        --case cell.stepsToHero of
        --    Nothing ->
        --        Svg.g []
        --            [ Svg.rect baseGridCellAttributes []
        --            ]
        --
        --    Just steps ->
        --        Svg.g []
        --            [ Svg.rect baseGridCellAttributes []
        --            , Svg.text_ (makeBaseGridCellAttributes cell "black" 5 20) [ Svg.text (String.fromInt steps) ]
        --            ]
        --Next lines are to see steps in playField
    in
    case cell.content of
        Empty ->
            baseRect :: svgList

        Hero ->
            baseRect :: renderHeroCell cell Image :: svgList

        Monster specie _ ->
            baseRect :: renderMonsterCell cell specie Image :: svgList

        Obstacle obstacleType ->
            baseRect :: renderObstacleCell obstacleType cell :: svgList


renderMonsterCell : Cell -> Specie -> Display -> Svg msg
renderMonsterCell cell specie displayType =
    let
        ( baseAttributes, textAttributes ) =
            case displayType of
                Image ->
                    ( makeBaseGridCellAttributes cell, monsterNumberImageAttributes cell )

                Animation ->
                    ( baseCellAttributes, monsterNumberAnimationAttributes )

        imageLink =
            case specie of
                Dummy ->
                    "assets/images/dummyNoBg.png"

        imageAttributes =
            SvgAttr.xlinkHref imageLink :: baseAttributes
    in
    Svg.g []
        [ Svg.image imageAttributes []
        , Svg.text_ textAttributes [ Svg.text "3" ]
        ]


renderObstacleCell : ObstacleType -> Cell -> Svg msg
renderObstacleCell obstacle cell =
    let
        imageLink =
            case obstacle of
                Rock ->
                    "assets/images/rock.png"

        imageAttributes =
            SvgAttr.xlinkHref imageLink :: makeBaseGridCellAttributes cell
    in
    Svg.image imageAttributes []


renderHeroCell : Cell -> Display -> Svg msg
renderHeroCell cell display =
    let
        attributes =
            case display of
                Image ->
                    makeBaseGridCellAttributes cell

                Animation ->
                    baseCellAttributes

        attributesWithImage =
            SvgAttr.xlinkHref "assets/images/swordsmanNoBg.png" :: attributes
    in
    Svg.image attributesWithImage []
