module Views.MainView exposing (..)

import Constants.FieldSizes exposing (imageStatsSize)
import Constants.Hero exposing (heroWarriorImageLink)
import Constants.Monster exposing (getImageLinkForSpecie)
import Dict exposing (Dict)
import Functions.Monsters.MonsterDict exposing (tryGetMonsterFromMonsterDictByKey)
import Html exposing (Html, audio, div, img, span, text)
import Html.Attributes exposing (height, id, style, width)
import Messages exposing (Msg(..))
import Models.Cell exposing (Cell)
import Models.Hero exposing (HeroModel)
import Models.Level exposing (Level)
import Models.MainModel exposing (MainModel)
import Models.Monster exposing (MonsterModel)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttr exposing (visibility)
import Types exposing (CellContent(..), Display(..), ObstacleType(..), Specie(..))
import Views.Attributes exposing (baseCellAttributes, heroImageStatsAttributes, makeBaseGridCellAttributes, monsterImageStatsAttributes, monsterNumberAnimationAttributes, monsterNumberImageAttributes, monsterNumberStatsAttributes, textAttributes)
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
                , drawHeroAndMonsterStats model.level.heroModel model.level.playField.screenHeight model.level.monsterDict
                , audio [ id "audio-player", visibility "hidden" ] []

                --, div [] [ text (monsterCoordinatesToString model.level.monsterModels) ]
                ]

        Just error ->
            div [] [ div [] [ text error.method ], div [] [ text error.error ] ]


drawHeroAndMonsterStats : HeroModel -> Float -> Dict String MonsterModel -> Svg Msg
drawHeroAndMonsterStats hero height monsters =
    div
        [ -- todo width, as soon as i know
          style "width" "500px"
        , style "height" (makePxStringFromFloat height)
        , style "display" "flex"
        , style "flex-direction" "column"
        ]
        (renderHeroStats hero :: renderMonstersStats monsters)


renderHeroStats : HeroModel -> Svg Msg
renderHeroStats hero =
    img heroImageStatsAttributes []


renderMonstersStats : Dict String MonsterModel -> List (Svg Msg)
renderMonstersStats monsters =
    Dict.foldl renderMonsterStats [] monsters


renderMonsterStats : String -> MonsterModel -> List (Svg Msg) -> List (Svg Msg)
renderMonsterStats _ monster svgList =
    div
        [ style "width" (makePxStringFromInt imageStatsSize)
        , style "height" (makePxStringFromInt imageStatsSize)
        , style "position" "relative"
        ]
        [ img (monsterImageStatsAttributes monster.specie) []
        , span monsterNumberStatsAttributes [ text (String.fromInt monster.number) ]
        ]
        :: svgList


drawLevel : Level -> List (Svg Msg)
drawLevel level =
    -- animation must be first in the list, so that it will be rendered last.
    let
        startSvgList =
            level.animations
    in
    Dict.foldl (drawCell level.monsterDict) startSvgList level.playField.field


drawCell : Dict String MonsterModel -> String -> Cell -> List (Svg Msg) -> List (Svg Msg)
drawCell monsterDict key cell svgList =
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

        Monster ->
            let
                getMonsterResult =
                    tryGetMonsterFromMonsterDictByKey key monsterDict
            in
            case getMonsterResult of
                -- we check if monsters are on good spot in monster turn.
                Err _ ->
                    svgList

                Ok monster ->
                    baseRect :: renderMonsterCell cell monster Image :: svgList

        Obstacle obstacleType ->
            baseRect :: renderObstacleCell obstacleType cell :: svgList


renderMonsterCell : Cell -> MonsterModel -> Display -> Svg msg
renderMonsterCell cell monster displayType =
    let
        ( baseAttributes, textAttributes ) =
            case displayType of
                Image ->
                    ( makeBaseGridCellAttributes cell, monsterNumberImageAttributes cell )

                Animation ->
                    ( baseCellAttributes, monsterNumberAnimationAttributes )

        imageAttributes =
            SvgAttr.xlinkHref (getImageLinkForSpecie monster.specie) :: baseAttributes
    in
    Svg.g []
        [ Svg.image imageAttributes []
        , Svg.text_ textAttributes [ Svg.text (String.fromInt monster.number) ]
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
            SvgAttr.xlinkHref heroWarriorImageLink :: attributes
    in
    Svg.image attributesWithImage []
