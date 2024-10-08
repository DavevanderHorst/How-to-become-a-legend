module Functions.Animations.Move exposing (..)

import Constants.Times exposing (heroMoveAnimationDuration, monsterAnimationDuration)
import Functions.Animations.Helpers exposing (animatedG, makeAnimationStep)
import Messages exposing (Msg)
import Models.Cell exposing (Cell, Coordinate)
import Models.Monster exposing (MonsterModel)
import Simple.Animation as Simple exposing (Animation)
import Simple.Animation.Property as P
import Svg exposing (Svg)
import Types exposing (Display(..), Specie)
import Views.MainView exposing (renderHeroCell, renderMonsterCell)


makeHeroMoveAnimationSvg : Cell -> Cell -> Svg Msg
makeHeroMoveAnimationSvg heroCell nextCell =
    makeMoveAnimationSvg (renderHeroCell heroCell Animation) heroMoveAnimationDuration heroCell nextCell


makeMonsterMoveAnimationUnsafe : MonsterModel -> Cell -> Cell -> Svg Msg
makeMonsterMoveAnimationUnsafe monster monsterCell moveToCell =
    makeMoveAnimationSvg (renderMonsterCell monsterCell monster Animation) monsterAnimationDuration monsterCell moveToCell


makeMoveAnimationSvg : Svg Msg -> Int -> Cell -> Cell -> Svg Msg
makeMoveAnimationSvg renderedSvg duration startCell nextCell =
    -- MoveAnimation is using coordinates, but we are using the screen positions here in the coordinate.
    let
        startCoordinate =
            Coordinate startCell.gridX startCell.gridY

        endCoordinate =
            Coordinate nextCell.gridX nextCell.gridY
    in
    animatedG (makeMoveAnimation startCoordinate endCoordinate duration) [] [ renderedSvg ]


makeMoveAnimation : Coordinate -> Coordinate -> Int -> Animation
makeMoveAnimation start end duration =
    Simple.steps
        { startAt = [ P.x (toFloat start.columnNumber), P.y (toFloat start.rowNumber) ]
        , options = []
        }
        [ makeAnimationStep end duration ]
