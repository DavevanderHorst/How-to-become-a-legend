module Functions.Animations.Idle exposing (..)

import Constants.Times exposing (halfMonsterAnimationDuration)
import Functions.Animations.Helpers exposing (animatedG, makeAnimationStep)
import Messages exposing (Msg)
import Models.Cell exposing (Cell, Coordinate)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Property as P
import Svg exposing (Svg)
import Types exposing (Specie)
import Views.Attributes exposing (baseCellAttributes)
import Views.MainView exposing (renderMonsterCell)


makeMonsterIdleAnimationUnsafe : Specie -> Cell -> Svg Msg
makeMonsterIdleAnimationUnsafe specie cell =
    let
        animationCoordinate =
            Coordinate cell.gridX cell.gridY
    in
    animatedG (makeIdleAnimation animationCoordinate) [] [ renderMonsterCell specie baseCellAttributes ]


makeIdleAnimation : Coordinate -> Animation
makeIdleAnimation coordinate =
    Animation.steps
        { startAt = [ P.x (toFloat coordinate.columnNumber), P.y (toFloat coordinate.rowNumber) ]
        , options = []
        }
        [ Animation.step halfMonsterAnimationDuration [ P.x (toFloat coordinate.columnNumber), P.y (toFloat coordinate.rowNumber), P.scale 2 ]
        , makeAnimationStep coordinate halfMonsterAnimationDuration
        ]
