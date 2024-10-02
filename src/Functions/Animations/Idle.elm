module Functions.Animations.Idle exposing (..)

import Constants.Times exposing (halfMonsterAnimationDuration)
import Functions.Animations.Helpers exposing (animatedG, makeAnimationStep)
import Messages exposing (Msg)
import Models.Cell exposing (Cell, Coordinate)
import Models.Monster exposing (MonsterModel)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Property as P
import Svg exposing (Svg)
import Types exposing (Display(..), Specie)
import Views.MainView exposing (renderMonsterCell)


makeMonsterIdleAnimationUnsafe : MonsterModel -> Cell -> Svg Msg
makeMonsterIdleAnimationUnsafe monster cell =
    let
        animationCoordinate =
            Coordinate cell.gridX cell.gridY
    in
    animatedG (makeIdleAnimation animationCoordinate) [] [ renderMonsterCell cell monster Animation ]


makeIdleAnimation : Coordinate -> Animation
makeIdleAnimation coordinate =
    Animation.steps
        { startAt = [ P.x (toFloat coordinate.columnNumber), P.y (toFloat coordinate.rowNumber) ]
        , options = []
        }
        [ Animation.step halfMonsterAnimationDuration [ P.x (toFloat coordinate.columnNumber), P.y (toFloat coordinate.rowNumber), P.scale 2 ]
        , makeAnimationStep coordinate halfMonsterAnimationDuration
        ]
