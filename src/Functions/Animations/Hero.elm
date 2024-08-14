module Functions.Animations.Hero exposing (makeAttackAnimation, makeMoveAnimation)

import Constants.Times exposing (halfAttackAnimationDuration, moveAnimationDuration)
import Models exposing (AnimationType(..), Cell, Coordinate)
import Simple.Animation exposing (Animation, Step, step, steps)
import Simple.Animation.Property as P


makeMoveAnimation : Cell -> Cell -> AnimationType
makeMoveAnimation heroSpot nextSpot =
    -- AnimationMove is using coordinates, but we are using the screen positions here.
    let
        startCoordinate =
            Coordinate heroSpot.gridX heroSpot.gridY

        endCoordinate =
            Coordinate nextSpot.gridX nextSpot.gridY
    in
    HeroAnimation (moveAnimation startCoordinate endCoordinate)


moveAnimation : Coordinate -> Coordinate -> Animation
moveAnimation start end =
    steps
        { startAt = [ P.x (toFloat start.columnNumber), P.y (toFloat start.rowNumber) ]
        , options = []
        }
        [ makeAnimationStep end moveAnimationDuration ]


makeAttackAnimation : Cell -> Cell -> AnimationType
makeAttackAnimation heroSpot nextSpot =
    -- AnimationMove is using coordinates, but we are using the screen positions here.
    let
        startCoordinate =
            Coordinate heroSpot.gridX heroSpot.gridY

        xDistanceToAttack =
            (heroSpot.gridX - nextSpot.gridX) // 2

        yDistanceToAttack =
            (heroSpot.gridY - nextSpot.gridY) // 2

        attackCoordinate =
            Coordinate (heroSpot.gridX - xDistanceToAttack) (heroSpot.gridY - yDistanceToAttack)
    in
    HeroAnimation (attackAnimation startCoordinate attackCoordinate)


attackAnimation : Coordinate -> Coordinate -> Animation
attackAnimation start end =
    steps
        { startAt = [ P.x (toFloat start.columnNumber), P.y (toFloat start.rowNumber) ]
        , options = []
        }
        [ makeAnimationStep end halfAttackAnimationDuration, makeAnimationStep start halfAttackAnimationDuration ]


makeAnimationStep : Coordinate -> Int -> Step
makeAnimationStep coordinate duration =
    step duration [ P.x (toFloat coordinate.columnNumber), P.y (toFloat coordinate.rowNumber) ]
