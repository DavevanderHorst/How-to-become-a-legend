module Functions.Animations.Hero exposing (makeAttackAnimationSvgs, makeMoveAnimationSvgs)

import Constants.FieldSizes exposing (halfSquareSize)
import Constants.Times exposing (attackAnimationDuration, halfAttackAnimationDuration, moveAnimationDuration)
import MainView exposing (baseCellAttributes, renderHeroCell)
import Messages exposing (Msg)
import Models.Cell exposing (Cell, Coordinate)
import Simple.Animation exposing (Animation, Step, step, steps)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttr


makeMoveAnimationSvgs : Cell -> Cell -> List (Svg Msg)
makeMoveAnimationSvgs heroSpot nextSpot =
    -- MoveAnimation is using coordinates, but we are using the screen positions here.
    let
        startCoordinate =
            Coordinate heroSpot.gridX heroSpot.gridY

        endCoordinate =
            Coordinate nextSpot.gridX nextSpot.gridY
    in
    [ animatedG (makeMoveAnimation startCoordinate endCoordinate) [] [ renderHeroCell baseCellAttributes ] ]


makeAttackAnimationSvgs : Cell -> Cell -> Int -> List (Svg Msg)
makeAttackAnimationSvgs heroSpot nextSpot damage =
    -- AttackAnimation is using coordinates, but we are using the screen positions here.
    let
        heroAttackAnimation =
            animatedG (makeAttackAnimation heroSpot nextSpot) [] [ renderHeroCell baseCellAttributes ]

        damageAnimation =
            animatedG (makeDamageAnimation nextSpot) [] [ Svg.text_ textAttributes [ Svg.text (String.fromInt damage) ] ]
    in
    [ heroAttackAnimation, damageAnimation ]


textAttributes : List (Attribute msg)
textAttributes =
    [ SvgAttr.fill "red"
    , SvgAttr.fontWeight "950"
    , SvgAttr.fontFamily "Helvetica"
    , SvgAttr.stroke "white"
    ]


makeMoveAnimation : Coordinate -> Coordinate -> Animation
makeMoveAnimation start end =
    steps
        { startAt = [ P.x (toFloat start.columnNumber), P.y (toFloat start.rowNumber) ]
        , options = []
        }
        [ makeAnimationStep end moveAnimationDuration ]


makeAttackAnimation : Cell -> Cell -> Animation
makeAttackAnimation heroCell monsterCell =
    let
        xDistanceToAttack =
            (heroCell.gridX - monsterCell.gridX) // 2

        yDistanceToAttack =
            (heroCell.gridY - monsterCell.gridY) // 2

        startCoordinate =
            Coordinate heroCell.gridX heroCell.gridY

        attackCoordinate =
            Coordinate (heroCell.gridX - xDistanceToAttack) (heroCell.gridY - yDistanceToAttack)
    in
    steps
        { startAt = [ P.x (toFloat heroCell.gridX), P.y (toFloat heroCell.gridY) ]
        , options = []
        }
        [ makeAnimationStep attackCoordinate halfAttackAnimationDuration, makeAnimationStep startCoordinate halfAttackAnimationDuration ]


makeDamageAnimation : Cell -> Animation
makeDamageAnimation startCell =
    let
        ( startX, startY ) =
            ( toFloat startCell.gridX, toFloat (startCell.gridY + halfSquareSize) )
    in
    steps
        { startAt = [ P.x startX, P.y startY ]
        , options = []
        }
        [ step attackAnimationDuration [ P.x startX, P.y startY, P.scale 3 ] ]


makeAnimationStep : Coordinate -> Int -> Step
makeAnimationStep coordinate duration =
    step duration [ P.x (toFloat coordinate.columnNumber), P.y (toFloat coordinate.rowNumber) ]


animatedG : Animation -> List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
animatedG =
    animatedSvg Svg.g


animatedSvg =
    Animated.svg
        { class = SvgAttr.class
        }
