module Functions.Animations.Attack exposing (makeAttackAnimationSvgs)

import Constants.FieldSizes exposing (halfSquareSize)
import Constants.Times exposing (halfHeroAttackAnimationDuration, heroAttackAnimationDuration)
import Functions.Animations.Base exposing (animatedG, makeAnimationStep)
import Messages exposing (Msg)
import Models.Cell exposing (Cell, Coordinate)
import Simple.Animation as Simple exposing (Animation)
import Simple.Animation.Property as P
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttr
import Views.Attributes exposing (baseCellAttributes)
import Views.MainView exposing (renderHeroCell)


makeAttackAnimationSvgs : Cell -> Cell -> Int -> List (Svg Msg)
makeAttackAnimationSvgs heroSpot nextSpot damage =
    -- AttackAnimation is using coordinates, but we are using the screen positions here.
    let
        heroAttackAnimation =
            animatedG (makeAttackAnimation heroSpot nextSpot) [] [ renderHeroCell baseCellAttributes ]

        damageAnimation =
            animatedG (makeDamageAnimation nextSpot) [] [ Svg.text_ attackTextAttributes [ Svg.text (String.fromInt damage) ] ]
    in
    [ heroAttackAnimation, damageAnimation ]


attackTextAttributes : List (Attribute msg)
attackTextAttributes =
    [ SvgAttr.fill "red"
    , SvgAttr.fontWeight "950"
    , SvgAttr.fontFamily "Helvetica"
    , SvgAttr.stroke "white"
    ]


makeAttackAnimation : Cell -> Cell -> Animation
makeAttackAnimation startCell attackedCell =
    let
        xDistanceToAttack =
            (startCell.gridX - attackedCell.gridX) // 2

        yDistanceToAttack =
            (startCell.gridY - attackedCell.gridY) // 2

        startCoordinate =
            Coordinate startCell.gridX startCell.gridY

        attackCoordinate =
            Coordinate (startCell.gridX - xDistanceToAttack) (startCell.gridY - yDistanceToAttack)
    in
    Simple.steps
        { startAt = [ P.x (toFloat startCell.gridX), P.y (toFloat startCell.gridY) ]
        , options = []
        }
        [ makeAnimationStep attackCoordinate halfHeroAttackAnimationDuration, makeAnimationStep startCoordinate halfHeroAttackAnimationDuration ]


makeDamageAnimation : Cell -> Animation
makeDamageAnimation startCell =
    let
        ( startX, startY ) =
            ( toFloat startCell.gridX, toFloat (startCell.gridY + halfSquareSize) )
    in
    Simple.steps
        { startAt = [ P.x startX, P.y startY ]
        , options = []
        }
        [ Simple.step heroAttackAnimationDuration [ P.x startX, P.y startY, P.scale 3 ] ]
