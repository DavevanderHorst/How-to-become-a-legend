module Functions.Animations.Attack exposing (..)

import Constants.FieldSizes exposing (halfSquareSize)
import Constants.Monster exposing (getDamageForSpecie)
import Constants.Times exposing (halfHeroAttackAnimationDuration, halfMonsterAnimationDuration, heroAttackAnimationDuration, monsterAnimationDuration)
import Functions.Animations.Helpers exposing (animatedG, makeAnimationStep)
import Messages exposing (Msg)
import Models.Cell exposing (Cell, Coordinate)
import Simple.Animation as Simple exposing (Animation)
import Simple.Animation.Property as P
import Svg exposing (Attribute, Svg)
import Types exposing (CellContent(..), Display(..), Specie)
import Views.Attributes exposing (textAttributes)
import Views.MainView exposing (renderHeroCell, renderMonsterCell)


makeMonsterAttackAnimationSvgsUnsafe : Cell -> Specie -> Cell -> List (Svg Msg)
makeMonsterAttackAnimationSvgsUnsafe monsterCell specie heroCell =
    -- AttackAnimation is using coordinates, but we are using the screen positions here.
    let
        monsterAttackAnimation =
            makeMonsterAttackAnimation monsterCell specie heroCell

        damage =
            getDamageForSpecie specie

        damageAnimation =
            makeDamageSvgAnimation heroCell damage monsterAnimationDuration
    in
    [ monsterAttackAnimation, damageAnimation ]


makeMonsterAttackAnimation : Cell -> Specie -> Cell -> Svg Msg
makeMonsterAttackAnimation monsterCell specie heroCell =
    makeAttackAnimationSvg monsterCell heroCell halfMonsterAnimationDuration (renderMonsterCell monsterCell specie Animation)


makeHeroAttackAnimationSvgs : Cell -> Cell -> Int -> List (Svg Msg)
makeHeroAttackAnimationSvgs heroCell attackedCell damage =
    -- AttackAnimation is using coordinates, but we are using the screen positions here.
    let
        heroAttackAnimation =
            makeHeroAttackAnimation heroCell attackedCell

        damageAnimation =
            makeDamageSvgAnimation attackedCell damage heroAttackAnimationDuration
    in
    [ heroAttackAnimation, damageAnimation ]


makeHeroAttackAnimation : Cell -> Cell -> Svg Msg
makeHeroAttackAnimation heroCell attackedCell =
    makeAttackAnimationSvg heroCell attackedCell halfHeroAttackAnimationDuration (renderHeroCell heroCell Animation)


makeAttackAnimationSvg : Cell -> Cell -> Int -> Svg msg -> Svg msg
makeAttackAnimationSvg baseCell attackedCell duration svg =
    animatedG (makeAttackAnimation baseCell attackedCell duration) [] [ svg ]


makeAttackAnimation : Cell -> Cell -> Int -> Animation
makeAttackAnimation startCell attackedCell duration =
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
        [ makeAnimationStep attackCoordinate duration, makeAnimationStep startCoordinate duration ]


makeDamageSvgAnimation : Cell -> Int -> Int -> Svg Msg
makeDamageSvgAnimation cell damage duration =
    animatedG (makeDamageAnimation cell duration) [] [ Svg.text_ (textAttributes "red") [ Svg.text (String.fromInt damage) ] ]


makeDamageAnimation : Cell -> Int -> Animation
makeDamageAnimation startCell duration =
    let
        ( startX, startY ) =
            ( toFloat startCell.gridX, toFloat (startCell.gridY + halfSquareSize) )
    in
    Simple.steps
        { startAt = [ P.x startX, P.y startY ]
        , options = []
        }
        [ Simple.step duration [ P.x startX, P.y startY, P.scale 3 ] ]
