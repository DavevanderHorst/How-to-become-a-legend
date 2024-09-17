module Functions.Animations.Attack exposing (..)

import Constants.FieldSizes exposing (halfSquareSize)
import Constants.Monster exposing (getDamageForSpecie)
import Constants.Times exposing (halfHeroAttackAnimationDuration, halfMonsterAnimationDuration, heroAttackAnimationDuration, monsterAnimationDuration)
import Dict exposing (Dict)
import Functions.Animations.Helpers exposing (animatedG, makeAnimationStep)
import Functions.PlayField.Get exposing (tryGetCellFromFieldByCoordinate)
import Functions.ToString exposing (coordinateToString)
import Messages exposing (Msg)
import Models.Cell exposing (Cell, Coordinate)
import Models.MainModel exposing (Error)
import Simple.Animation as Simple exposing (Animation)
import Simple.Animation.Property as P
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttr
import Types exposing (CellContent(..), Specie)
import Views.Attributes exposing (baseCellAttributes)
import Views.MainView exposing (renderHeroCell, renderMonsterCell)


tryMakeMonsterAttackAnimation : Cell -> Specie -> Coordinate -> Dict String Cell -> Result Error (List (Svg Msg))
tryMakeMonsterAttackAnimation monsterCell specie heroSpot playField =
    let
        heroCellResult =
            tryGetCellFromFieldByCoordinate heroSpot playField
    in
    case heroCellResult of
        Err error ->
            Err
                { method = "tryMakeMonsterAttackAnimation - " ++ error.method
                , error = error.error
                }

        Ok heroCell ->
            if heroCell.content /= Hero then
                Err
                    { method = "tryMakeMonsterAttackAnimation"
                    , error = "Hero is not on given coordinate : " ++ coordinateToString heroSpot
                    }

            else
                Ok (makeMonsterAttackAnimationSvgs monsterCell specie heroCell)


makeMonsterAttackAnimationSvgs : Cell -> Specie -> Cell -> List (Svg Msg)
makeMonsterAttackAnimationSvgs monsterCell specie heroCell =
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
    makeAttackAnimationSvg monsterCell heroCell halfMonsterAnimationDuration (renderMonsterCell specie baseCellAttributes)


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
    makeAttackAnimationSvg heroCell attackedCell halfHeroAttackAnimationDuration (renderHeroCell baseCellAttributes)


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
    animatedG (makeDamageAnimation cell duration) [] [ Svg.text_ attackTextAttributes [ Svg.text (String.fromInt damage) ] ]


attackTextAttributes : List (Attribute msg)
attackTextAttributes =
    [ SvgAttr.fill "red"
    , SvgAttr.fontWeight "950"
    , SvgAttr.fontFamily "Helvetica"
    , SvgAttr.stroke "white"
    ]


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
