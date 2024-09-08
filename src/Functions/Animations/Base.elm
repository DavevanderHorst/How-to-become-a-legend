module Functions.Animations.Base exposing (..)

import Dict exposing (Dict)
import Functions.Animations.Attack exposing (tryMakeMonsterAttackAnimation)
import Functions.Animations.Move exposing (tryMakeMonsterMoveAnimationAndUpdateModel)
import Functions.PlayField.Get exposing (tryGetCellFromPlayFieldByCoordinate)
import Functions.ToString exposing (coordinateToString)
import Messages exposing (Msg)
import Models.Cell exposing (Cell, Coordinate)
import Models.MainModel exposing (Error)
import Models.Monster exposing (MonsterModel)
import Svg exposing (Svg)
import Types exposing (Action(..), CellContent(..))


tryMakeMonsterAnimationAndAdjustModel : MonsterModel -> Dict String Cell -> Coordinate -> Result Error ( List (Svg Msg), MonsterModel )
tryMakeMonsterAnimationAndAdjustModel monster playField heroSpot =
    let
        monsterCellResult =
            tryGetCellFromPlayFieldByCoordinate monster.coordinate playField
    in
    case monsterCellResult of
        Err err ->
            Err
                { method = "tryMakeMonsterAnimationsAndAdjustModel - " ++ err.method
                , error = "Monster coordinate is not in our dict. - " ++ err.error
                }

        Ok monsterCell ->
            case monsterCell.content of
                Empty ->
                    Err
                        { method = "tryMakeMonsterAnimationsAndAdjustModel"
                        , error = "Monster coordinate is empty. " ++ coordinateToString monster.coordinate
                        }

                Hero ->
                    Err
                        { method = "tryMakeMonsterAnimationsAndAdjustModel"
                        , error = "Monster coordinate contains a hero"
                        }

                Monster specie action ->
                    if specie /= monster.specie then
                        Err
                            { method = "tryMakeMonsterAnimationsAndAdjustModel"
                            , error = "Monster specie is not the same as cell content."
                            }

                    else if action /= monster.action then
                        Err
                            { method = "tryMakeMonsterAnimationsAndAdjustModel"
                            , error = "Monster action is not the same as cell content."
                            }

                    else
                        case monster.action of
                            Moving ->
                                let
                                    makeMonsterMoveAnimationResult =
                                        tryMakeMonsterMoveAnimationAndUpdateModel monster playField
                                in
                                case makeMonsterMoveAnimationResult of
                                    Err error ->
                                        Err error

                                    Ok ( monsterMoveAnimation, updatedMonster ) ->
                                        Ok ( [ monsterMoveAnimation ], updatedMonster )

                            Attacking ->
                                let
                                    makeMonsterAttackAnimationResult =
                                        tryMakeMonsterAttackAnimation monsterCell specie heroSpot playField
                                in
                                case makeMonsterAttackAnimationResult of
                                    Err error ->
                                        Err error

                                    Ok monsterAttackAnimations ->
                                        Ok ( monsterAttackAnimations, monster )

                Obstacle _ ->
                    Err
                        { method = "tryMakeMonsterAnimationsAndAdjustModel"
                        , error = "Monster coordinate contains an obstacle"
                        }



--
--makeGrowAnimation : Cell -> Animation
--makeGrowAnimation startCell =
--    let
--        ( startX, startY ) =
--            ( toFloat startCell.gridX, toFloat startCell.gridY )
--    in
--    Simple.steps
--        { startAt = [ P.x startX, P.y startY ]
--        , options = []
--        }
--        [ Simple.step 100 [ P.x startX, P.y startY, P.scale 2 ] ]
