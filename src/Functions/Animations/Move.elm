module Functions.Animations.Move exposing (..)

import Constants.Times exposing (heroMoveAnimationDuration, monsterAnimationDuration)
import Dict exposing (Dict)
import Functions.Animations.Helpers exposing (animatedG, makeAnimationStep)
import Functions.PathFinding exposing (tryFindCoordinateWithOneLowerStepAroundCoordinate)
import Functions.PlayField.Get exposing (tryGetCellFromPlayFieldByCoordinate)
import Messages exposing (Msg)
import Models.Cell exposing (Cell, Coordinate)
import Models.MainModel exposing (Error)
import Models.Monster exposing (MonsterModel)
import Simple.Animation as Simple exposing (Animation)
import Simple.Animation.Property as P
import Svg exposing (Svg)
import Views.Attributes exposing (baseCellAttributes)
import Views.MainView exposing (renderHeroCell, renderMonsterCell)


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


makeHeroMoveAnimationSvg : Cell -> Cell -> Svg Msg
makeHeroMoveAnimationSvg heroCell nextCell =
    makeMoveAnimationSvg (renderHeroCell baseCellAttributes) heroMoveAnimationDuration heroCell nextCell


tryMakeMonsterMoveAnimationAndUpdateModel : MonsterModel -> Dict String Cell -> Result Error ( Svg Msg, MonsterModel )
tryMakeMonsterMoveAnimationAndUpdateModel monster playField =
    let
        moveToCoordinateResult =
            tryFindCoordinateWithOneLowerStepAroundCoordinate monster.coordinate playField
    in
    case moveToCoordinateResult of
        Err error ->
            Err
                { method = "tryMakeMonsterMoveAnimationAndUpdateModel - " ++ error.method
                , error = error.error
                }

        Ok coordinateToMoveTo ->
            let
                monsterCellResult =
                    tryGetCellFromPlayFieldByCoordinate monster.coordinate playField
            in
            case monsterCellResult of
                Err error ->
                    Err
                        { method = "tryMakeMonsterMoveAnimationAndUpdateModel(monster cell) - " ++ error.method
                        , error = error.error
                        }

                Ok monsterCell ->
                    let
                        moveToCellResult =
                            tryGetCellFromPlayFieldByCoordinate coordinateToMoveTo playField
                    in
                    case moveToCellResult of
                        Err error ->
                            Err
                                { method = "tryMakeMonsterMoveAnimationAndUpdateModel(move towards cell) - " ++ error.method
                                , error = error.error
                                }

                        Ok moveToCell ->
                            let
                                updatedMonsterModel =
                                    { monster | coordinate = coordinateToMoveTo }

                                moveAnimation =
                                    makeMoveAnimationSvg (renderMonsterCell monster.specie baseCellAttributes) monsterAnimationDuration monsterCell moveToCell
                            in
                            Ok ( moveAnimation, updatedMonsterModel )
