module Functions.Animations.Move exposing (..)

import Constants.Times exposing (heroMoveAnimationDuration, monsterAnimationDuration)
import Dict exposing (Dict)
import Functions.Animations.Base exposing (animatedG, makeAnimationStep)
import Functions.PathFinding exposing (tryFindCoordinateWithOneLowerStepAroundCoordinate)
import Functions.PlayField.Get exposing (tryGetCellFromPlayFieldByCoordinate, tryGetCellFromPlayFieldByKey)
import Functions.PlayField.KeyHelpers exposing (makePlayFieldDictKeyFromCoordinate)
import Functions.ToString exposing (coordinateToString)
import Messages exposing (Msg)
import Models.Cell exposing (Cell, Coordinate)
import Models.MainModel exposing (Error)
import Models.Monster exposing (MonsterModel)
import Simple.Animation as Simple exposing (Animation)
import Simple.Animation.Property as P
import Svg exposing (Svg)
import Types exposing (Action(..), CellContent(..), Specie(..))
import Views.Attributes exposing (baseCellAttributes)
import Views.MainView exposing (renderHeroCell, renderMonsterCell)


tryMakeMoveAnimationSvg : Cell -> Cell -> Result Error (Svg Msg)
tryMakeMoveAnimationSvg startCell nextCell =
    -- MoveAnimation is using coordinates, but we are using the screen positions here in the coordinate.
    let
        startCoordinate =
            Coordinate startCell.gridX startCell.gridY

        endCoordinate =
            Coordinate nextCell.gridX nextCell.gridY

        renderedSvgWithDurationResult : Result Error ( Svg msg, Int )
        renderedSvgWithDurationResult =
            case startCell.content of
                Empty ->
                    Err { method = "makeMoveAnimationSvg", error = "Cant make an animation from an empty cell." }

                Hero ->
                    Ok ( renderHeroCell baseCellAttributes, heroMoveAnimationDuration )

                Monster specie ->
                    Ok ( renderMonsterCell specie baseCellAttributes, monsterAnimationDuration )

                Obstacle _ ->
                    Err { method = "makeMoveAnimationSvg", error = "Obstacles dont move." }
    in
    case renderedSvgWithDurationResult of
        Err error ->
            Err error

        Ok ( renderedSvg, duration ) ->
            Ok <| animatedG (makeMoveAnimation startCoordinate endCoordinate duration) [] [ renderedSvg ]


makeMoveAnimation : Coordinate -> Coordinate -> Int -> Animation
makeMoveAnimation start end duration =
    Simple.steps
        { startAt = [ P.x (toFloat start.columnNumber), P.y (toFloat start.rowNumber) ]
        , options = []
        }
        [ makeAnimationStep end duration ]


tryMakeMonsterAnimationsAndAdjustModels : Dict String Cell -> Dict String MonsterModel -> Result Error ( List (Svg Msg), Dict String MonsterModel )
tryMakeMonsterAnimationsAndAdjustModels playField monsterDict =
    Dict.foldl (makeMonsterAnimation playField) (Ok ( [], Dict.empty )) monsterDict


makeMonsterAnimation : Dict String Cell -> String -> MonsterModel -> Result Error ( List (Svg Msg), Dict String MonsterModel ) -> Result Error ( List (Svg Msg), Dict String MonsterModel )
makeMonsterAnimation playField key monster animationsAndMonstersResult =
    case animationsAndMonstersResult of
        Err err ->
            Err err

        Ok ( animationList, newMonsterDict ) ->
            let
                monsterCellResult =
                    tryGetCellFromPlayFieldByKey key playField
            in
            case monsterCellResult of
                Err err ->
                    Err
                        { method = "makeMonsterAnimation - " ++ err.method
                        , error = "Monster coordinate is not in our dict. - " ++ err.error
                        }

                Ok monsterCell ->
                    case monsterCell.content of
                        Empty ->
                            Err
                                { method = "makeMonsterAnimation"
                                , error = "Monster coordinate is empty. " ++ key
                                }

                        Hero ->
                            Err
                                { method = "makeMonsterAnimation"
                                , error = "Monster coordinate contains a hero"
                                }

                        Monster specie ->
                            if specie /= monster.specie then
                                Err
                                    { method = "makeMonsterAnimation"
                                    , error = "Monster specie is not the same as cell content."
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
                                                let
                                                    updatedMonsterDict =
                                                        Dict.insert (makePlayFieldDictKeyFromCoordinate updatedMonster.coordinate) updatedMonster newMonsterDict
                                                in
                                                Ok ( monsterMoveAnimation :: animationList, updatedMonsterDict )

                                    Attacking ->
                                        let
                                            updatedMonsterDict =
                                                Dict.insert key monster newMonsterDict
                                        in
                                        -- TODO make attack animation
                                        Ok ( animationList, updatedMonsterDict )

                        Obstacle _ ->
                            Err
                                { method = "makeMonsterAnimation"
                                , error = "Monster coordinate contains an obstacle"
                                }


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

                                makeMoveAnimationSvgResult =
                                    tryMakeMoveAnimationSvg monsterCell moveToCell
                            in
                            case makeMoveAnimationSvgResult of
                                Err error ->
                                    Err
                                        { method = "tryMakeMonsterMoveAnimationAndUpdateModel(make move animation) - " ++ error.method
                                        , error = error.error
                                        }

                                Ok moveAnimation ->
                                    Ok ( moveAnimation, updatedMonsterModel )
