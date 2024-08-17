module Functions.Monsters.Base exposing (handleMonstersTurn)

import Constants.Times exposing (heroAttackAnimationDuration, monsterAnimationDuration)
import Dict exposing (Dict)
import Functions.Animations.Base exposing (animatedG)
import Functions.PlayField.Get exposing (tryGetCellFromPlayField)
import Functions.PlayField.Set exposing (removeMonstersFromPlayFieldUnSafe)
import MainView exposing (baseCellAttributes, renderMonsterCell)
import Messages exposing (Msg(..))
import Models.Cell exposing (Cell)
import Models.Level exposing (Level)
import Models.MainModel exposing (Error, MainModel)
import Models.Monster exposing (MonsterModel)
import Process
import Simple.Animation as Simple exposing (Animation)
import Simple.Animation.Property as P
import Svg exposing (Svg)
import Task
import Types exposing (CellContent(..), PlayerInput(..))


handleMonstersTurn : MainModel -> ( MainModel, Cmd Msg )
handleMonstersTurn model =
    -- every monster does do something
    -- so all get an animation, which means we have to remove them temporarily
    let
        oldLevel =
            model.level

        animationsResult =
            Dict.foldl (makeMonsterAnimation oldLevel.playField) (Ok []) oldLevel.monsterModels
    in
    case animationsResult of
        -- animations are made correctly, so we can remove them from playfield.
        Ok animations ->
            let
                playFieldWithoutMonsters =
                    removeMonstersFromPlayFieldUnSafe oldLevel.monsterModels oldLevel.playField

                updatedLevel =
                    { oldLevel | currentAnimations = animations, playField = playFieldWithoutMonsters }

                nextCommand =
                    Process.sleep (toFloat <| monsterAnimationDuration) |> Task.perform (always MonsterAnimationsAreDone)
            in
            ( { model | level = updatedLevel }, nextCommand )

        Err err ->
            ( { model | error = Just err }, Cmd.none )


makeMonsterAnimation : Dict String Cell -> String -> MonsterModel -> Result Error (List (Svg Msg)) -> Result Error (List (Svg Msg))
makeMonsterAnimation dict key monster svgListResult =
    case svgListResult of
        Err err ->
            Err err

        Ok svgList ->
            let
                monsterCellResult =
                    tryGetCellFromPlayField key dict
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
                                , error = "Monster coordinate is empty."
                                }

                        Hero ->
                            Err
                                { method = "makeMonsterAnimation - "
                                , error = "Monster coordinate contains a hero"
                                }

                        Monster specie ->
                            if specie /= monster.specie then
                                Err
                                    { method = "makeMonsterAnimation - "
                                    , error = "Monster specie is not the same as cell content."
                                    }

                            else
                                let
                                    damageAnimation =
                                        animatedG (makeGrowAnimation monsterCell) [] [ renderMonsterCell monster.specie baseCellAttributes ]
                                in
                                Ok (damageAnimation :: svgList)


makeGrowAnimation : Cell -> Animation
makeGrowAnimation startCell =
    let
        ( startX, startY ) =
            ( toFloat startCell.gridX, toFloat startCell.gridY )
    in
    Simple.steps
        { startAt = [ P.x startX, P.y startY ]
        , options = []
        }
        [ Simple.step monsterAnimationDuration [ P.x startX, P.y startY, P.scale 2 ] ]
