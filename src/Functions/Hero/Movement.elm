module Functions.Hero.Movement exposing (..)

import Constants.Times exposing (heroMoveAnimationDuration)
import Functions.Animations.Move exposing (makeHeroMoveAnimationSvg)
import Functions.PlayField.Get exposing (tryGetCellFromFieldByCoordinate)
import Messages exposing (Msg(..))
import Models.Cell exposing (Cell)
import Models.MainModel exposing (MainModel)
import Process
import Task
import Types exposing (CellContent(..), PlayerInput(..))


handleHeroMovement : Cell -> MainModel -> ( MainModel, Cmd Msg )
handleHeroMovement nextCell model =
    -- we can move
    -- set move animation, for this we also need the current hero cell.
    let
        currentLevel =
            model.level

        currentHeroCellResult =
            tryGetCellFromFieldByCoordinate currentLevel.heroModel.coordinate currentLevel.playField.field
    in
    case currentHeroCellResult of
        -- hero and steps are already removed from play field
        -- if move animation is finished, we set hero on this new spot.
        Ok currentHeroCell ->
            let
                moveAnimation =
                    makeHeroMoveAnimationSvg currentHeroCell nextCell

                oldHeroModel =
                    currentLevel.heroModel

                newHeroCoordinate =
                    nextCell.coordinate

                updatedHeroModel =
                    { oldHeroModel | coordinate = newHeroCoordinate }

                finishedLevel =
                    { currentLevel
                        | heroModel = updatedHeroModel
                        , animations = [ moveAnimation ]
                    }

                nextCommand =
                    Process.sleep (toFloat <| heroMoveAnimationDuration) |> Task.perform (always HeroAnimationIsDone)
            in
            ( { model | level = finishedLevel, playerInput = Stopped }, nextCommand )

        Err error ->
            let
                newError =
                    { method = "handlePressedArrowDirection - " ++ error.method
                    , error = "Failed to make move animation, currentHeroCell is not found in our play field. - " ++ error.error
                    }
            in
            ( { model | error = Just newError }, Cmd.none )
