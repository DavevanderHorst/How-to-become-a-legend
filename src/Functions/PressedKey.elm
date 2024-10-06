module Functions.PressedKey exposing (..)

import Constants.Sounds exposing (bumpInWallSound)
import Functions.Coordinate exposing (getNextCoordinateForDirection)
import Functions.Hero.Movement exposing (handleHeroMovement)
import Functions.Level exposing (removeHeroFromPlayFieldInLevel)
import Functions.PlayField.Get exposing (tryGetCellFromFieldByCoordinate)
import Functions.PlayField.Set exposing (removeStepsFromPlayField)
import Functions.Random exposing (rollHeroDamage)
import Functions.ToString exposing (coordinateToString)
import Messages exposing (Msg(..))
import Models.MainModel exposing (MainModel)
import Ports exposing (playMusic)
import Random
import Task
import Types exposing (CellContent(..), Direction(..), PlayerInput(..), PressedKey(..))


handleKeyPressed : String -> MainModel -> ( MainModel, Cmd Msg )
handleKeyPressed key model =
    if model.playerInput == Possible then
        let
            maybePressedKey =
                case key of
                    "ArrowUp" ->
                        Just (Arrow Up)

                    "ArrowDown" ->
                        Just (Arrow Down)

                    "ArrowLeft" ->
                        Just (Arrow Left)

                    "ArrowRight" ->
                        Just (Arrow Right)

                    _ ->
                        Nothing
        in
        case maybePressedKey of
            Nothing ->
                ( model, Cmd.none )

            Just pressedKey ->
                ( { model | playerInput = Stopped }
                , Task.perform (\_ -> HandleKeyPressed pressedKey) (Task.succeed True)
                )

    else
        ( model, Cmd.none )


handlePressedKey : PressedKey -> MainModel -> ( MainModel, Cmd Msg )
handlePressedKey pressedKey model =
    case pressedKey of
        Arrow direction ->
            handlePressedArrowDirection direction model


handlePressedArrowDirection : Direction -> MainModel -> ( MainModel, Cmd Msg )
handlePressedArrowDirection direction model =
    let
        nextCoordinate =
            getNextCoordinateForDirection direction model.level.heroModel.coordinate

        nextCellResult =
            tryGetCellFromFieldByCoordinate nextCoordinate model.level.playField.field
    in
    case nextCellResult of
        Err _ ->
            -- no cell found in direction
            ( { model | playerInput = Possible }
            , playMusic bumpInWallSound
            )

        Ok nextCell ->
            -- found a cell, now we check if it possible to move too, or if monster so we now if we move or attack.
            -- we remove hero and steps from playField
            let
                oldLevel =
                    model.level

                playFieldWithRemovedPathFinding =
                    removeStepsFromPlayField oldLevel.playField

                updatedLevel =
                    removeHeroFromPlayFieldInLevel { oldLevel | playField = playFieldWithRemovedPathFinding }

                updatedModel =
                    { model | level = updatedLevel }
            in
            case nextCell.content of
                Empty ->
                    handleHeroMovement nextCell updatedModel

                Hero ->
                    let
                        newError =
                            { method = "handlePressedArrowDirection"
                            , error = "Cant move Hero, there is another hero in place on coordinate : " ++ coordinateToString nextCoordinate
                            }
                    in
                    ( { model | error = Just newError }, Cmd.none )

                Monster ->
                    -- need to roll damage die then,
                    -- we move into a monster, so attack!!
                    -- make an attack animation
                    -- we need current hero cell again for screen positions
                    -- make a sound
                    -- show damage above monster head
                    ( { model | playerInput = Stopped, level = updatedLevel }, Random.generate (HeroAttacks nextCell) rollHeroDamage )

                Obstacle _ ->
                    -- cant move here, its blocked, make bump sound.
                    ( { model | playerInput = Possible }
                    , playMusic bumpInWallSound
                    )
