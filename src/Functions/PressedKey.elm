module Functions.PressedKey exposing (..)

import Constants.Sounds exposing (bumpInWallSound)
import Constants.Times exposing (moveAnimationDuration)
import Functions.Animations.Hero exposing (makeMoveAnimationSvgs)
import Functions.Coordinate exposing (getNextCoordinateForDirection)
import Functions.Level exposing (removeHeroFromPlayFieldInLevel)
import Functions.PlayField.Get exposing (tryGetCellFromPlayFieldByCoordinate)
import Functions.Random exposing (rollHeroDamage)
import Functions.ToString exposing (coordinateToString)
import Messages exposing (Msg(..))
import Models.MainModel exposing (MainModel)
import Ports exposing (playMusic)
import Process
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
                -- TODO player input must be stopped
                ( { model | playerInput = Possible }
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
            tryGetCellFromPlayFieldByCoordinate nextCoordinate model.level.playField.field
    in
    case nextCellResult of
        Err _ ->
            -- no cell found in direction
            ( { model | playerInput = Possible }
            , playMusic bumpInWallSound
            )

        Ok nextCell ->
            -- found a cell, now we check if it possible to move too, or if monster so we now if we move or attack.
            case nextCell.content of
                Empty ->
                    -- we can move
                    -- set move animation, for this we also need the current hero cell.
                    let
                        level =
                            model.level

                        currentHeroCellResult =
                            tryGetCellFromPlayFieldByCoordinate level.heroModel.coordinate level.playField.field
                    in
                    case currentHeroCellResult of
                        -- we remove hero from play field, and set the new coordinate as hero coordinate
                        -- if move animation is finished, we set hero on this new spot.
                        Ok currentHeroCell ->
                            let
                                updatedLevel =
                                    removeHeroFromPlayFieldInLevel level

                                moveAnimation =
                                    makeMoveAnimationSvgs currentHeroCell nextCell

                                oldHeroModel =
                                    updatedLevel.heroModel

                                updatedHeroModel =
                                    { oldHeroModel | coordinate = nextCell.coordinate }

                                finishedLevel =
                                    { updatedLevel | heroModel = updatedHeroModel, currentAnimations = moveAnimation }

                                nextCommand =
                                    Process.sleep (toFloat <| moveAnimationDuration) |> Task.perform (always HeroAnimationIsDone)
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

                Hero ->
                    let
                        newError =
                            { method = "handlePressedArrowDirection"
                            , error = "Cant move Hero, there is another hero in place on coordinate : " ++ coordinateToString nextCoordinate
                            }
                    in
                    ( { model | error = Just newError }, Cmd.none )

                Monster specie ->
                    -- need to roll damage die then,
                    -- we move into a monster, so attack!!
                    -- make an attack animation
                    -- we need current hero cell again for screen positions
                    -- make a sound
                    -- show damage above monster head
                    ( { model | playerInput = Stopped }, Random.generate (HeroAttacks nextCell) rollHeroDamage )
