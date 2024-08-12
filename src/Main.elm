port module Main exposing (init, keyDecoder, main, subscriptions)

import Browser
import Browser.Dom
import Browser.Events exposing (onResize)
import Constants.Sounds exposing (bumpInWallSound)
import Constants.Times exposing (moveAnimationDuration)
import Functions.Animations.Move exposing (makeMoveAnimation)
import Functions.Coordinate exposing (getNextCoordinateForDirection)
import Functions.Level exposing (removeHeroFromLevel, setHeroInLevel)
import Functions.PlayField.Get exposing (tryGetCellFromPlayField)
import Functions.PlayField.KeyHelpers exposing (makePlayFieldDictKeyFromCoordinate)
import Functions.ToString exposing (coordinateToString)
import Json.Decode as Decode
import Levels.TestLevel exposing (createTestLevel)
import MainView exposing (mainView)
import Messages exposing (Msg(..))
import Models exposing (AnimationType(..), CellContent(..), Direction(..), Level, MainModel, PlayerInput(..), PressedKey(..), Size, emptyLevel, startSize)
import Process
import Task


port playMusic : String -> Cmd msg


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = mainView
        }


subscriptions : MainModel -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , onResize (\w h -> GotNewSize w h)
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map KeyPressed (Decode.field "key" Decode.string)


init : () -> ( MainModel, Cmd Msg )
init _ =
    let
        ( error, level ) =
            case createTestLevel of
                Ok testLevel ->
                    ( Nothing, testLevel )

                Err err ->
                    ( Just err, emptyLevel )
    in
    ( { windowSize = startSize
      , error = error
      , playerInput = Possible
      , level = level
      }
    , Task.perform GotViewport Browser.Dom.getViewport
    )


update : Msg -> MainModel -> ( MainModel, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewPort ->
            handleScreenSize viewPort.viewport.width viewPort.viewport.height model

        GotNewSize width height ->
            handleScreenSize (toFloat width) (toFloat height) model

        KeyPressed key ->
            handleKeyPressed key model

        HandleKeyPressed pressedKey ->
            handlePressedKey pressedKey model

        MoveAnimationIsDone ->
            let
                updatedLevel =
                    setHeroInLevel model.level

                finishedLevel =
                    { updatedLevel | currentAnimation = NoAnimation }
            in
            ( { model | level = finishedLevel, playerInput = Possible }, Cmd.none )


handleScreenSize : Float -> Float -> MainModel -> ( MainModel, Cmd Msg )
handleScreenSize width height model =
    let
        newSize =
            Size width height
    in
    ( { model | windowSize = newSize }, Cmd.none )


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
            getNextCoordinateForDirection direction model.level.heroCoordinate

        nextKey =
            makePlayFieldDictKeyFromCoordinate nextCoordinate

        nextCellResult =
            tryGetCellFromPlayField nextKey model.level.playField
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
                            tryGetCellFromPlayField (makePlayFieldDictKeyFromCoordinate level.heroCoordinate) level.playField
                    in
                    case currentHeroCellResult of
                        -- we remove hero from play field, and set the new coordinate as hero coordinate
                        -- if move animation is finished, we set hero on this new spot.
                        Ok currentHeroCell ->
                            let
                                updatedLevel =
                                    removeHeroFromLevel level

                                moveAnimation =
                                    makeMoveAnimation currentHeroCell nextCell

                                finishedLevel =
                                    { updatedLevel | heroCoordinate = nextCell.coordinate, currentAnimation = moveAnimation }

                                nextCommand =
                                    Process.sleep (toFloat <| moveAnimationDuration) |> Task.perform (always MoveAnimationIsDone)
                            in
                            ( { model | level = finishedLevel, playerInput = Stopped }, nextCommand )

                        Err error ->
                            let
                                newError =
                                    { method = "TODO"
                                    , error = "Failed to make move animation, currentHeroCell is not found in our play field."
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
                    -- we move into a monster, so attack!!
                    -- TODO
                    let
                        newError =
                            { method = "TODO"
                            , error = "Moved into a monster"
                            }
                    in
                    ( { model | error = Just newError }, Cmd.none )
