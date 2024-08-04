module Main exposing (init, keyDecoder, main, subscriptions)

import Browser
import Browser.Dom
import Browser.Events exposing (onResize)
import Functions.Coordinate exposing (getNextCoordinateForDirection)
import Functions.Level exposing (moveHeroToNextCoordinateInLevel)
import Functions.PlayField.Get exposing (tryGetCellFromPlayField)
import Functions.PlayField.KeyHelpers exposing (makeDictKeyFromCoordinate)
import Functions.ToString exposing (coordinateToString)
import Json.Decode as Decode
import Levels.TestLevel exposing (createTestLevel)
import MainView exposing (mainView)
import Messages exposing (Msg(..))
import Models exposing (Direction(..), Level, MainModel, PlayerInput(..), PressedKey(..), Size, emptyLevel, startSize)
import Task


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
      , pressedKey = "Start"
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
            makeDictKeyFromCoordinate nextCoordinate

        nextCellResult =
            tryGetCellFromPlayField nextKey model.level.playField
    in
    case nextCellResult of
        Err _ ->
            -- no cell found in direction
            -- TODO make error sound
            ( { model | playerInput = Possible }, Cmd.none )

        Ok nextCell ->
            -- found a cell, now we check if it possible to move too, or if monster so we now if we move or attack.
            case nextCell.content of
                Models.Empty ->
                    let
                        updatedLevel =
                            moveHeroToNextCoordinateInLevel nextCoordinate model.level
                    in
                    ( { model | level = updatedLevel }, Cmd.none )

                Models.Hero ->
                    let
                        newError =
                            { method = "handlePressedArrowDirection"
                            , error = "Cant move Hero, there is another hero in place on coordinate : " ++ coordinateToString nextCoordinate
                            }
                    in
                    ( { model | error = Just newError }, Cmd.none )
