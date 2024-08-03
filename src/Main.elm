module Main exposing (init, keyDecoder, main, subscriptions)

import Browser
import Browser.Dom
import Browser.Events exposing (onResize)
import Json.Decode as Decode
import Levels.TestLevel exposing (createTestLevel)
import MainView exposing (mainView)
import Messages exposing (Msg(..))
import Models exposing (Level, MainModel, PlayerInput(..), PressedKey(..), Size, emptyLevel, startSize)
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
            -- TODO
            ( model, Cmd.none )


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
                        Just ArrowUp

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
