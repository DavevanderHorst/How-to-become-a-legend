module Main exposing (init, keyDecoder, main, subscriptions)

import Browser
import Browser.Dom
import Browser.Events exposing (onResize)
import Json.Decode as Decode
import Levels.TestLevel exposing (createTestLevel)
import MainView exposing (mainView)
import Messages exposing (Msg(..))
import Models exposing (MainModel, PlayerInput(..), Size, startSize)
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
    ( { windowSize = startSize
      , error = Nothing
      , playerInput = Stopped
      , level = createTestLevel
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
        ( model, Cmd.none )

    else
        ( model, Cmd.none )
