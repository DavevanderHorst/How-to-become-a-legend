module Main exposing (init, keyDecoder, main, subscriptions)

import Browser
import Browser.Dom
import Browser.Events exposing (onResize)
import Constants.Times exposing (waitTimeBetweenAnimations)
import Dict
import Functions.Hero.Attack exposing (handleHeroAttack)
import Functions.Level exposing (setHeroBackInPlayFieldInLevel, trySetMonsterInPlayFieldInLevel)
import Functions.Monsters.Base exposing (handleMonsterTurn, handleMonstersTurn)
import Functions.PressedKey exposing (handleKeyPressed, handlePressedKey)
import Json.Decode as Decode
import Levels.TestLevel exposing (createTestLevel)
import Messages exposing (Msg(..))
import Models.Level exposing (emptyLevel)
import Models.MainModel exposing (MainModel, Size, startSize)
import Process
import Task
import Types exposing (CellContent(..), Direction(..), PlayerInput(..), PressedKey(..))
import Views.MainView exposing (mainView)


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

        HeroAnimationIsDone ->
            -- hero is removed for the animation, so needs to be set back.
            -- remove made animations
            -- after hero is done its monsters turn now.
            let
                updatedLevel =
                    setHeroBackInPlayFieldInLevel model.level

                finishedLevel =
                    { updatedLevel | animations = [] }
            in
            ( { model | level = finishedLevel }, Process.sleep waitTimeBetweenAnimations |> Task.perform (always MonstersTurn) )

        MonsterAnimationIsDone monster restOfMonstersForAnimation ->
            -- monster is removed from play field for it's animation, so we need to set it back
            -- remove made animations
            -- we enable player input again.
            let
                levelWithSetMonsterResult =
                    trySetMonsterInPlayFieldInLevel monster model.level
            in
            case levelWithSetMonsterResult of
                Ok levelWithSetMonster ->
                    let
                        finishedLevel =
                            { levelWithSetMonster | animations = [] }

                        updatedModel =
                            { model | level = finishedLevel }
                    in
                    if List.isEmpty restOfMonstersForAnimation then
                        ( { updatedModel | playerInput = Possible }, Cmd.none )

                    else
                        handleMonsterTurn restOfMonstersForAnimation updatedModel

                Err err ->
                    ( { model | error = Just err }, Cmd.none )

        HeroAttacks attackedCell damage ->
            handleHeroAttack model attackedCell damage

        MonstersTurn ->
            -- Check if there are any monsters
            if Dict.isEmpty model.level.monsterModels then
                ( { model | playerInput = Possible }, Cmd.none )

            else
                handleMonstersTurn model


handleScreenSize : Float -> Float -> MainModel -> ( MainModel, Cmd Msg )
handleScreenSize width height model =
    let
        newSize =
            Size width height
    in
    ( { model | windowSize = newSize }, Cmd.none )
