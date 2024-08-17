module Functions.Hero.Attack exposing (..)

import Constants.Sounds exposing (heroAttackSound)
import Constants.Times exposing (heroAttackAnimationDuration)
import Functions.Animations.Hero exposing (makeAttackAnimationSvgs)
import Functions.Level exposing (removeHeroFromPlayFieldInLevel)
import Functions.PlayField.Get exposing (tryGetCellFromPlayField)
import Functions.PlayField.KeyHelpers exposing (makePlayFieldDictKeyFromCoordinate)
import Messages exposing (Msg(..))
import Models.Cell exposing (Cell)
import Models.MainModel exposing (MainModel)
import Ports exposing (playMusic)
import Process
import Task
import Types exposing (PlayerInput(..))


handleHeroAttack : MainModel -> Cell -> Int -> ( MainModel, Cmd Msg )
handleHeroAttack model attackedCell damage =
    let
        level =
            model.level

        currentHeroCellResult =
            tryGetCellFromPlayField (makePlayFieldDictKeyFromCoordinate level.heroModel.coordinate) level.playField
    in
    case currentHeroCellResult of
        Ok currentHeroCell ->
            let
                updatedLevel =
                    removeHeroFromPlayFieldInLevel level

                animations =
                    makeAttackAnimationSvgs currentHeroCell attackedCell damage

                finishedLevel =
                    { updatedLevel | currentAnimations = animations }

                animationIsDoneCommand =
                    Process.sleep (toFloat <| heroAttackAnimationDuration) |> Task.perform (always HeroAnimationIsDone)

                playSoundCommand =
                    playMusic heroAttackSound
            in
            ( { model | level = finishedLevel, playerInput = Stopped }
            , Cmd.batch [ animationIsDoneCommand, playSoundCommand ]
            )

        Err error ->
            let
                newError =
                    { method = "handlePressedArrowDirection - " ++ error.method
                    , error = "Failed to make move animation, currentHeroCell is not found in our play field. - " ++ error.error
                    }
            in
            ( { model | error = Just newError }, Cmd.none )
