module Functions.Hero.Attack exposing (..)

import Constants.Sounds exposing (heroAttackSound)
import Constants.Times exposing (heroAttackAnimationDuration)
import Functions.Animations.Attack exposing (makeHeroAttackAnimationSvgs)
import Functions.Monsters.Base exposing (doDamageToMonster)
import Functions.Monsters.MonsterDict exposing (setMonsterInDictByCoordinateUnsafe, tryGetMonsterFromMonsterDictByCoordinate)
import Functions.PlayField.Get exposing (tryGetCellFromFieldByKey)
import Functions.PlayField.Helpers exposing (makeDictKeyFromCoordinate)
import Messages exposing (Msg(..))
import Models.Cell exposing (Cell)
import Models.MainModel exposing (MainModel)
import Ports exposing (playMusic)
import Process
import Task
import Types exposing (PlayerInput(..))


handleHeroAttack : MainModel -> Cell -> Int -> ( MainModel, Cmd Msg )
handleHeroAttack model attackedCell damage =
    -- hero and pathfinding are removed from field
    let
        level =
            model.level

        currentHeroCellResult =
            tryGetCellFromFieldByKey (makeDictKeyFromCoordinate level.heroModel.coordinate) level.playField.field
    in
    case currentHeroCellResult of
        Ok currentHeroCell ->
            let
                getAttackedMonsterResult =
                    tryGetMonsterFromMonsterDictByCoordinate attackedCell.coordinate model.level.monsterDict
            in
            case getAttackedMonsterResult of
                Err error ->
                    let
                        newError =
                            { method = "handleHeroAttack - " ++ error.method
                            , error = "Failed to get attacked monster. - " ++ error.error
                            }
                    in
                    ( { model | error = Just newError }, Cmd.none )

                Ok attackedMonster ->
                    let
                        updatedAttackedMonster =
                            doDamageToMonster attackedMonster damage

                        updatedMonsterDict =
                            -- coordinate is same, so we just overwrite our old monster
                            setMonsterInDictByCoordinateUnsafe updatedAttackedMonster level.monsterDict

                        animations =
                            makeHeroAttackAnimationSvgs currentHeroCell attackedCell damage

                        finishedLevel =
                            { level | animations = animations, monsterDict = updatedMonsterDict }

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
