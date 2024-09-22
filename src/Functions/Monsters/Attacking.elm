module Functions.Monsters.Attacking exposing (..)

import Functions.Animations.Attack exposing (makeMonsterAttackAnimationSvgsUnsafe)
import Functions.Monsters.FinishMonsterTurn exposing (finishMonsterTurn)
import Functions.PlayField.Get exposing (tryGetCellFromFieldByCoordinate)
import Functions.ToString exposing (coordinateToString)
import Messages exposing (Msg)
import Models.Cell exposing (Cell)
import Models.MainModel exposing (MainModel)
import Models.Monster exposing (MonsterModel)
import Types exposing (CellContent(..))


handleMonsterAttackingTurn : MonsterModel -> Cell -> MainModel -> List MonsterModel -> ( MainModel, Cmd Msg )
handleMonsterAttackingTurn monster monsterCell model restOfMonsters =
    -- monster and monster cell are checked, we check everything else here
    let
        heroCoordinate =
            model.level.heroModel.coordinate

        heroCellResult =
            tryGetCellFromFieldByCoordinate heroCoordinate model.level.playField.field
    in
    case heroCellResult of
        Err error ->
            let
                updatedError =
                    { method = "handleMonsterAttackingTurn - " ++ error.method
                    , error = error.error
                    }
            in
            ( { model | error = Just updatedError }, Cmd.none )

        Ok heroCell ->
            if heroCell.content /= Hero then
                let
                    updatedError =
                        { method = "handleMonsterAttackingTurn"
                        , error = "Hero is not on given coordinate : " ++ coordinateToString heroCoordinate
                        }
                in
                ( { model | error = Just updatedError }, Cmd.none )

            else
                -- Everything is oke!
                let
                    animations =
                        makeMonsterAttackAnimationSvgsUnsafe monsterCell monster.specie heroCell
                in
                finishMonsterTurn monster Nothing animations model restOfMonsters
