module Functions.Level exposing (..)

import Functions.PlayField.Set exposing (removeHeroFromPlayFieldUnsafe, setHeroInPlayFieldUnsafe, trySetMonsterInPlayField)
import Models.Level exposing (Level)
import Models.MainModel exposing (Error)
import Models.Monster exposing (MonsterModel)


setHeroBackInPlayFieldInLevel : Level -> Level
setHeroBackInPlayFieldInLevel level =
    -- unsafe, notting will be checked.
    let
        oldPlayField =
            level.playField

        fieldWithHero =
            setHeroInPlayFieldUnsafe level.heroModel.coordinate oldPlayField.field

        updatedPlayField =
            { oldPlayField | field = fieldWithHero }
    in
    { level | playField = updatedPlayField }


removeHeroFromPlayFieldInLevel : Level -> Level
removeHeroFromPlayFieldInLevel level =
    -- unsafe, notting will be checked.
    let
        oldPlayField =
            level.playField

        fieldWithoutHero =
            removeHeroFromPlayFieldUnsafe level.heroModel.coordinate oldPlayField.field

        updatedPlayField =
            { oldPlayField | field = fieldWithoutHero }
    in
    { level | playField = updatedPlayField }


trySetMonsterInPlayFieldInLevel : MonsterModel -> Level -> Result Error Level
trySetMonsterInPlayFieldInLevel monster level =
    let
        oldPlayField =
            level.playField

        fieldWithMonsterResult =
            trySetMonsterInPlayField monster oldPlayField.field
    in
    case fieldWithMonsterResult of
        Ok fieldWithMonster ->
            let
                updatedPlayField =
                    { oldPlayField | field = fieldWithMonster }
            in
            Ok { level | playField = updatedPlayField }

        Err err ->
            Err { method = "setMonstersInPlayFieldInLevel - " ++ err.method, error = err.error }
