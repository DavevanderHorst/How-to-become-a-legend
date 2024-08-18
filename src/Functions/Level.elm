module Functions.Level exposing (..)

import Functions.PlayField.Set exposing (removeHeroFromPlayFieldUnsafe, setHeroInPlayFieldUnsafe, setMonstersInPlayFieldUnsafe)
import Models.Level exposing (Level)


setHeroInPlayFieldInLevel : Level -> Level
setHeroInPlayFieldInLevel level =
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


setMonstersInPlayFieldInLevel : Level -> Level
setMonstersInPlayFieldInLevel level =
    -- unsafe, notting will be checked.
    let
        oldPlayField =
            level.playField

        fieldWithMonsters =
            setMonstersInPlayFieldUnsafe level.monsterModels oldPlayField.field

        updatedPlayField =
            { oldPlayField | field = fieldWithMonsters }
    in
    { level | playField = updatedPlayField }
