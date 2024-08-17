module Functions.Level exposing (..)

import Functions.PlayField.Set exposing (removeHeroFromPlayFieldUnsafe, setHeroInPlayFieldUnsafe, setMonstersInPlayFieldUnsafe)
import Models.Level exposing (Level)


setHeroInPlayFieldInLevel : Level -> Level
setHeroInPlayFieldInLevel level =
    -- unsafe, notting will be checked.
    let
        playFieldWithHero =
            setHeroInPlayFieldUnsafe level.heroModel.coordinate level.playField
    in
    { level | playField = playFieldWithHero }


removeHeroFromPlayFieldInLevel : Level -> Level
removeHeroFromPlayFieldInLevel level =
    -- unsafe, notting will be checked.
    let
        playFieldWithoutHero =
            removeHeroFromPlayFieldUnsafe level.heroModel.coordinate level.playField
    in
    { level | playField = playFieldWithoutHero }


setMonstersInPlayFieldInLevel : Level -> Level
setMonstersInPlayFieldInLevel level =
    -- unsafe, notting will be checked.
    let
        playFieldWithMonsters =
            setMonstersInPlayFieldUnsafe level.monsterModels level.playField
    in
    { level | playField = playFieldWithMonsters }
