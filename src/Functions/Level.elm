module Functions.Level exposing (..)

import Functions.PlayField.Set exposing (removeHeroFromPlayFieldUnsafe, setHeroInPlayFieldUnsafe)
import Models exposing (Coordinate, Level)


setHeroInLevel : Level -> Level
setHeroInLevel level =
    -- unsafe, notting will be checked.
    let
        playFieldWithHero =
            setHeroInPlayFieldUnsafe level.heroCoordinate level.playField
    in
    { level | playField = playFieldWithHero }


removeHeroFromLevel : Level -> Level
removeHeroFromLevel level =
    -- unsafe, notting will be checked.
    let
        playFieldWithoutHero =
            removeHeroFromPlayFieldUnsafe level.heroCoordinate level.playField
    in
    { level | playField = playFieldWithoutHero }
