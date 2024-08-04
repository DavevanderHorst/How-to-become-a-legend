module Functions.Level exposing (..)

import Functions.PlayField.Set exposing (removeHeroFromPlayFieldUnsafe, setHeroInPlayFieldUnsafe)
import Models exposing (Coordinate, Level)


moveHeroToNextCoordinateInLevel : Coordinate -> Level -> Level
moveHeroToNextCoordinateInLevel nextCoordinate level =
    -- unsafe, notting will be checked.
    let
        playFieldWithoutHero =
            removeHeroFromPlayFieldUnsafe level.heroCoordinate level.playField

        playFieldWithHero =
            setHeroInPlayFieldUnsafe nextCoordinate playFieldWithoutHero
    in
    { level | playField = playFieldWithHero, heroCoordinate = nextCoordinate }
