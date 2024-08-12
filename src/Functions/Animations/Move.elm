module Functions.Animations.Move exposing (..)

import Models exposing (AnimationType(..), Cell, Coordinate)


makeMoveAnimation : Cell -> Cell -> AnimationType
makeMoveAnimation heroSpot nextSpot =
    -- AnimationMove is using coordinates, but we are using the screen positions here.
    let
        startCoordinate =
            Coordinate heroSpot.gridX heroSpot.gridY

        endCoordinate =
            Coordinate nextSpot.gridX nextSpot.gridY
    in
    AnimationMove startCoordinate endCoordinate
