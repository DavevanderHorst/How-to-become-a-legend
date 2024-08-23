module Functions.Direction exposing (..)

import Types exposing (Direction(..))


findNextDirectionForGoingAroundUnSafe : Direction -> Direction
findNextDirectionForGoingAroundUnSafe direction =
    case direction of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up

        _ ->
            direction


getOppositeDirection : Direction -> Direction
getOppositeDirection direction =
    case direction of
        Up ->
            Down

        UpRight ->
            DownLeft

        Right ->
            Left

        DownRight ->
            UpLeft

        Down ->
            Up

        DownLeft ->
            UpRight

        Left ->
            Right

        UpLeft ->
            DownRight
