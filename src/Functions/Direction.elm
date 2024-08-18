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
