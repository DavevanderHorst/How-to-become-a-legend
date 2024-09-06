module Functions.Animations.Helpers exposing (..)

import Models.Cell exposing (Coordinate)
import Simple.Animation as Simple exposing (Animation, Step)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


makeAnimationStep : Coordinate -> Int -> Step
makeAnimationStep coordinate duration =
    Simple.step duration [ P.x (toFloat coordinate.columnNumber), P.y (toFloat coordinate.rowNumber) ]


animatedG : Animation -> List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
animatedG =
    animatedSvg Svg.g


animatedSvg =
    Animated.svg
        { class = SvgAttr.class
        }
