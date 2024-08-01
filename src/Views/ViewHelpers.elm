module Views.ViewHelpers exposing (..)

import Html


attrFloat : (String -> Html.Attribute msg) -> Float -> Html.Attribute msg
attrFloat attr value =
    attr (String.fromFloat value ++ "px")


attrInt : (String -> Html.Attribute msg) -> Int -> Html.Attribute msg
attrInt attr value =
    attr (String.fromInt value ++ "px")
