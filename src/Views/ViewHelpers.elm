module Views.ViewHelpers exposing (..)


makePxStringFromFloat : Float -> String
makePxStringFromFloat floatNumber =
    String.fromFloat floatNumber ++ "px"


makePxStringFromInt : Int -> String
makePxStringFromInt intNumber =
    String.fromInt intNumber ++ "px"
