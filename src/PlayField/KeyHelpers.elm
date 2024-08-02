module PlayField.KeyHelpers exposing (..)


makePlayFieldDictKey : Int -> Int -> String
makePlayFieldDictKey rowNumber colNumber =
    String.fromInt rowNumber ++ "," ++ String.fromInt colNumber
