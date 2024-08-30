module Functions.Base exposing (..)


differenceBetweenNumbers : Int -> Int -> Int
differenceBetweenNumbers numberOne numberTwo =
    let
        biggestNumber =
            max numberOne numberTwo

        smallestNumber =
            min numberOne numberTwo
    in
    biggestNumber - smallestNumber
