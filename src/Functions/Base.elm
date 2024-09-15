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


addToTheBackOfTheList : a -> List a -> List a
addToTheBackOfTheList a list =
    let
        reversedList =
            List.reverse list

        addedList =
            a :: reversedList
    in
    List.reverse addedList
