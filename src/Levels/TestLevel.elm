module Levels.TestLevel exposing (..)

import Constants.FieldSizes exposing (backGroundMargin, betweenSquaresSize, totalBackGroundMargin, totalSquareSize)
import Dict exposing (Dict)
import Dict.Insert exposing (trySetHeroInMapCellDict)
import Dict.KeyHelpers exposing (makeDictKeyFromCoordinate)
import Models exposing (Cell, CellContent(..), Coordinate, Error, Level)


rows : Int
rows =
    15


columns : Int
columns =
    20


heroStartSpot : Coordinate
heroStartSpot =
    { columnNumber = 5, rowNumber = 4 }


createTestLevel : Result Error Level
createTestLevel =
    let
        basePlayField =
            List.foldl generateLevelRows Dict.empty (List.range 1 rows)

        playFieldWithHeroResult =
            trySetHeroInMapCellDict heroStartSpot basePlayField
    in
    case playFieldWithHeroResult of
        Ok playFieldWithHero ->
            let
                playFieldWidth =
                    -- last column and row doesnt need the between squares size
                    (columns * totalSquareSize) + totalBackGroundMargin - betweenSquaresSize

                playFieldHeight =
                    (rows * totalSquareSize) + totalBackGroundMargin - betweenSquaresSize
            in
            Ok (Level playFieldWithHero heroStartSpot playFieldWidth playFieldHeight)

        Err error ->
            Err { error | error = "Adding hero to playField failed. " ++ error.error }


generateLevelRows : Int -> Dict String Cell -> Dict String Cell
generateLevelRows rowNumber dict =
    List.foldl (generateLevelColumns rowNumber) dict (List.range 1 columns)


generateLevelColumns : Int -> Int -> Dict String Cell -> Dict String Cell
generateLevelColumns rowNumber colNumber dict =
    let
        key =
            makeDictKeyFromCoordinate (Coordinate colNumber rowNumber)

        gridX =
            -- total size of a cell * (column number - 1)
            -- + black margin of background - 1x the between for the last square
            (totalSquareSize * (colNumber - 1)) + backGroundMargin

        gridY =
            (totalSquareSize * (rowNumber - 1)) + backGroundMargin

        createdCell =
            Cell (Coordinate colNumber rowNumber) gridX gridY Empty
    in
    Dict.insert key createdCell dict
