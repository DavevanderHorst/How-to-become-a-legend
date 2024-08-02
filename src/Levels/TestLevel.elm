module Levels.TestLevel exposing (..)

import Constants.FieldSizes exposing (backGroundMargin, betweenSquaresSize, totalBackGroundMargin, totalSquareSize)
import Dict exposing (Dict)
import Models exposing (Cell, Coordinate, Level)
import PlayField.KeyHelpers exposing (makePlayFieldDictKey)


rows : Int
rows =
    15


columns : Int
columns =
    20


createTestLevel : Level
createTestLevel =
    let
        playField =
            List.foldl generateLevelRows Dict.empty (List.range 1 rows)

        playFieldWidth =
            -- last column and row doesnt need the between squares size
            (columns * totalSquareSize) + totalBackGroundMargin - betweenSquaresSize

        playFieldHeight =
            (rows * totalSquareSize) + totalBackGroundMargin - betweenSquaresSize
    in
    Level playField playFieldWidth playFieldHeight


generateLevelRows : Int -> Dict String Cell -> Dict String Cell
generateLevelRows rowNumber dict =
    List.foldl (generateLevelColumns rowNumber) dict (List.range 1 columns)


generateLevelColumns : Int -> Int -> Dict String Cell -> Dict String Cell
generateLevelColumns rowNumber colNumber dict =
    let
        key =
            makePlayFieldDictKey rowNumber colNumber

        gridX =
            -- total size of a cell * (column number - 1)
            -- + black margin of background - 1x the between for the last square
            (totalSquareSize * (colNumber - 1)) + backGroundMargin

        gridY =
            (totalSquareSize * (rowNumber - 1)) + backGroundMargin

        createdCell =
            Cell (Coordinate rowNumber colNumber) gridX gridY
    in
    Dict.insert key createdCell dict
