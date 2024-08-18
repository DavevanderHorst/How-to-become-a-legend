module Levels.TestLevel exposing (..)

import Constants.FieldSizes exposing (backGroundMargin, betweenSquaresSize, totalBackGroundMargin, totalSquareSize)
import Dict exposing (Dict)
import Functions.PathFinding exposing (setPathFindingInPlayField)
import Functions.PlayField.Insert exposing (insertMonstersInMonsterDict, trySetHeroInPlayField, trySetMonstersInPlayField)
import Functions.PlayField.KeyHelpers exposing (makePlayFieldDictKeyFromCoordinate)
import Models.Cell exposing (Cell, Coordinate)
import Models.Hero exposing (HeroModel)
import Models.Level exposing (Level)
import Models.MainModel exposing (Error)
import Models.Monster exposing (MonsterModel)
import Types exposing (CellContent(..), Specie(..))


rows : Int
rows =
    15


columns : Int
columns =
    20


heroStartModel : HeroModel
heroStartModel =
    { coordinate = Coordinate 5 4 }


testLevelDummy : Specie
testLevelDummy =
    Dummy 1


monsterList : List MonsterModel
monsterList =
    --[ monsterOne, monsterTwo ]
    []


monsterOne : MonsterModel
monsterOne =
    { coordinate = Coordinate 8 2, specie = testLevelDummy }


monsterTwo : MonsterModel
monsterTwo =
    { coordinate = Coordinate 3 7, specie = testLevelDummy }


createTestLevel : Result Error Level
createTestLevel =
    let
        basePlayField =
            generateTestPlayField

        playFieldWithHeroResult =
            trySetHeroInPlayField heroStartModel.coordinate basePlayField
    in
    case playFieldWithHeroResult of
        Ok playFieldWithHero ->
            let
                playFieldWithHeroAndMonstersResult =
                    trySetMonstersInPlayField monsterList playFieldWithHero
            in
            case playFieldWithHeroAndMonstersResult of
                Ok playFieldWithHeroAndMonsters ->
                    let
                        finishedPlayField =
                            setPathFindingInPlayField heroStartModel.coordinate playFieldWithHeroAndMonsters

                        playFieldWidth =
                            -- last column and row doesnt need the between squares size
                            (columns * totalSquareSize) + totalBackGroundMargin - betweenSquaresSize

                        playFieldHeight =
                            (rows * totalSquareSize) + totalBackGroundMargin - betweenSquaresSize

                        monsterDict =
                            -- if monsters are successfully set in play field, then everything is oke.
                            insertMonstersInMonsterDict monsterList Dict.empty
                    in
                    Ok (Level playFieldWidth playFieldHeight finishedPlayField heroStartModel monsterDict [])

                Err error ->
                    Err { error | method = "createTestLevel " ++ error.method, error = "Adding monsters to playField failed. " ++ error.error }

        Err error ->
            Err { error | method = "createTestLevel " ++ error.method, error = "Adding hero to playField failed. " ++ error.error }


generateTestPlayField : Dict String Cell
generateTestPlayField =
    List.foldl generatePlayFieldRows Dict.empty (List.range 1 rows)


generatePlayFieldRows : Int -> Dict String Cell -> Dict String Cell
generatePlayFieldRows rowNumber playField =
    List.foldl (generatePlayFieldColumns rowNumber) playField (List.range 1 columns)


generatePlayFieldColumns : Int -> Int -> Dict String Cell -> Dict String Cell
generatePlayFieldColumns rowNumber colNumber playField =
    let
        key =
            makePlayFieldDictKeyFromCoordinate (Coordinate colNumber rowNumber)

        gridX =
            -- total size of a cell * (column number - 1)
            -- + black margin of background - 1x the between for the last square
            (totalSquareSize * (colNumber - 1)) + backGroundMargin

        gridY =
            (totalSquareSize * (rowNumber - 1)) + backGroundMargin

        createdCell : Cell
        createdCell =
            { coordinate = Coordinate colNumber rowNumber
            , gridX = gridX
            , gridY = gridY
            , content = Empty
            , stepsToHero = Nothing
            }
    in
    Dict.insert key createdCell playField
