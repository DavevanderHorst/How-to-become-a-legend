module Levels.TestLevel exposing (..)

import Constants.FieldSizes exposing (backGroundMargin, betweenSquaresSize, totalBackGroundMargin, totalSquareSize)
import Dict exposing (Dict)
import Functions.PathFinding exposing (setPathFindingInPlayField)
import Functions.PlayField.Insert exposing (insertMonstersInMonsterDict, trySetHeroInPlayField, trySetMonstersAndObstaclesInPlayField)
import Functions.PlayField.KeyHelpers exposing (makePlayFieldDictKeyFromCoordinate)
import Models.Cell exposing (Cell, Coordinate)
import Models.Hero exposing (HeroModel)
import Models.Level exposing (Level, PlayField)
import Models.MainModel exposing (Error)
import Models.Monster exposing (MonsterModel)
import Models.Obstacle exposing (ObstacleModel)
import Types exposing (CellContent(..), ObstacleType(..), Specie(..))


rows : Int
rows =
    15


columns : Int
columns =
    20


heroStartModel : HeroModel
heroStartModel =
    { coordinate = Coordinate 6 5 }


testLevelDummy : Specie
testLevelDummy =
    Dummy 1


monsterList : List MonsterModel
monsterList =
    []


obstacleList : List ObstacleModel
obstacleList =
    [ obstacleOne, obstacleTwo, obstacleThree, obstacleFour, obstacleFive, obstacleSix ]


obstacleOne : ObstacleModel
obstacleOne =
    { coordinate = Coordinate 8 5, obstacleType = Rock }


obstacleTwo : ObstacleModel
obstacleTwo =
    { coordinate = Coordinate 8 6, obstacleType = Rock }


obstacleThree : ObstacleModel
obstacleThree =
    { coordinate = Coordinate 9 5, obstacleType = Rock }


obstacleFour : ObstacleModel
obstacleFour =
    { coordinate = Coordinate 10 5, obstacleType = Rock }


obstacleFive : ObstacleModel
obstacleFive =
    { coordinate = Coordinate 8 7, obstacleType = Rock }


obstacleSix : ObstacleModel
obstacleSix =
    { coordinate = Coordinate 9 7, obstacleType = Rock }


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
                    trySetMonstersAndObstaclesInPlayField monsterList obstacleList playFieldWithHero
            in
            case playFieldWithHeroAndMonstersResult of
                Ok playFieldWithHeroAndMonsters ->
                    let
                        playFieldWidth =
                            -- last column and row doesnt need the between squares size
                            toFloat <| (columns * totalSquareSize) + totalBackGroundMargin - betweenSquaresSize

                        playFieldHeight =
                            toFloat <| (rows * totalSquareSize) + totalBackGroundMargin - betweenSquaresSize

                        createdPlayField =
                            PlayField playFieldWidth playFieldHeight columns rows playFieldWithHeroAndMonsters

                        finishedPlayField =
                            setPathFindingInPlayField heroStartModel.coordinate createdPlayField

                        monsterDict =
                            -- if monsters are successfully set in play field, then everything is oke.
                            insertMonstersInMonsterDict monsterList Dict.empty
                    in
                    Ok (Level finishedPlayField heroStartModel monsterDict [])

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
