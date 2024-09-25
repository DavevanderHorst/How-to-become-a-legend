module Levels.TestLevel exposing (..)

import Constants.FieldSizes exposing (betweenSquaresSize, totalSquareSize)
import Dict exposing (Dict)
import Functions.PathFinding exposing (setPathFindingInPlayField)
import Functions.PlayField.Helpers exposing (makeDictKeyFromCoordinate)
import Functions.PlayField.Insert exposing (insertMonstersInMonsterDict, trySetHeroInPlayField, trySetMonstersAndObstaclesInPlayField)
import Models.Cell exposing (Cell, Coordinate)
import Models.Hero exposing (HeroModel)
import Models.Level exposing (Level, PlayField)
import Models.MainModel exposing (Error)
import Models.Monster exposing (MonsterModel)
import Models.Obstacle exposing (ObstacleModel)
import Types exposing (Action(..), CellContent(..), ObstacleType(..), Specie(..))


rows : Int
rows =
    14


columns : Int
columns =
    19


heroStartModel : HeroModel
heroStartModel =
    { coordinate = Coordinate 2 5 }


monsterList : List MonsterModel
monsterList =
    [ { coordinate = Coordinate 2 4
      , specie = Dummy
      , action = Moving
      }
    , { coordinate = Coordinate 1 5
      , specie = Dummy
      , action = Moving
      }
    , { coordinate = Coordinate 2 3
      , specie = Dummy
      , action = Moving
      }
    ]


obstacleList : List ObstacleModel
obstacleList =
    [ { coordinate = Coordinate 8 5, obstacleType = Rock }
    , { coordinate = Coordinate 8 6, obstacleType = Rock }
    , { coordinate = Coordinate 9 5, obstacleType = Rock }
    , { coordinate = Coordinate 10 5, obstacleType = Rock }
    , { coordinate = Coordinate 8 7, obstacleType = Rock }
    , { coordinate = Coordinate 9 7, obstacleType = Rock }
    , { coordinate = Coordinate 11 7, obstacleType = Rock }
    , { coordinate = Coordinate 12 7, obstacleType = Rock }
    , { coordinate = Coordinate 13 7, obstacleType = Rock }
    , { coordinate = Coordinate 10 7, obstacleType = Rock }
    , { coordinate = Coordinate 10 8, obstacleType = Rock }
    , { coordinate = Coordinate 10 9, obstacleType = Rock }
    , { coordinate = Coordinate 10 10, obstacleType = Rock }
    , { coordinate = Coordinate 10 11, obstacleType = Rock }
    , { coordinate = Coordinate 11 10, obstacleType = Rock }
    , { coordinate = Coordinate 13 6, obstacleType = Rock }
    , { coordinate = Coordinate 13 5, obstacleType = Rock }
    , { coordinate = Coordinate 13 4, obstacleType = Rock }
    , { coordinate = Coordinate 13 3, obstacleType = Rock }
    , { coordinate = Coordinate 12 3, obstacleType = Rock }
    , { coordinate = Coordinate 11 3, obstacleType = Rock }
    , { coordinate = Coordinate 10 3, obstacleType = Rock }
    , { coordinate = Coordinate 9 3, obstacleType = Rock }
    , { coordinate = Coordinate 8 3, obstacleType = Rock }
    , { coordinate = Coordinate 11 5, obstacleType = Rock }
    ]


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
                            toFloat <| (columns * totalSquareSize) + betweenSquaresSize

                        playFieldHeight =
                            toFloat <| (rows * totalSquareSize) + betweenSquaresSize

                        createdPlayField =
                            PlayField playFieldWidth playFieldHeight columns rows playFieldWithHeroAndMonsters

                        monsterDict =
                            -- if monsters are successfully set in play field, then everything is oke.
                            insertMonstersInMonsterDict monsterList Dict.empty

                        finishedPlayField =
                            setPathFindingInPlayField heroStartModel.coordinate createdPlayField monsterDict
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
            makeDictKeyFromCoordinate (Coordinate colNumber rowNumber)

        gridX =
            -- total size of a cell * (column number - 1)
            -- betweenSquaresSize - 1x the between for the last square
            (totalSquareSize * (colNumber - 1)) + betweenSquaresSize

        gridY =
            (totalSquareSize * (rowNumber - 1)) + betweenSquaresSize

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
