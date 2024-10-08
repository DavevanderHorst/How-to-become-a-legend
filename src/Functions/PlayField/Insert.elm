module Functions.PlayField.Insert exposing (..)

import Dict exposing (Dict)
import Functions.PlayField.Get exposing (tryGetCellFromFieldByCoordinate, tryGetCellFromFieldByKey)
import Functions.PlayField.Helpers exposing (makeDictKeyFromCoordinate, setCellContentToHero, setCellContentToMonster, setCellContentToObstacle, updateGridCellDictByCoordinateUnsafe, updateGridCellDictByKeyUnsafe)
import Functions.ToString exposing (cellContentToString, obstacleTypeToString, specieToString)
import Models.Cell exposing (Cell, Coordinate)
import Models.MainModel exposing (Error)
import Models.Monster exposing (MonsterModel)
import Models.Obstacle exposing (ObstacleModel)
import Types exposing (Action(..), CellContent(..), ObstacleType, Specie)


insertMonstersInMonsterDict : List MonsterModel -> Dict String MonsterModel -> Dict String MonsterModel
insertMonstersInMonsterDict monsters dict =
    List.foldl insertMonsterInToMonsterDict dict monsters


insertMonsterInToMonsterDict : MonsterModel -> Dict String MonsterModel -> Dict String MonsterModel
insertMonsterInToMonsterDict monster dict =
    let
        dictKey =
            makeDictKeyFromCoordinate monster.coordinate
    in
    insertIntoDict monster dictKey dict


insertIntoDict : a -> String -> Dict String a -> Dict String a
insertIntoDict a key dict =
    Dict.insert key a dict


trySetMonstersAndObstaclesInPlayField : List MonsterModel -> List ObstacleModel -> Dict String Cell -> Result Error (Dict String Cell)
trySetMonstersAndObstaclesInPlayField monsters obstacles playField =
    let
        monstersSetResult =
            List.foldl trySetMonsterInPlayField (Ok playField) monsters
    in
    case monstersSetResult of
        Err err ->
            Err err

        Ok monstersSet ->
            List.foldl trySetObstaclesInPlayField (Ok monstersSet) obstacles


trySetObstaclesInPlayField : ObstacleModel -> Result Error (Dict String Cell) -> Result Error (Dict String Cell)
trySetObstaclesInPlayField obstacle playFieldResult =
    case playFieldResult of
        Err err ->
            Err err

        Ok playField ->
            let
                dictKey =
                    makeDictKeyFromCoordinate obstacle.coordinate

                getCellResult =
                    tryGetCellFromFieldByKey dictKey playField
            in
            case getCellResult of
                Err err ->
                    Err err

                Ok cell ->
                    if cell.content == Empty then
                        Ok (updateGridCellDictByKeyUnsafe dictKey (setCellContentToObstacle obstacle.obstacleType) playField)

                    else
                        Err
                            { method = "Functions.Dict.Insert.trySetObstaclesInPlayField"
                            , error =
                                "Cant set obstacle("
                                    ++ obstacleTypeToString obstacle.obstacleType
                                    ++ ") in play field, cell is not empty : "
                                    ++ cellContentToString cell.content
                            }


updateMonsterInPlayFieldUnsafe : MonsterModel -> Dict String Cell -> Dict String Cell
updateMonsterInPlayFieldUnsafe monster playField =
    updateGridCellDictByCoordinateUnsafe monster.coordinate setCellContentToMonster playField


trySetMonsterInPlayField : MonsterModel -> Result Error (Dict String Cell) -> Result Error (Dict String Cell)
trySetMonsterInPlayField monster playFieldResult =
    case playFieldResult of
        Err err ->
            Err err

        Ok playField ->
            let
                dictKey =
                    makeDictKeyFromCoordinate monster.coordinate

                getCellResult =
                    tryGetCellFromFieldByKey dictKey playField
            in
            case getCellResult of
                Err err ->
                    Err err

                Ok cell ->
                    if cell.content == Empty then
                        Ok (updateGridCellDictByKeyUnsafe dictKey setCellContentToMonster playField)

                    else
                        Err
                            { method = "Functions.Dict.Insert.trySetMonsterInPlayField"
                            , error =
                                "Cant set monster("
                                    ++ specieToString monster.specie
                                    ++ ") in play field, cell is not empty : "
                                    ++ cellContentToString cell.content
                            }


trySetHeroInPlayField : Coordinate -> Dict String Cell -> Result Error (Dict String Cell)
trySetHeroInPlayField coordinate playField =
    let
        getCellResult =
            tryGetCellFromFieldByCoordinate coordinate playField
    in
    case getCellResult of
        Err err ->
            Err err

        Ok cell ->
            if cell.content == Empty then
                Ok (updateGridCellDictByCoordinateUnsafe coordinate setCellContentToHero playField)

            else
                Err
                    { method = "Functions.Dict.Insert.trySetHeroInPlayField"
                    , error = "Cant set hero in play field, cell is not empty : " ++ cellContentToString cell.content
                    }
