module Functions.PlayField.Insert exposing (insertMonstersInMonsterDict, trySetHeroInPlayField, trySetMonstersAndObstaclesInPlayField)

import Dict exposing (Dict)
import Functions.PlayField.Get exposing (tryGetCellFromPlayFieldByKey)
import Functions.PlayField.KeyHelpers exposing (makePlayFieldDictKeyFromCoordinate)
import Functions.ToString exposing (cellContentToString, obstacleTypeToString, specieToString)
import Models.Cell exposing (Cell, Coordinate)
import Models.MainModel exposing (Error)
import Models.Monster exposing (MonsterModel)
import Models.Obstacle exposing (ObstacleModel)
import Types exposing (CellContent(..), ObstacleType, Specie)


insertMonstersInMonsterDict : List MonsterModel -> Dict String MonsterModel -> Dict String MonsterModel
insertMonstersInMonsterDict monsters dict =
    List.foldl insertMonsterInToMonsterDict dict monsters


insertMonsterInToMonsterDict : MonsterModel -> Dict String MonsterModel -> Dict String MonsterModel
insertMonsterInToMonsterDict monster dict =
    let
        dictKey =
            makePlayFieldDictKeyFromCoordinate monster.coordinate
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
                    makePlayFieldDictKeyFromCoordinate obstacle.coordinate

                getCellResult =
                    tryGetCellFromPlayFieldByKey dictKey playField
            in
            case getCellResult of
                Err err ->
                    Err err

                Ok cell ->
                    if cell.content == Empty then
                        Ok (updateGridCellDict dictKey (setCellContentToObstacle obstacle.obstacleType) playField)

                    else
                        Err
                            { method = "Functions.Dict.Insert.trySetObstaclesInPlayField"
                            , error =
                                "Cant set obstacle("
                                    ++ obstacleTypeToString obstacle.obstacleType
                                    ++ ") in play field, cell is not empty : "
                                    ++ cellContentToString cell.content
                            }


trySetMonsterInPlayField : MonsterModel -> Result Error (Dict String Cell) -> Result Error (Dict String Cell)
trySetMonsterInPlayField monster playFieldResult =
    case playFieldResult of
        Err err ->
            Err err

        Ok playField ->
            let
                dictKey =
                    makePlayFieldDictKeyFromCoordinate monster.coordinate

                getCellResult =
                    tryGetCellFromPlayFieldByKey dictKey playField
            in
            case getCellResult of
                Err err ->
                    Err err

                Ok cell ->
                    if cell.content == Empty then
                        Ok (updateGridCellDict dictKey (setCellContentToMonster monster.specie) playField)

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
        dictKey =
            makePlayFieldDictKeyFromCoordinate coordinate

        getCellResult =
            tryGetCellFromPlayFieldByKey dictKey playField
    in
    case getCellResult of
        Err err ->
            Err err

        Ok cell ->
            if cell.content == Empty then
                Ok (updateGridCellDict dictKey setCellContentToHero playField)

            else
                Err
                    { method = "Functions.Dict.Insert.trySetHeroInPlayField"
                    , error = "Cant set hero in play field, cell is not empty : " ++ cellContentToString cell.content
                    }


setCellContentToHero : Maybe Cell -> Maybe Cell
setCellContentToHero =
    setCellContent Hero


setCellContentToMonster : Specie -> Maybe Cell -> Maybe Cell
setCellContentToMonster specie =
    setCellContent (Monster specie)


setCellContentToObstacle : ObstacleType -> Maybe Cell -> Maybe Cell
setCellContentToObstacle obstacleType =
    setCellContent (Obstacle obstacleType)


setCellContent : CellContent -> Maybe Cell -> Maybe Cell
setCellContent newContent =
    Maybe.map
        (\old -> { old | content = newContent })


updateGridCellDict : String -> (Maybe Cell -> Maybe Cell) -> Dict String Cell -> Dict String Cell
updateGridCellDict key function gridCellDict =
    -- Unsafe, only use when your 100% sure cellState is Empty
    Dict.update key function gridCellDict
