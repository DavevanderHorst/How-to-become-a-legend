module Functions.PlayField.Insert exposing (trySetHeroInPlayField, trySetMonstersInPlayField)

import Dict exposing (Dict)
import Functions.PlayField.Get exposing (tryGetCellFromPlayField)
import Functions.PlayField.KeyHelpers exposing (makePlayFieldDictKeyFromCoordinate)
import Functions.ToString exposing (cellContentToString, specieToString)
import Models exposing (Cell, CellContent(..), Coordinate, Error, MonsterModel, Specie)


trySetMonstersInPlayField : List MonsterModel -> Dict String Cell -> Result Error (Dict String Cell)
trySetMonstersInPlayField monsterCoordinates playField =
    List.foldl trySetMonsterInPlayField (Ok playField) monsterCoordinates


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
                    tryGetCellFromPlayField dictKey playField
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
            tryGetCellFromPlayField dictKey playField
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


setCellContent : CellContent -> Maybe Cell -> Maybe Cell
setCellContent newContent =
    Maybe.map
        (\old -> { old | content = newContent })


updateGridCellDict : String -> (Maybe Cell -> Maybe Cell) -> Dict String Cell -> Dict String Cell
updateGridCellDict key function gridCellDict =
    -- Unsafe, only use when your 100% sure cellState is Empty
    Dict.update key function gridCellDict
