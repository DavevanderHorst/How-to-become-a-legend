module Functions.Monsters.MonsterDict exposing (..)

import Dict exposing (Dict)
import Functions.PlayField.Helpers exposing (makeDictKeyFromCoordinate)
import Models.Cell exposing (Coordinate)
import Models.MainModel exposing (Error)
import Models.Monster exposing (MonsterModel)


addMonsterToMonsterDictUnsafe : MonsterModel -> Dict String MonsterModel -> Dict String MonsterModel
addMonsterToMonsterDictUnsafe monster monsterDict =
    let
        dictKey =
            makeDictKeyFromCoordinate monster.coordinate
    in
    Dict.insert dictKey monster monsterDict


tryGetMonsterFromMonsterDictByCoordinate : Coordinate -> Dict String MonsterModel -> Result Error MonsterModel
tryGetMonsterFromMonsterDictByCoordinate coordinate monsterDict =
    let
        dictKey =
            makeDictKeyFromCoordinate coordinate
    in
    tryGetMonsterFromMonsterDictByKey dictKey monsterDict


tryGetMonsterFromMonsterDictByKey : String -> Dict String MonsterModel -> Result Error MonsterModel
tryGetMonsterFromMonsterDictByKey key monsterDict =
    let
        maybeMonster =
            Dict.get key monsterDict
    in
    case maybeMonster of
        Just monster ->
            Ok monster

        Nothing ->
            Err
                { method = "getMonsterFromMonsterDictByCoordinate"
                , error = "Coordinate is not in our monster dict : " ++ key
                }


removeMonsterFromDictByCoordinateUnsafe : Coordinate -> Dict String MonsterModel -> Dict String MonsterModel
removeMonsterFromDictByCoordinateUnsafe coordinate monsterDict =
    let
        dictKey =
            makeDictKeyFromCoordinate coordinate
    in
    Dict.remove dictKey monsterDict


setMonsterInDictByCoordinateUnsafe : MonsterModel -> Dict String MonsterModel -> Dict String MonsterModel
setMonsterInDictByCoordinateUnsafe monster monsterDict =
    let
        dictKey =
            makeDictKeyFromCoordinate monster.coordinate
    in
    Dict.insert dictKey monster monsterDict
