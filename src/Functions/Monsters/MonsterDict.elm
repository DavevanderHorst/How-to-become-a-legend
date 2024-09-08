module Functions.Monsters.MonsterDict exposing (..)

import Dict exposing (Dict)
import Functions.PlayField.KeyHelpers exposing (makeDictKeyFromCoordinate)
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

        maybeMonster =
            Dict.get dictKey monsterDict
    in
    case maybeMonster of
        Just monster ->
            Ok monster

        Nothing ->
            Err
                { method = "getMonsterFromMonsterDictByCoordinate"
                , error = "Coordinate is not in our monster dict : " ++ dictKey
                }
