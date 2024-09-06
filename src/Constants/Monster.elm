module Constants.Monster exposing (..)

import Types exposing (Specie(..))


getDamageForSpecie : Specie -> Int
getDamageForSpecie specie =
    case specie of
        Dummy ->
            1
