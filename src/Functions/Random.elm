module Functions.Random exposing (..)

import Random


rollHeroDamage : Random.Generator Int
rollHeroDamage =
    Random.int 1 10
