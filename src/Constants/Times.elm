module Constants.Times exposing (..)

-- 1 second = 1000


heroMoveAnimationDuration : Int
heroMoveAnimationDuration =
    200


waitTimeBetweenAnimations : Float
waitTimeBetweenAnimations =
    50


heroAttackAnimationDuration : Int
heroAttackAnimationDuration =
    halfHeroAttackAnimationDuration * 2


halfHeroAttackAnimationDuration : Int
halfHeroAttackAnimationDuration =
    250


monsterAnimationDuration : Int
monsterAnimationDuration =
    halfMonsterAnimationDuration * 2


halfMonsterAnimationDuration : Int
halfMonsterAnimationDuration =
    200
