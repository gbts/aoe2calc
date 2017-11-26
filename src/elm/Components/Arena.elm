module Components.Arena exposing (..)
import Components.Unit exposing (applyEffect, commandAppliesToAttribute, roundFloat)
import Html exposing (..)
import Html.Attributes exposing (..)
import Dict

import Model exposing (..)
import Components.Data.Dat exposing (..)

getAttrForUnit: (Unit -> Float) -> String -> Unit -> List EffectCommand -> Float
getAttrForUnit attrGetter attrName unit effect_commands =
    let base = attrGetter unit
        relevantEffects =
            effect_commands
            |> List.filter (commandAppliesToAttribute attrName)
    in
    List.foldl applyEffect base relevantEffects

getAttrForProjectile: (Unit -> Float) -> String -> Maybe Unit -> List EffectCommand -> Float
getAttrForProjectile attrGetter attrName projectile effect_commands =
    case projectile of
        Just projectile ->
            getAttrForUnit attrGetter attrName projectile effect_commands
        Nothing ->
            0

getAAForUnit: Unit -> String -> String -> Int -> List EffectCommand -> Int
getAAForUnit unit attr aa_type aa_value effect_commands =
    let
        relevantEffects =
            effect_commands
            |> List.filter (commandAppliesToAttribute attr)
            |> List.filter (\c -> (toString c.aa_type) == aa_type)
    in
    List.foldr applyEffect (toFloat aa_value) relevantEffects |> truncate


attacksForUnit: Unit -> List EffectCommand -> Dict.Dict String Int
attacksForUnit unit effect_commands =
        unit.attacks
        |> Dict.map (\k v -> getAAForUnit unit "attack" k v effect_commands)
 
attacksForProjectile: Maybe Unit -> List EffectCommand -> Dict.Dict String Int
attacksForProjectile projectile effect_commands =
    case projectile of
        Just projectile ->
            attacksForUnit projectile effect_commands
        Nothing ->
            Dict.empty
 
armoursForUnit: Unit -> List EffectCommand -> Dict.Dict String Int
armoursForUnit unit effect_commands =
        unit.armours
        |> Dict.map (\k v -> getAAForUnit unit "armor" k v effect_commands)

calcAttack: Dict.Dict String Int -> Dict.Dict String Int -> Int
calcAttack attacks armours =
    let damage = Dict.merge (\k v d -> d) aaMatch (\k v d -> d) attacks armours Dict.empty
                 |> Dict.foldl (\k v b -> b + v) 0
    in
    if damage <= 0 && not (Dict.intersect attacks armours |> Dict.isEmpty) then
        -- Minimum damage 
        1
    else
        -- Real damage or 0 if no match
        damage

aaMatch: String -> Int -> Int -> Dict.Dict String Int -> Dict.Dict String Int
aaMatch key attack armour dict =
    let damage = Basics.max 0 (attack - armour)
    in
    Dict.insert key damage dict

valueForUnit: Unit -> List EffectCommand -> Float
valueForUnit unit effect_commands =
    let wood = getAttrForUnit .wood "wood" unit effect_commands
        food = getAttrForUnit .food "food" unit effect_commands
        gold = getAttrForUnit .gold "gold" unit effect_commands
    in
        (wood * 0.30) + (food * 0.60) + gold


arenaView: Unit -> List EffectCommand -> Maybe Unit -> List EffectCommand -> Unit -> List EffectCommand -> Maybe Unit -> List EffectCommand -> Html a
arenaView unit1 effects1 projectile1 proj_effects1 unit2 effects2 projectile2 proj_effects2 =
    let attacks1 = attacksForUnit unit1 effects1
        armours1 = armoursForUnit unit1 effects1
        proj_attacks1 = attacksForProjectile projectile1 proj_effects1

        attacks2 = attacksForUnit unit2 effects2
        armours2 = armoursForUnit unit2 effects2
        proj_attacks2 = attacksForProjectile projectile2 proj_effects2

        leftAttack = toFloat (calcAttack attacks1 armours2)
        rightAttack = toFloat (calcAttack attacks2 armours1)

        leftShotsToKill = Basics.ceiling (rightHP / (leftAttack + leftSecondaryAttack))
        rightShotsToKill = Basics.ceiling (leftHP / (rightAttack + rightSecondaryAttack))


        leftProjectiles = getAttrForUnit .num_projectiles "num_projectiles" unit1 effects1
        rightProjectiles = getAttrForUnit .num_projectiles "num_projectiles" unit2 effects2

        leftSecondaryAttack = toFloat ((calcAttack proj_attacks1 armours2) * (truncate leftProjectiles - 1))
        rightSecondaryAttack = toFloat ((calcAttack proj_attacks2 armours1) * (truncate rightProjectiles - 1))

        leftAccuracy = (getAttrForUnit .accuracy "accuracy" unit1 effects1) / 100.0
        rightAccuracy = (getAttrForUnit .accuracy "accuracy" unit2 effects2) / 100.0

        leftHP = getAttrForUnit .hp "hp" unit1 effects1
        rightHP = getAttrForUnit .hp "hp" unit2 effects2

        -- This is incorrect. The extra projectiles deal their own damage
        --leftDPS = (leftAccuracy^leftProjectiles) * ((toFloat leftAttack)*leftProjectiles) / (getAttrForUnit .reload_time "reload_time" unit1 effects1)
        --rightDPS = (rightAccuracy^rightProjectiles) * ((toFloat rightAttack)*rightProjectiles) / (getAttrForUnit .reload_time "reload_time" unit2 effects2)

        leftROF = (getAttrForUnit .reload_time "reload_time" unit1 effects1)
        rightROF = (getAttrForUnit .reload_time "reload_time" unit2 effects2)

        leftDPS = (leftAttack + leftSecondaryAttack) / leftROF 
        rightDPS = (rightAttack + rightSecondaryAttack) / rightROF

        leftDPSAcc = (leftAttack + leftSecondaryAttack) * leftAccuracy / leftROF
        rightDPSAcc = (rightAttack + rightSecondaryAttack) * rightAccuracy / rightROF

        -- Let's say the timer starts immediately after the first attack
        leftSecsToKill = Basics.floor ((rightHP - (leftAttack + leftSecondaryAttack) * leftAccuracy) / leftDPSAcc) |> Basics.max 0
        rightSecsToKill = Basics.floor ((leftHP - (rightAttack + rightSecondaryAttack) * rightAccuracy) / rightDPSAcc) |> Basics.max 0

        leftRange = getAttrForUnit .max_range "max_range" unit1 effects1
        rightRange = getAttrForUnit .max_range "max_range" unit2 effects2

        leftSpeed = getAttrForUnit .speed "speed" unit1 effects1
        rightSpeed = getAttrForUnit .speed "speed" unit2 effects2

        rangeLeftDPS = leftDPS + leftDPS * (leftRange/leftSpeed)
        rangeRightDPS = rightDPS + rightDPS * (rightRange/rightSpeed)

        leftValue = valueForUnit unit1 effects1
        rightValue = valueForUnit unit2 effects2

        leftValueHP = leftValue/leftHP
        rightValueHP = rightValue/rightHP

        dpsTradeLeft = (leftDPSAcc / rightHP) / (rightDPSAcc / leftHP)
        dpsTradeRight = (rightDPSAcc / leftHP) / (leftDPSAcc / rightHP)

        tradeLeft = (leftDPSAcc * (rightValueHP)) / (rightDPSAcc * (leftValueHP))
        tradeRight = (rightDPSAcc * (leftValueHP)) / (leftDPSAcc * (rightValueHP))

    in
    div [] [
        table [class "arena-stats", style [("text-align", "center"), ("width", "100%")]] (
            [
                tr [] [ td [] [leftAttack |> toString |> text], td [] [text "Attack Damage"], td [] [rightAttack |> toString |> text] ]
            ]
            ++
            (
                if leftSecondaryAttack /= 0 || rightSecondaryAttack /= 0 then
                [
                    tr [] [ td [] [leftSecondaryAttack |> toString |> text], td [] [text "Secondary Projectile Damage"], td [] [rightSecondaryAttack |> toString |> text] ]
                    ,tr [] [ td [] [leftAttack + leftSecondaryAttack |> toString |> text], td [] [text "Total Damage"], td [] [rightAttack + rightSecondaryAttack |> toString |> text] ]
                ]
                else []
            )
            ++
            [
                tr [] [ td [] [leftShotsToKill |> toString |> text], td [] [text "Kills other in X hits"], td [] [rightShotsToKill |> toString |> text] ]
                ,tr [class "divider"] [ td [] [] ]
                ,tr [] [ td [] [leftDPS |> roundFloat |> text], td [] [text "DPS" ], td [] [rightDPS |> roundFloat |> text] ]
                ,tr [] [ td [] [leftDPSAcc |> roundFloat |> text], td [] [text "DPS*Accuracy" ], td [] [rightDPSAcc |> roundFloat |> text] ]
                ,tr [] [ td [] [leftSecsToKill |> toString |> text], td [] [text "Kills other in X seconds (avg)"], td [] [rightSecsToKill |> toString |> text] ]
                ,tr [] [  td [] [dpsTradeLeft |> roundFloat |> text], td [] [text "Strength ratio (DPS/HP, higher=better)"], td [] [dpsTradeRight |> roundFloat |> text] ]
                ,tr [class "divider"] [ td [] [] ]
                ,tr [] [ td [] [leftValue |> roundFloat |> text], td [] [text "Cost (W=30,F=60)"], td [] [rightValue |> roundFloat |> text] ]
                ,tr [] [  td [] [leftValueHP |> roundFloat |> text], td [] [text "Cost/HP"], td [] [rightValueHP |> roundFloat |> text] ]
                ,tr [] [  td [] [leftValue/rightValue |> roundFloat |> text], td [] [text "Cost ratio"], td [] [rightValue/leftValue |> roundFloat |> text] ]
                ,tr [] [  td [] [tradeLeft |> roundFloat |> text], td [] [text "Trade ratio (cost vs. strength, higher=better)"], td [] [tradeRight |> roundFloat |> text] ]
            ]
        )
    ]
