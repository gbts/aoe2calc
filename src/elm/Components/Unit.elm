module Components.Unit exposing (..)
import Round exposing (..)
import Components.Data.Dat exposing (Unit, EffectCommand)
import Html exposing (..)
import Html.Attributes exposing (..)
import View.Extra exposing (..)
import Dict

type UnitLayout = UnitLayoutLeft | UnitLayoutRight

getAAClassName: String -> String
getAAClassName id =
    case id of
        "1"  -> "Infantry"
        "2"  -> "Turtle Ships"
        "3"  -> "Base Pierce"
        "4"  -> "Base Melee"
        "5"  -> "War Elephants"
        "6"  -> "Unused"
        "7"  -> "Unused"
        "8"  -> "Cavalry"
        "9"  -> "Unused"
        "10" -> "Unused"
        "11" -> "All Buildings (except Port)"
        "12" -> "Unused"
        "13" -> "Stone Defense"
        "14" -> "FE Predator Animals"
        "15" -> "Archers"
        "16" -> "Ships & Camels & Saboteurs"
        "17" -> "Rams"
        "18" -> "Trees"
        "19" -> "Unique Units (except Turtle Ship)"
        "20" -> "Siege Weapons"
        "21" -> "Standard Buildings"
        "22" -> "Walls & Gates"
        "23" -> "FE Gunpowder Units"
        "24" -> "Boars"
        "25" -> "Monks"
        "26" -> "Castle"
        "27" -> "Spearmen"
        "28" -> "Cavalry Archers"
        "29" -> "Eagle Warriors"
        "30" -> "HD Camels"
        _    -> "???"

getUnitClassName: Int -> String
getUnitClassName class_id =
    case class_id of
            0 -> "Archer"
            2 -> "Trade Boat"
            4 -> "Civilian"
            6 -> "Infantry"
            9 -> "Prey Animal"
            10 -> "Predator Animal"
            12 -> "Cavalry"
            13 -> "Siege Weapon"
            18 -> "Priest"
            19 -> "Trade Cart"
            20 -> "Transport Boat"
            21 -> "Fishing Boat"
            22 -> "Warship"
            23 -> "Conquistador"
            24 -> "War Elephant"
            26 -> "Elephant Archer"
            28 -> "Phalanx"
            29 -> "Domesticated Animal"
            35 -> "Petard"
            36 -> "Cavalry Archer"
            43 -> "Monk with Relic"
            44 -> "Hand Cannoneer"
            45 -> "Two Handed Swordsman"
            46 -> "Pikeman"
            47 -> "Scout Cavalry"
            50 -> "Spearman"
            51 -> "Packed Siege Unit"
            53 -> "Boarding Boat"
            54 -> "Unpacked Siege Unit"
            55 -> "Scorpion"
            56 -> "Raider"
            57 -> "Cavalry Raider"
            59 -> "King"
            61 -> "Horse"
            _ -> "???"

commandAppliesToAttribute: String -> EffectCommand  -> Bool
commandAppliesToAttribute attribute command =
    command.attribute == attribute
    ||
    (command.attribute == "cost" && (List.member attribute ["wood", "food", "gold"]))

attributeView: UnitLayout -> String -> Float -> List EffectCommand -> Html a
attributeView layout attribute base effect_commands =
    let
        relevantEffects =
            effect_commands
            |> List.filter (commandAppliesToAttribute attribute)
        final = List.foldr applyEffect base relevantEffects
        bold_if_not_zero =
            case final /= 0 of
                True -> "bold"
                False -> ""
    in
    if List.isEmpty relevantEffects then
        td [] [
            span [class bold_if_not_zero] [base |> roundFloat |> text]
        ]
    else
        td [] (case layout of
                UnitLayoutLeft ->
                    [
                        List.foldr applyEffectString (base |> roundFloat) relevantEffects ++ " = " |> text
                        , span [class bold_if_not_zero] [ final |> roundFloat |> text]
                    ]
                UnitLayoutRight ->
                    [
                        span [class bold_if_not_zero] [final |> roundFloat |> text]
                        ,  " = " ++ List.foldl applyEffectString (base |> roundFloat) relevantEffects |> text
                    ]
            )


aaView: UnitLayout -> String -> Int -> String -> List EffectCommand -> List (Html a)
aaView layout aa_type aa_value attribute effect_commands =
    let
        relevantEffects =
            effect_commands
            |> List.filter (commandAppliesToAttribute attribute)
            |> List.filter (\c -> (toString c.aa_type) == aa_type)
        final = List.foldr applyEffect (toFloat aa_value) relevantEffects
        bold_if_not_zero =
            case final /= 0 of
                True -> "bold"
                False -> ""
    in
    if List.isEmpty relevantEffects then
        [td [] [text (getAAClassName aa_type)]
            ,td [] [
                span [class bold_if_not_zero] [aa_value |> toFloat |> roundFloat |> text]
            ]
        ]
    else
        [ td [] [text (getAAClassName aa_type)]
        , td [] (case layout of
                UnitLayoutLeft ->
                    [
                        List.foldr applyEffectString (aa_value |> toFloat |> roundFloat) relevantEffects ++ " = " |> text
                        , span [class bold_if_not_zero] [ final |> roundFloat |> text]
                    ]
                UnitLayoutRight ->
                    [
                        span [class bold_if_not_zero] [final |> roundFloat |> text]
                        , " = " ++ List.foldl applyEffectString (aa_value |> toFloat |> roundFloat) relevantEffects |> text
                    ]
            )
        ]

-- Quick workaround for an issue with multiply commands. Large integer values are
-- percentile values while floats are not
-- FIXME: this should be fixed in the import script
correctEffectAmount: EffectCommand -> Float
correctEffectAmount effect_command =
    if effect_command.command_type == 5 && effect_command.amount > 10 then
        effect_command.amount / 100.0
    else
        effect_command.amount

roundFloat: Float -> String
roundFloat value =
    if isNaN value || isInfinite value then
        "N/A"
    else
        if (value |> Basics.truncate |> toFloat) == value then
            -- Don't round values without decimals
            value |> toString
        else
            value |> Round.round 2

applyEffect: EffectCommand -> Float -> Float
applyEffect effect_command base =
    case effect_command.command_type of
    0 -> -- Set
        correctEffectAmount effect_command
    4 -> -- +/-
        base + correctEffectAmount effect_command
    5 -> -- *
        base * correctEffectAmount effect_command
    _ -> 0


applyEffectString: EffectCommand -> String -> String
applyEffectString effect_command base =
    if effect_command.amount == 0 then
        -- Don't show commands with a 0 effect
        base
    else
        let effect_amount = correctEffectAmount effect_command |> roundFloat
        in
        case effect_command.command_type of
        0 -> -- Set
            base ++ " → " ++ effect_amount
        4 -> -- +/-
            base ++ " + " ++ effect_amount
        5 -> -- *
            "(" ++ base ++ ") * " ++ effect_amount
        _ -> ""


aaList: UnitLayout -> String -> Dict.Dict String Int -> List EffectCommand -> Html a
aaList layout attribute aadict effect_commands =
        aadict
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.reverse
        |> List.map (\(k,v) -> tr []
            (case layout of
                UnitLayoutLeft ->
                    (aaView layout k v attribute effect_commands)
                UnitLayoutRight ->
                    List.reverse (aaView layout k v attribute effect_commands)
            )
        )
        |> tbody []

unitStatsRow: UnitLayout -> List (Html a) -> Html a
unitStatsRow layout cells =
    case layout of
        UnitLayoutLeft ->
            tr [] cells
        UnitLayoutRight ->
            tr [] (List.reverse cells)

unitView: UnitLayout -> Unit ->  List EffectCommand -> Maybe Unit -> List EffectCommand -> Html a
unitView layout unit effect_commands projectile proj_effect_commands =
    let layoutClass = case layout of
        UnitLayoutLeft -> "left"
        UnitLayoutRight-> "right"
    in
    div [class "unit", class layoutClass ] [
        img [ src ("static/data/icons/units/" ++ (toString unit.icon_id) ++ "_50730.png"), width 64, height 64 ] []
        , table [class "unit-stats"] ([
            unitStatsRow layout [ td [] [text "Base class"], td [] [text (getUnitClassName unit.class_id)] ]
            ,unitStatsRow layout [ td [] [text "HP"] , attributeView layout "hp" unit.hp effect_commands ]
            ,unitStatsRow layout [ td [] [text "LoS"] , attributeView layout "los" unit.los effect_commands ]
            ,unitStatsRow layout [ td [] [text "Speed"] , attributeView layout "speed" unit.speed effect_commands ]
            ,unitStatsRow layout [ td [] [text "Rate of Fire"] , attributeView layout "reload_time" unit.reload_time effect_commands ]
            ,unitStatsRow layout [ td [] [text "Blast Radius"] , attributeView layout "blast_width" unit.blast_width effect_commands ]
            ,unitStatsRow layout [ td [] [text "Min Range"] , attributeView layout "min_range" unit.min_range effect_commands ]
            ,unitStatsRow layout [ td [] [text "Max Range"] , attributeView layout "max_range" unit.max_range effect_commands ]
            ,unitStatsRow layout [ td [] [text "Accuracy (%)"] , attributeView layout "accuracy" unit.accuracy effect_commands ]
            ,unitStatsRow layout [ td [] [text "# Projectiles"] , attributeView layout "num_projectiles" unit.num_projectiles effect_commands ]

            
            ,tr [class "divider"] [td [colspan 2, style[("text-align", "center")]] [text "Cost"]]

            ,unitStatsRow layout [ td [] [text "Wood"] , attributeView layout "wood" unit.wood effect_commands ]
            ,unitStatsRow layout [ td [] [text "Food"] , attributeView layout "food" unit.food effect_commands ]
            ,unitStatsRow layout [ td [] [text "Gold"] , attributeView layout "gold" unit.gold effect_commands ]

            ,tr [class "divider"] [td [colspan 2, style[("text-align", "center")]] [text "Attacks"]]

            ,aaList layout "attack" unit.attacks effect_commands
            
            ,tr [class "divider"] [td [colspan 2, style[("text-align", "center")]] [text "Armours"]]
            
            ,aaList layout "armor" unit.armours effect_commands
        ] ++ case projectile of
            Just projectile ->
                [
                    tr [class "divider"] [td [colspan 2, style[("text-align", "center")]] [text "Projectile stats"]]
                    ,unitStatsRow layout [ td [] [text "Speed"] , attributeView layout "speed" projectile.speed proj_effect_commands ]
                    ,unitStatsRow layout [ td [] [text "Ballistics enabled"] , attributeView layout "ballistics" (toFloat projectile.ballistics) proj_effect_commands ]
                    ,aaList layout "attack" projectile.attacks proj_effect_commands
                ]
            Nothing -> []
        )

       -- , ul [] (proj_effect_commands |> List.map (\c -> li [] [text c.command_name, text c.attribute, text (toString c.amount)]))
    ]
