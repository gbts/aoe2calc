module Components.Data.Dat exposing (..)

import Dict

import Json.Decode exposing (int, string, bool, float, dict, list, map2, field, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)

type alias Dat = {
     meta: Meta,
     techs: Dict.Dict String Tech,
     civs: Dict.Dict String Civ,
     units: Dict.Dict String Unit,
     effects: Dict.Dict String Effect
}

datDecoder : Decoder Dat
datDecoder =
    decode Dat
    |> required "meta" metaDecoder
    |> required "techs" (dict techDecoder)
    |> required "civs" (dict civDecoder)
    |> required "units" (dict unitDecoder)
    |> required "effects" (dict effectDecoder)

type alias Tech = {
    id: Int,
    name: String,
    required_techs: List Int,
    required_by_techs: List Int,
    effect_id: Int,
    civ: Int,
    icon_id: Int,
    research_location: Int
}

techDecoder : Decoder Tech
techDecoder =
    decode Tech
    |> required "id" int
    |> required "name" string
    |> required "required_techs" (list int)
    |> required "required_by_techs" (list int)
    |> required "effect_id" int
    |> required "civ" int
    |> required "icon_id" int
    |> required "research_location" int

type alias Meta = {
    unit_classes: Dict.Dict String String,
    aa_classes: Dict.Dict String String
}
metaDecoder : Decoder Meta
metaDecoder =
    decode Meta
    |> required "unit_classes" (dict string)
    |> required "aa_classes" (dict string)

type alias Civ = {
    id: Int,
    name: String,
    civ_bonuses: List Int,
    team_bonus_effect_id: Int,
    tech_tree_id: Int,
    tech_ids: List Int, 
    unit_ids: List Int,
    unique_techs: CivUniqueTechs
}
type alias CivUniqueTechs = {castle: Int, imp: Int }

civDecoder : Decoder Civ
civDecoder =
    decode Civ
    |> required "id" int
    |> required "name" string
    |> required "civ_bonuses" (list int)
    |> optional "team_bonus_effect_id" int -1
    |> optional "tech_tree_id" int -1
    |> required "tech_ids" (list int)
    |> required "unit_ids" (list int)
    |> required "unique_techs" (map2 CivUniqueTechs (field "castle" int) (field "imp" int))


type alias Unit = {
    id: Int,
    name: String,
    class_id: Int,
    icon_id: Int,
    projectile_id: Int,
    ballistics: Int,
    hp: Float,
    los: Float,
    max_range: Float,
    min_range: Float,
    accuracy: Float,
    num_projectiles: Float,
    blast_width: Float,
    reload_time: Float,
    speed: Float,
    wood: Float,
    food: Float,
    gold: Float,
    attacks: Dict.Dict String Int,
    armours: Dict.Dict String Int
}

unitDecoder : Decoder Unit
unitDecoder =
    decode Unit
    |> required "id" int
    |> required "name" string
    |> required "class_id" int
    |> required "icon_id" int
    |> required "projectile_id" int
    |> required "ballistics" int
    |> required "hp" float
    |> required "los" float
    |> required "max_range" float
    |> required "min_range" float
    |> required "accuracy_percent" float
    |> required "total_projectiles" float
    |> required "blast_width" float
    |> required "reload_time" float
    |> required "speed" float
    |> required "wood" float
    |> required "food" float
    |> required "gold" float
    |> required "attacks" (dict int)
    |> required "armours" (dict int)

type alias Effect = {
    id: Int,
    name: String,
    commands: List EffectCommand
}

effectDecoder: Decoder Effect
effectDecoder =
    decode Effect
    |> required "id" int
    |> required "name" string
    |> required "commands" (list effectCommandDecoder)

type alias EffectCommand = {
    command_name: String,
    raw: EffectCommandRaw,
    amount: Float,
    unit_id: Int,
    class_id: Int,
    command_type: Int,
    aa_type: Int,
    attribute: String
}

effectCommandDecoder: Decoder EffectCommand
effectCommandDecoder =
    decode EffectCommand
    |> required "command_name" string
    |> required "raw" effectCommandRawDecoder
    |> optional "amount" float -1
    |> optional "unit_id" int -1
    |> optional "class_id" int -1
    |> optional "command_type" int -1
    |> optional "aa_type" int -1
    |> optional "attribute" string ""

type alias EffectCommandRaw = {
    a: Int, b: Int, c: Int, d: Float, type_: Int
}

effectCommandRawDecoder: Decoder EffectCommandRaw 
effectCommandRawDecoder =
    decode EffectCommandRaw
    |> required "A" int
    |> required "B" int
    |> required "C" int
    |> required "D" float
    |> required "type" int