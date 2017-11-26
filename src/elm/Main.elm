module Main exposing (..)
-- component import example

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
--import Debug exposing (..)
import Dict
import Set exposing (..)
import View.Extra exposing (..)

import Utils exposing (onChange)
import Components.Data.Dat exposing (..)
import Components.Unit exposing (..)
import Components.Arena exposing (..)
import Model exposing (..)


ages: List Int
ages = [101, 102, 103]

loom: List Int
loom = [22]

blacksmith1: List Int
blacksmith1 = [67, 68, 75]
blacksmith2: List Int
blacksmith2 = [74, 76, 77]
blacksmith3: List Int
blacksmith3 = [81, 82, 80]
blacksmith4: List Int
blacksmith4 = [199, 200, 201]
blacksmith5: List Int
blacksmith5 = [211, 212, 219]

archeryRange: List Int
archeryRange = [437, 436]

stable: List Int
stable = [435, 39]

barracks: List Int
barracks = [215, 90]

dock: List Int
dock = [374, 375, 373]

monastery: List Int
monastery = [230, 231, 252]

university: List Int
university = [377, 93, 47]

allTechs: List Int
allTechs = [22, 101, 102, 103, 67, 68, 75, 74, 76, 77, 81, 82, 80, 199, 200, 201, 211, 212, 219, 437, 436, 435, 39, 215, 90, 374, 375, 373, 230, 231, 252, 377, 93, 47]


getUniqueTechs: Model -> Int -> List Int
getUniqueTechs model civ_id =
    case model.dat of
        Just dat ->
           [
             (dat.techs
                |> Dict.filter (\k v -> v.civ == civ_id)
                |> Dict.filter (\k v -> v.icon_id == 33)
                |> Dict.values
                |> List.map (\t -> t.id)
                |> List.head |> Maybe.withDefault -1)
           ,(dat.techs
                |> Dict.filter (\k v -> v.civ == civ_id)
                |> Dict.filter (\k v -> v.icon_id == 107)
                |> Dict.values
                |> List.map (\t -> t.id)
                |> List.head |> Maybe.withDefault -1)
           ]
        Nothing ->
            []

-- MODEL
init : (Model, Cmd Msg)
init =
    (
        {
            dat = Nothing,
            firstRun = True,
            leftPanel = {
                selectedCiv = Nothing,
                selectedCivID = "",
                selectedUnit = Nothing,
                selectedUnitID = "",
                activeTechs = Set.empty,
                civTechs = Set.empty
            },
            rightPanel = {
                selectedCiv = Nothing,
                selectedCivID = "",
                selectedUnit = Nothing,
                selectedUnitID = "",
                activeTechs = Set.empty,
                civTechs = Set.empty
            }
        },
        fetchDat "static/data/data.aoc.json"
    )


-- UPDATE


type Msg
    = NoOp
    | SelectDat String
    | SelectCiv PanelPosition String
    | SelectUnit PanelPosition String
    | ToggleTech PanelPosition String
    | EnableTech PanelPosition String
    | EnableAllTechs PanelPosition
    | DisableAllTechs PanelPosition
    | DisableTech PanelPosition String
    | Refresh
    | NewDat (Result Http.Error Dat)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)

        SelectDat datFile ->
            (model, fetchDat datFile)

        SelectCiv panelPosition civID ->
            case model.dat of
                Just dat ->
                    case (Dict.get civID dat.civs) of
                        Just civ ->
                            -- Recurse with SelectUnit (allows comparing units across civs)
                            let civTechs = Set.fromList civ.tech_ids |> Set.map toString
                            in
                            case panelPosition of
                                LeftPanel ->
                                    let leftPanel = model.leftPanel
                                        newLeftPanel = {
                                            leftPanel | 
                                                activeTechs =
                                                    Set.intersect leftPanel.activeTechs civTechs
                                                    |> applyCivBonuses model civID,
                                                selectedCivID = civID,
                                                selectedCiv = Just civ,
                                                civTechs = civTechs
                                         }
                                    in
                                    update (SelectUnit panelPosition model.leftPanel.selectedUnitID) { model | leftPanel = newLeftPanel }
                                RightPanel ->
                                    let rightPanel = model.rightPanel
                                        newRightPanel = {
                                            rightPanel | 
                                                activeTechs =
                                                    Set.intersect rightPanel.activeTechs civTechs
                                                    |> applyCivBonuses model civID,
                                                selectedCivID = civID,
                                                selectedCiv = Just civ,
                                                civTechs = civTechs
                                         }
                                    in
                                    update (SelectUnit panelPosition model.rightPanel.selectedUnitID) { model | rightPanel = newRightPanel }

                        Nothing ->
                            case panelPosition of
                                LeftPanel ->
                                    let leftPanel = model.leftPanel
                                        newLeftPanel = { leftPanel |
                                            selectedCivID = "",
                                            selectedCiv = Nothing,
                                            civTechs = Set.empty 
                                        }
                                    -- This can happen when switching from FE to AoC
                                    -- and a new civ was selected
                                    in
                                    update (SelectUnit panelPosition "") { model | leftPanel = newLeftPanel }

                                RightPanel ->
                                    let rightPanel = model.rightPanel
                                        newRightPanel = { rightPanel |
                                            selectedCivID = "",
                                            selectedCiv = Nothing,
                                            civTechs = Set.empty 
                                        }
                                    -- This can happen when switching from FE to AoC
                                    -- and a new civ was selected
                                    in
                                    update (SelectUnit panelPosition "") { model | rightPanel = newRightPanel }
                Nothing ->
                    (model, Cmd.none)

        SelectUnit panelPosition unitID ->
            case model.dat of
                Just dat ->
                    case (Dict.get unitID dat.units) of
                        Just unit ->
                            case panelPosition of
                                LeftPanel ->
                                    let leftPanel = model.leftPanel
                                        newLeftPanel = { leftPanel |
                                            selectedUnitID = unitID,
                                            selectedUnit = Just unit
                                        }
                                    in 
                                    ({ model | leftPanel = newLeftPanel }, Cmd.none)
                                RightPanel ->
                                    let rightPanel = model.rightPanel
                                        newRightPanel = { rightPanel |
                                            selectedUnitID = unitID,
                                            selectedUnit = Just unit
                                        }
                                    in 
                                    ({ model | rightPanel = newRightPanel }, Cmd.none)
                        Nothing ->
                            -- This can happen when switching from FE to AoC
                            -- and a new unit was selected
                            case panelPosition of
                                LeftPanel ->
                                    let leftPanel = model.leftPanel
                                        newLeftPanel = { leftPanel |
                                            selectedUnitID = "",
                                            selectedUnit = Nothing
                                        }
                                    in
                                    ({ model | leftPanel = newLeftPanel}, Cmd.none)
                                RightPanel ->
                                    let rightPanel = model.rightPanel
                                        newRightPanel = { rightPanel |
                                            selectedUnitID = "",
                                            selectedUnit = Nothing
                                        }
                                    in
                                    ({ model | rightPanel = newRightPanel}, Cmd.none)
                            
                Nothing ->
                    (model, Cmd.none)

        ToggleTech panelPosition techID ->
            let activeTechs =
                    case panelPosition of
                        LeftPanel -> model.leftPanel.activeTechs
                        RightPanel -> model.rightPanel.activeTechs
                civTechs =
                    case panelPosition of
                        LeftPanel -> model.leftPanel.civTechs
                        RightPanel -> model.rightPanel.civTechs
            in
            if Set.member techID activeTechs then
                update (DisableTech panelPosition techID) model
            else if Set.member techID civTechs then
                update (EnableTech panelPosition techID) model
            else
                (model, Cmd.none)
        
        EnableTech panelPosition techID ->
            case model.dat of
                Just dat ->
                    case (Dict.get techID dat.techs) of
                        Just tech ->
                            case panelPosition of
                                LeftPanel ->
                                    case model.leftPanel.selectedCiv of
                                        Just civ ->
                                            let leftPanel = model.leftPanel
                                                newLeftPanel = { leftPanel | activeTechs =
                                                    getRequiredTechs dat.techs techID
                                                    |> Set.fromList
                                                    |> applyCivBonuses model (toString civ.id)
                                                    |> Set.union model.leftPanel.activeTechs
                                                }
                                            in
                                            ({ model | leftPanel = newLeftPanel }, Cmd.none)
                                        Nothing ->
                                            (model, Cmd.none)
                                RightPanel ->
                                    case model.rightPanel.selectedCiv of
                                        Just civ ->
                                            let rightPanel = model.rightPanel
                                                newRightPanel = { rightPanel | activeTechs =
                                                    getRequiredTechs dat.techs techID
                                                    |> Set.fromList
                                                    |> applyCivBonuses model (toString civ.id)
                                                    |> Set.union model.rightPanel.activeTechs
                                                }
                                            in
                                            ({ model | rightPanel = newRightPanel }, Cmd.none)
                                        Nothing ->
                                            (model, Cmd.none)
                        Nothing ->
                            (model, Cmd.none)
                Nothing ->
                    (model, Cmd.none)
        
        EnableAllTechs panelPosition ->
            let recurse = \civTechs techs ->
                if not (List.isEmpty techs) then
                    let techID = (techs |> List.head |> Maybe.withDefault -1 |> toString)
                    in
                        if Set.member techID civTechs then 
                            update
                                (EnableTech panelPosition techID)
                                (techs |> List.tail |> Maybe.withDefault [] |> recurse civTechs |> Tuple.first)
                        else
                            update
                                NoOp
                                (techs |> List.tail |> Maybe.withDefault [] |> recurse civTechs |> Tuple.first)
                else
                    (model, Cmd.none)
            in
            case panelPosition of
                LeftPanel ->
                    let civ_id = (String.toInt model.leftPanel.selectedCivID |> Result.toMaybe |> Maybe.withDefault 0)
                    in
                    recurse model.leftPanel.civTechs (allTechs ++ getUniqueTechs model civ_id)
                RightPanel ->
                    let civ_id = (String.toInt model.rightPanel.selectedCivID |> Result.toMaybe |> Maybe.withDefault 0)
                    in
                    recurse model.rightPanel.civTechs (allTechs ++ getUniqueTechs model civ_id)

        DisableAllTechs panelPosition ->
            -- Disable Feudal age and Loom (all the rest depend on Feudal)
            case panelPosition of
                LeftPanel ->
                    update (DisableTech LeftPanel "101") model
                    |> Tuple.first
                    |> update (DisableTech LeftPanel "22")
                RightPanel ->
                    update (DisableTech RightPanel "101") model
                    |> Tuple.first
                    |> update (DisableTech RightPanel "22")

        DisableTech panelPosition techID ->
            case model.dat of
                Just dat ->
                    case (Dict.get techID dat.techs) of
                        Just tech ->
                            case panelPosition of
                                LeftPanel ->
                                    let leftPanel = model.leftPanel
                                        newLeftPanel = { leftPanel | activeTechs =
                                            tech.required_by_techs
                                            |> List.map toString
                                            |> Set.fromList
                                            |> Set.insert techID
                                            |> Set.diff model.leftPanel.activeTechs
                                        }
                                    in
                                    ({ model | leftPanel = newLeftPanel }, Cmd.none)
                                RightPanel ->
                                    let rightPanel = model.rightPanel
                                        newRightPanel = { rightPanel | activeTechs =
                                            tech.required_by_techs
                                            |> List.map toString
                                            |> Set.fromList
                                            |> Set.insert techID
                                            |> Set.diff model.rightPanel.activeTechs
                                        }
                                    in
                                    ({ model | rightPanel = newRightPanel }, Cmd.none)

                        Nothing ->
                            (model, Cmd.none)
                Nothing ->
                    (model, Cmd.none)

        Refresh ->
            -- call SelectCiv for the two panels in succession
            let leftUpdate = update (SelectCiv LeftPanel model.leftPanel.selectedCivID) model
            in
                update (SelectCiv RightPanel model.rightPanel.selectedCivID) (Tuple.first leftUpdate)

        NewDat (Ok newDat) ->
            let newModel = { model | dat = Just newDat, firstRun = False}
            in
            if model.firstRun then
                -- Chain these 4 actions together and return the initialized model on the first run
                let pickLeftCivModel = Tuple.first (update (SelectCiv LeftPanel "15") newModel)
                    pickLeftUnitModel = Tuple.first (update (SelectUnit LeftPanel "4") pickLeftCivModel)
                    pickRightCivModel = Tuple.first (update (SelectCiv RightPanel "15") pickLeftUnitModel)
                    pickRightUnitModel = Tuple.first (update (SelectUnit RightPanel "4") pickRightCivModel)
                in
                update Refresh pickRightUnitModel
            else
                update Refresh newModel

        NewDat (Err _) ->
            (model, Cmd.none)


applyCivBonuses: Model -> String -> Set String -> Set String
applyCivBonuses model civID activeTechs =
    case model.dat of
        Just dat ->
            -- Remove any existing civ bonuses (but not unique techs!)
            let activeTechsClean = dat.techs
                |> Dict.filter (\k v -> Set.member k activeTechs && (v.civ == -1 || v.research_location /= -1))
                |> Dict.keys
                |> Set.fromList
            in
            case (Dict.get civID dat.civs) of
                Just civ ->
                    List.map toString civ.civ_bonuses
                    |> List.filter (\id ->
                        List.all (\i -> Set.member i activeTechs) (
                            -- getRequiredTechs includes the tech itself, which we don't want in this case
                            getRequiredTechs dat.techs id |> List.reverse |> List.tail |> Maybe.withDefault []
                        )
                    )
                    |> Set.fromList
                    |> Set.union activeTechsClean
                Nothing ->
                    activeTechsClean
        Nothing ->
            activeTechs



getRequiredTechs : Dict.Dict String Tech -> String -> List String
getRequiredTechs techDict techID =
    case (Dict.get techID techDict) of
        Just tech ->
            tech.required_techs
            |> List.map toString
            |> List.map (getRequiredTechs techDict)
            |> List.foldr List.append [techID]
        Nothing ->
            []


getActiveEffectCommandsForUnit : Model -> PanelPosition -> Maybe Unit -> List EffectCommand
getActiveEffectCommandsForUnit model panelPosition unit =
    let panel = case panelPosition of
        LeftPanel -> model.leftPanel
        RightPanel -> model.rightPanel
    in
    case model.dat of
        Just dat ->
            case unit of
                Just unit ->
                    case panel.selectedCiv of
                        Just civ ->
                            let effectIDs = dat.techs
                                |> Dict.filter (\k v -> Set.member k panel.activeTechs)
                                |> Dict.values
                                |> List.map (\t -> toString t.effect_id)
                            in
                                dat.effects
                                |> Dict.filter (\k v -> List.member k effectIDs
                                                        || k == (toString civ.team_bonus_effect_id)
                                                        || k == (toString civ.tech_tree_id))
                                |> Dict.values
                                |> List.map (\e -> e.commands)
                                |> List.foldr (++) []
                                |> List.filter (\c -> c.unit_id == unit.id || c.class_id == unit.class_id)
                        Nothing ->
                            []
                Nothing ->
                    []
        Nothing ->
            []

fetchDat : String -> Cmd Msg
fetchDat url =
    Http.send NewDat (Http.get url datDecoder)
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW
civOption: String -> (String, Civ) -> Html Msg
civOption selectedID (civID, civ) =
    if selectedID == civID then
        option [value civID, selected True] [text civ.name]
    else
        option [value civID] [text civ.name]

civSelector : Model -> PanelPosition -> Html Msg
civSelector model panelPosition =
    let panel = case panelPosition of
        LeftPanel -> model.leftPanel
        RightPanel -> model.rightPanel
    in
    model.dat |> viewJust
        (\dat -> select [class "form-control col-md-6", onChange (SelectCiv panelPosition)] (
                let civList = -- Sort by civ name
                    dat.civs
                    |> Dict.toList
                    |> List.sortWith
                        (\a b -> compare (Tuple.second a).name (Tuple.second b).name)
                in
                [ option [ value "", selected (panel.selectedCivID == "") ] [ text "--"] ]
                ++
                (civList |> List.map (civOption panel.selectedCivID))
            )
        )

unitOption: String -> (String, Unit) -> Html Msg
unitOption selectedID (unitID, unit) =
    if unitID == selectedID then
        option [value unitID, selected True] [text unit.name]
    else
        option [value unitID] [text unit.name]

unitSelector : Model -> PanelPosition -> Html Msg
unitSelector model panelPosition =
    let panel = case panelPosition of
        LeftPanel -> model.leftPanel
        RightPanel -> model.rightPanel
    in
    model.dat |> viewJust
        (\dat ->
            panel.selectedCiv |> viewJust
                (\civ ->
                    let unitList = -- Sort by unit name
                        filterCivUnits civ dat
                        |> List.sortWith
                            (\a b -> compare (Tuple.second a).name (Tuple.second b).name)
                    in
                    select [class "form-control col-md-6", onChange (SelectUnit panelPosition)] (
                    [ option [ value "", selected (panel.selectedUnitID == "") ] [ text "--"] ]
                    ++
                    (unitList |> List.map (unitOption panel.selectedUnitID))
                    )
            ) 
        )

filterCivUnits: Civ -> Dat -> List (String, Unit)
filterCivUnits civ dat =
    dat.units
    |> Dict.filter (
        \uidStr unit ->
        let
            uid = String.toInt uidStr |> Result.toMaybe |> Maybe.withDefault -1
        in 
            List.member uid civ.unit_ids
        )
    |> Dict.toList

techOption: Model -> PanelPosition -> Tech -> Html Msg
techOption model panelPosition tech =
    let
        techID = toString tech.id
        panel = case panelPosition of
            LeftPanel -> model.leftPanel
            RightPanel -> model.rightPanel
    in
    div [ onClick (ToggleTech panelPosition techID)
        , title tech.name
        , style (
            if Set.member techID panel.activeTechs then
            [
                ("outline", "4px solid lime")
                , ("cursor", "pointer")
                , ("height", "32px")
                , ("width", "32px")
                , ("float", "left")
                , ("background-image", "url(" ++ "static/data/icons/techs/" ++ (toString tech.icon_id) ++ "_50729.png)" )
                , ("margin", "8px")
            ]
            else if Set.member techID panel.civTechs then
            [
                ("outline", "4px solid lightgray")
                , ("cursor", "pointer")
                , ("height", "32px")
                , ("width", "32px")
                , ("float", "left")
                , ("background-image", "url(" ++ "static/data/icons/techs/" ++ (toString tech.icon_id) ++ "_50729.png)" )
                , ("margin", "8px")
            ]
            else
            [
                ("outline", "4px solid red")
                , ("cursor", "pointer")
                , ("height", "32px")
                , ("width", "32px")
                , ("float", "left")
                , ("background-image", "url(" ++ "static/data/icons/techs/" ++ (toString tech.icon_id) ++ "_50729.png)" )
                , ("background-color", "red")
                , ("opacity", "0.5")
                , ("margin", "8px")
            ]
        )
    ] []


techSelector: Model -> PanelPosition -> Html Msg
techSelector model panelPosition =
    model.dat |> viewJust
        (\dat ->
            let panel = case panelPosition of
                LeftPanel -> model.leftPanel
                RightPanel -> model.rightPanel
            in
            panel.selectedCiv |> viewJust
                (\civ ->
                    div [] [
                          fieldset [] (List.map (techOption model panelPosition) <| techsFromIDs ages dat)
                        , fieldset [] (List.map (techOption model panelPosition) <| techsFromIDs loom dat)
                        , fieldset [] (List.map (techOption model panelPosition) <| techsFromIDs blacksmith1 dat)
                        , fieldset [] (List.map (techOption model panelPosition) <| techsFromIDs blacksmith2 dat)
                        , fieldset [] (List.map (techOption model panelPosition) <| techsFromIDs blacksmith3 dat)
                        , fieldset [] (List.map (techOption model panelPosition) <| techsFromIDs blacksmith4 dat)
                        , fieldset [] (List.map (techOption model panelPosition) <| techsFromIDs blacksmith5 dat)
                        , fieldset [] (List.map (techOption model panelPosition) <| techsFromIDs barracks dat)
                        , fieldset [] (List.map (techOption model panelPosition) <| techsFromIDs archeryRange dat)
                        , fieldset [] (List.map (techOption model panelPosition) <| techsFromIDs stable dat)
                        , fieldset [] (List.map (techOption model panelPosition) <| techsFromIDs dock dat)
                        , fieldset [] (List.map (techOption model panelPosition) <| techsFromIDs monastery dat)
                        , fieldset [] (List.map (techOption model panelPosition) <| techsFromIDs university dat)
                        , fieldset [] (List.map (techOption model panelPosition) <| techsFromIDs (getUniqueTechs model civ.id) dat)
                    ]

            ) 
        )

techsFromIDs: List Int -> Dat -> List Tech
techsFromIDs techIDs dat =
    let
        stringIDs = List.map toString techIDs
    in 
    stringIDs
    |> List.filterMap (\techID -> Dict.get techID dat.techs)

--    dat.techs
--    |> Dict.filter (
--        \techStr unit ->
--        let
--            uid = String.toInt techStr |> Result.toMaybe |> Maybe.withDefault -1
--        in 
--            List.member uid set
--        )
--    |> Dict.toList

projectileForUnit: Model -> Unit -> Maybe Unit
projectileForUnit model unit =
    case model.dat of
        Just dat ->
            Dict.get (toString unit.projectile_id) dat.units
        Nothing ->
            Nothing


view : Model -> Html Msg
view model =
    div [] [
        select
            [class "form-control", onChange SelectDat]
            [
                option [value "static/data/data.aoc.json"] [text "AoC"]
                , option [value "static/data/data.cysion.json"] [text "FE/Expansions"]
            ]
        , div [ id "panels", class "container-fluid", style [ ( "margin-top", "30px" )] ]
            [
                div [class "row"] [
                    div [class "col-md-5"] [
                        div [class "row"] [
                            div [class "col-md-12 row"] [
                                civSelector model LeftPanel
                                , unitSelector model LeftPanel
                            ]
                            , case model.leftPanel.selectedCiv of
                                Just selectedCiv ->
                                    div [class "col-md-5 col-lg-3"] [
                                        div [class "techs"] [ techSelector model LeftPanel ]
                                        ,div [class "btn-group"] [
                                            button [class "btn btn-sm btn-primary", href "#", onClick (EnableAllTechs LeftPanel)] [text "all"]
                                            ,button [class "btn btn-sm btn-danger", href "#", onClick (DisableAllTechs LeftPanel)] [text "none"]
                                        ]
                                    ]
                                Nothing ->
                                    div [] []

                            , div [class "col-md-7 col-lg-9"] [
                                case model.leftPanel.selectedUnit of
                                    Just selectedUnit ->
                                        let projectile = projectileForUnit model selectedUnit
                                        in
                                        unitView UnitLayoutLeft
                                            selectedUnit (getActiveEffectCommandsForUnit model LeftPanel (Maybe.Just selectedUnit))
                                            projectile (getActiveEffectCommandsForUnit model LeftPanel projectile)
                                    Nothing ->
                                        div [] []
                            ]
                        ]
                    ]
                    -- Show the arena table only when both units are selected
                    , div [class "col-md-2"] [
                        case model.leftPanel.selectedUnit of
                            Just selectedUnit1 ->
                                case model.rightPanel.selectedUnit of
                                    Just selectedUnit2 ->
                                        let projectile1 = projectileForUnit model selectedUnit1
                                            projectile2 = projectileForUnit model selectedUnit2
                                        in
                                        arenaView
                                            selectedUnit1 (getActiveEffectCommandsForUnit model LeftPanel (Maybe.Just selectedUnit1))
                                            projectile1 (getActiveEffectCommandsForUnit model LeftPanel projectile1)
                                            selectedUnit2 (getActiveEffectCommandsForUnit model RightPanel (Maybe.Just selectedUnit2))
                                            projectile2 (getActiveEffectCommandsForUnit model LeftPanel projectile2)
                                    Nothing ->
                                        div [] []
                            Nothing ->
                                div [] []

                    ]
                    , div [class "col-md-5"] [
                        div [class "row"] [
                            div [class "col-md-12 row"] [
                                civSelector model RightPanel
                                , unitSelector model RightPanel
                            ]

                            , div [class "col-md-7 col-lg-9"] [
                                case model.rightPanel.selectedUnit of
                                    Just selectedUnit ->
                                        let projectile = projectileForUnit model selectedUnit
                                        in
                                        unitView UnitLayoutRight
                                            selectedUnit (getActiveEffectCommandsForUnit model RightPanel (Maybe.Just selectedUnit))
                                            projectile (getActiveEffectCommandsForUnit model RightPanel projectile)
                                    Nothing ->
                                        div [] []
                            ]
                            , case model.rightPanel.selectedCiv of
                                Just selectedCiv ->
                                    div [class "col-md-5 col-lg-3"] [
                                        div [class "techs"] [ techSelector model RightPanel ]
                                        ,div [class "btn-group"] [
                                            button [class "btn btn-sm btn-primary", href "#", onClick (EnableAllTechs RightPanel)] [text "all"]
                                            ,button [class "btn btn-sm btn-danger", href "#", onClick (DisableAllTechs RightPanel)] [text "none"]
                                        ]
                                    ]
                                Nothing ->
                                    div [] []
                        ]
                    ]
                ]
            ]
        ]


-- CSS STYLES


-- APP

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }