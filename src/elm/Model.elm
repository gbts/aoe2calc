module Model exposing (..)
import Set exposing (..)
import Components.Data.Dat exposing (..)


type PanelPosition = LeftPanel | RightPanel

type alias PanelModel = {
    selectedCiv: Maybe Civ,
    selectedCivID: String,
    selectedUnit: Maybe Unit,
    selectedUnitID: String,
    activeTechs: Set String,
    civTechs: Set String
}

type alias Model = {
    dat: Maybe Dat,
    firstRun: Bool,
    leftPanel: PanelModel,
    rightPanel: PanelModel
}

