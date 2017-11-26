module Utils exposing (..)
import Html exposing (..)
import Html.Events exposing (on, onInput)
import Json.Decode as Json

onChange : (String -> msg) -> Attribute msg
onChange handler = 
  on "change" <| Json.map handler <| Json.at ["target", "value"] Json.string

