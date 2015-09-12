module Views where

import Html exposing (Html, Attribute, div, textarea, button, fromElement, text)
import Html.Attributes exposing (placeholder, value, hidden)

import Html.Events exposing (keyCode, on, targetValue, onClick)

import Json.Decode as Json
import String
import Model exposing (..)
import Drawing exposing (..)

drawWorld model =
  draw model

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err ""

onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)

commandsTextarea : Signal.Address Action -> Model -> Html
commandsTextarea address model = 
  div [] 
    [
      textarea 
        [ placeholder "Enter a command",
          on "input" targetValue (Signal.message address << UpdateText)
          ] [],
      button [ onClick address Enter ] [ text "Run program" ],
      button [ onClick address Reset ] [ text "Reset" ],
      textarea [ value <| String.trim model.errorMessage, hidden <| String.trim model.errorMessage == "" ] [ ]
    ]
    
view address model =
  div [] [
    commandsTextarea address model,
    fromElement <| drawWorld model,
    text <| toString model.stack
  ]