module Views where

import Html exposing (Html, Attribute, div, textarea, button, fromElement, text)
import Html.Attributes exposing (placeholder, value)

import Html.Events exposing (keyCode, on, targetValue, onClick)

import Json.Decode as Json

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

commandsTextarea : Signal.Address Command -> Html
commandsTextarea address = 
  div [] 
    [
      textarea 
        [ placeholder "Enter a command",
          on "input" targetValue (Signal.message address << UpdateText)
          ] [],
      button [ onClick address Enter ] [ text "Run program" ],
      button [ onClick address Clear ] [ text "Reset" ]
    ]
view address model =
  div [] [
    commandsTextarea address,
    fromElement <| drawWorld model
  ]