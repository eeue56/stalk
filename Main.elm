module Main where

import Matrix exposing (Matrix)
import Color exposing (Color, black, red)
import Html exposing (Html, Attribute, div, textarea, button, fromElement)
import Html.Attributes exposing (placeholder, value)

import Html.Events exposing (keyCode, on, targetValue, onClick)
import Json.Decode as Json

import Model exposing (..)
import Drawing exposing (..)


defaultPatch = { pcolor = black }
redPatch = { pcolor = red }

commands = 
  [("clear", Clear),
   ("still", Still)
  ]

clearPatches : Model -> Model 
clearPatches model =
  let
    (width, height) = model.patches.size
  in
    { model | patches <- Matrix.repeat width height defaultPatch }

update : Command -> Model -> Model
update command model =
  case command of
    Clear -> clearPatches model
    UpdateText x -> { model | enteredText <- x } 
    Enter -> 
      let
        (width, height) = model.patches.size
      in
        { model | patches <- Matrix.repeat width height redPatch }
    Still -> model

drawWorld model =
  draw model

model' : Model
model' = {
  enteredText = "",
  patches = Matrix.repeat 5 5 (defaultPatch),
  commands = commands,
  width = 500,
  height = 500,
  patchSize = 500 / 5 }

enteredCommands : Signal.Mailbox Command
enteredCommands = Signal.mailbox Still


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
      button [ onClick address Enter ] []
    ]

model = Signal.foldp
  update
  model' 
  enteredCommands.signal


view address model =
  div [] [
    commandsTextarea address,
    fromElement <| drawWorld model
  ]

main = Signal.map (view enteredCommands.address) model