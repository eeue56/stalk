module Main where

import Matrix exposing (Matrix)
import Color exposing (Color, black, red, green, blue, rgb)
import Html exposing (Html, Attribute, div, textarea, button, fromElement, text)
import Html.Attributes exposing (placeholder, value)

import Html.Events exposing (keyCode, on, targetValue, onClick)
import Json.Decode as Json
import Dict
import String
import Debug exposing (log)

import Model exposing (..)
import Drawing exposing (..)


defaultPatch = { pcolor = black }
redPatch = { pcolor = red }
colorPatch color = { pcolor = color } 

commands : Dict.Dict String (Argument -> Command)
commands = Dict.fromList
  [("clear", always Clear),
   ("still", always Still),
   ("set-pcolor", SetPcolor),
   ("failed", always Failed)
  ]

clearPatches : Model -> Model 
clearPatches model =
  let
    (width, height) = model.patches.size
  in
    { model | patches <- Matrix.repeat width height defaultPatch }

findCommand : String -> Dict.Dict String (Argument -> Command) -> Command
findCommand text dict =
  let
    trimText = String.trim text
    hasArgs = String.contains "$" trimText
    args = if not hasArgs then [] else
      case List.tail <| String.split "$" trimText of 
        Just v -> case List.head v of 
          Just x -> String.split "," x
          Nothing -> []
        Nothing -> []
  in
    if not hasArgs 
      then 
        case Dict.get trimText dict of
          Just v -> v []
          Nothing -> Failed
      else
        case List.head <| String.split "$" trimText of
          Just v -> case Dict.get (String.trim v) dict of
            Just command -> command <| List.map (String.trim) args 
            Nothing -> Failed
          Nothing -> Failed

-- TODO: add to elm-simple-data
rgbFromList vals = 
  let
    r = log "val" <| alwaysOk <| case List.head vals of Just v -> v
    g = log "val" <| alwaysOk <| (\x -> case List.head x of Just v -> v) 
      <| (case List.tail vals of Just v -> v)
    b = log "val" <| alwaysOk <| case List.head <| List.reverse vals of Just v -> v
    alwaysOk v = case String.toInt v of 
      Ok x -> x
      Err _ -> log "Incorrect convert" 0
  in
    rgb r g b

setPcolor : Argument -> Model -> Model
setPcolor color model =
  if List.length color == 1 then
    let
      myHead = case List.head color of 
        Just v -> String.trim v
        Nothing -> "black"
      newColor = if myHead == "red" then red else black
      (width, height) = model.patches.size
    in
      { model | patches <- Matrix.repeat width height <| colorPatch newColor }
  else
    let
      (width, height) = model.patches.size 
    in
      { model | patches <- Matrix.repeat width height <| colorPatch <| rgbFromList color }


update : Command -> Model -> Model
update command model =
  case command of
    Clear -> clearPatches model
    UpdateText x -> { model | enteredText <- x } 
    SetPcolor color -> 
      setPcolor color model
    Enter -> 
      let
        command = findCommand model.enteredText model.commands
      in
        update command model
    Still -> model
    Failed ->
      let
        (width, height) = model.patches.size
      in
        { model | patches <- Matrix.repeat width height <| colorPatch green }


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
      button [ onClick address Enter ] [ text "Run program" ],
      button [ onClick address Clear ] [ text "Reset" ]
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