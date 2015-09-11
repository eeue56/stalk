module Main where

import Dict
import String
import Debug exposing (log)

import Model exposing (..)
import Patches exposing (..)
import Views exposing (..)
import Parser exposing (..)



commands : CommandLibrary
commands = Dict.fromList
  [("clear", always Clear),
   ("still", always Still),
   ("set-pcolor", SetPcolor),
   ("set-pcolor-of", SetPcolorOf),
   ("failed", always Failed),
   ("log-patch", LogPatch)
  ]

runCommand : Command -> Model -> Model
runCommand command model =
  case command of
    SetPcolor color -> setPcolor color model
    SetPcolorOf args -> setPcolorOf args model
    LogPatch coors -> logPatch coors model
    Clear -> clearPatches model
    Still -> model
    Failed -> log "Failed" model

update : Action -> Model -> Model
update action model = 
  case action of 
    UpdateText x -> { model | enteredText <- x } 
    Enter -> 
      let
        commands = List.map (\x -> findCommand x model.commands) <| String.lines model.enteredText 
      in
        List.foldl (runCommand) model commands
    Reset -> runCommand Clear model
    Noop -> model

model' : Model
model' = {
  enteredText = "",
  patches = defaultPatches 25 25,
  commands = commands,
  width = 750,
  height = 750,
  patchSize =  750 / 25 }

enteredCommands : Signal.Mailbox Action
enteredCommands = Signal.mailbox Noop

model = Signal.foldp
  update
  model'
  enteredCommands.signal



main = Signal.map (view enteredCommands.address) model