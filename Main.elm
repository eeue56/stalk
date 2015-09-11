module Main where

import Dict
import String
import Debug exposing (log)

import Model exposing (..)
import Patches exposing (..)
import Views exposing (..)



commands : CommandLibrary
commands = Dict.fromList
  [("clear", always Clear),
   ("still", always Still),
   ("set-pcolor", SetPcolor),
   ("set-pcolor-of", SetPcolorOf),
   ("failed", always Failed),
   ("log-patch", LogPatch)
  ]


findCommand : String -> CommandLibrary -> Command
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



update : Command -> Model -> Model
update command model =
  case command of
    Clear -> clearPatches model
    UpdateText x -> { model | enteredText <- x } 
    SetPcolor color -> setPcolor color model
    SetPcolorOf args -> setPcolorOf args model
    LogPatch coors -> logPatch coors model
    Enter -> 
      let
        commands = List.map (\x -> findCommand x model.commands) <| String.lines model.enteredText 
      in
        List.foldl (update) model commands
    Still -> model
    Failed -> log "Failed" model




model' : Model
model' = {
  enteredText = "",
  patches = defaultPatches 25 25,
  commands = commands,
  width = 750,
  height = 750,
  patchSize =  750 / 25 }

enteredCommands : Signal.Mailbox Command
enteredCommands = Signal.mailbox Still

model = Signal.foldp
  update
  model'
  enteredCommands.signal



main = Signal.map (view enteredCommands.address) model