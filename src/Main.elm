module Main where

import Dict
import String
import Debug exposing (log)

import Model exposing (..)
import Patches exposing (..)
import Stack exposing (..)
import Maths exposing (..)

import Views exposing (..)
import Parser exposing (..)



commands : CommandLibrary
commands = Dict.fromList
  [("clear", always Clear),
   ("still", always Still),
   ("failed", always Failed),
   ("error", CompileError),

   ("empty-stack", always EmptyStack),
   ("push", PushToStack),
   ("pop", PopOffStack),
   ("repeat", RepeatTopOfStack),
   ("swap", always SwapTopOfStack),

   ("set-pcolor", SetPcolor),
   ("set-pcolor-of", SetPcolorOf),

   ("pcolor-of", PcolorOf),
   ("pxcor-of", PxcorOf),
   ("pycor-of", PycorOf),
   ("pxycor-of", PxycorOf),

   ("log-patch", LogPatch),

   ("add", Add),
   ("+", Add),
   ("subtract", Subtract),
   ("-", Subtract),
   ("multiply", Multiply),
   ("*", Multiply),
   ("divide", Divide),
   ("/", Divide)

  ]

runCommand : (Command, Int) -> Model -> Model
runCommand (command, stackUses) model' =
  let
    model = { model' | stack <- List.drop stackUses model'.stack }
  in
    case command of
      SetPcolor color -> setPcolor color model
      SetPcolorOf args -> setPcolorOf args model

      PcolorOf args -> pcolorOf args model
      PxcorOf args -> pxcorOf args model
      PycorOf args -> pycorOf args model
      PxycorOf args -> pxycorOf args model

      LogPatch coors -> logPatch coors model

      EmptyStack -> emptyStack model
      PushToStack args -> pushToStack args model
      PopOffStack args -> popOffStack args model
      RepeatTopOfStack args -> repeatTopOfStack args model
      SwapTopOfStack -> Stack.swap model

      Add args -> add args model
      Subtract args -> subtract args model
      Multiply args -> multiply args model
      Divide args -> divide args model

      CompileError messages -> compileError messages model
      Clear -> clearPatches model |> emptyStack
      Still -> model
      Failed -> model
      _ -> log "Not found" model

update : Action -> Model -> Model
update action model = 
  case action of 
    UpdateText x -> { model | enteredText <- x } 
    Enter -> 
      let
        model' = { model | errorMessage <- "" }
        commands = List.filter (not << String.isEmpty) <| String.lines model'.enteredText 
      in
        List.foldl (\command model'' -> runCommand (parse command model'') model'') model' commands
    Reset -> runCommand (Clear, 0) model
    Noop -> model

model' : Model
model' = {
  enteredText = "",
  errorMessage = "",
  patches = defaultPatches 25 25,
  commands = commands,
  width = 750,
  height = 750,
  stack = [],
  patchSize =  750 / 25 }

enteredCommands : Signal.Mailbox Action
enteredCommands = Signal.mailbox Noop

model = Signal.foldp
  update
  model'
  enteredCommands.signal



main = Signal.map (view enteredCommands.address) model